open Core_kernel
open Monads.Std

module Order = struct
  type partial = LT | EQ | GT | NC
end

module Id = Int63

let user_package = "user-knowledge"
let package = "knowledge"

module Domain = struct
  type 'a t = {
    inspect : 'a -> Sexp.t;
    empty : 'a;
    order : 'a -> 'a -> Order.partial;
    name : string;
  }

  let define ?(inspect=sexp_of_opaque) ~empty ~order name = {
    inspect; empty; order; name;
  }

  let partial_of_total order x y : Order.partial = match order x y with
    | 0 -> EQ
    | 1 -> GT
    | _ -> LT

  let total ?inspect ~empty ~order name =
    define ?inspect ~empty name ~order:(partial_of_total order)

  let mapping (type k) (type o) ?(equal=(fun _ _ -> true))
      (module K : Base.Comparable.S with type t = k
                                     and type comparator_witness = o)
      name =
    let empty = Map.empty (module K) in
    let inspect xs =
      Sexp.List (Map.keys xs |> List.map ~f:K.comparator.sexp_of_t) in
    let order x y =
      Map.symmetric_diff x y ~data_equal:equal |>
      Sequence.fold ~init:(0,0,0) ~f:(fun (l,m,r) -> function
          | (_,`Left _)     -> (l+1,m,r)
          | (_,`Right _)    -> (l,m,r+1)
          | (_, `Unequal _) -> (l,m+1,r)) |> function
      | 0,0,0 -> Order.EQ
      | 0,0,_ -> LT
      | _,0,0 -> GT
      | _,_,_ -> NC in
    define ~inspect ~empty ~order name

  let optional ?inspect ~order name =
    let inspect = match inspect with
      | None -> sexp_of_opaque
      | Some sexp_of_elt -> sexp_of_option sexp_of_elt in
    let order x y : Order.partial = match x, y with
      | None,None -> EQ
      | None,Some _ -> LT
      | Some _,None -> GT
      | Some x, Some y -> order x y in
    let empty = None in
    define ~inspect ~order ~empty name

  let string = define "string" ~empty:""
      ~inspect:sexp_of_string ~order:(fun x y ->
          match String.is_empty x, String.is_empty y with
          | true, true -> EQ
          | true,false -> GT
          | false,true -> LT
          | false,false -> partial_of_total String.compare x y)
end

type 'a obj = Id.t
type 'a ord = Id.comparator_witness

type fullname = {
  package : string;
  name : string;
}

module Registry = struct
  let packages = Hash_set.create (module String) ()
  let classes = Hashtbl.create (module String)
  let slots = Hashtbl.create (module String)

  let add_package name = Hash_set.add packages name

  let is_present ~package namespace name =
    match Hashtbl.find namespace package with
    | None -> false
    | Some names -> Map.mem names name

  let register kind namespace ?desc ?(package=user_package) name =
    add_package package;
    if is_present ~package namespace name
    then failwithf
        "Failed to declare new %s, there is already a %s \
         named `%s' in package `%s'" kind kind name package ();
    Hashtbl.update namespace package ~f:(function
        | None -> Map.singleton (module String) name desc
        | Some names -> Map.add_exn names ~key:name ~data:desc);
    {package; name}

  let add_class = register "class" classes
  let add_slot  = register "property" slots
end

let string_of_fname {package; name} =
  package ^ ":" ^ name

module Class = struct
  type top = TOP

  type _ data =
    | Top : top data
    | Derive : 'a data * 'b -> ('b -> 'a) data


  type 'a t = {
    id : Id.t;
    name : fullname;
    data : 'a data;
  }

  let classes = ref Id.zero

  let newclass ?desc ?package name data =
    Id.incr classes;
    {
      id = !classes;
      name = Registry.add_class ?desc ?package name;
      data;
    }

  let declare
    : ?desc:string -> ?package:string -> string -> 'a -> ('a -> top) t =
    fun ?desc ?package name data ->
      newclass ?desc ?package name (Derive (Top,data))

  let derived
    : ?desc:string -> ?package:string -> string -> 'a t -> 'b -> ('b -> 'a) t =
    fun ?desc ?package name parent data ->
      newclass ?desc ?package name (Derive (parent.data,data))

  let upcast : type a b. (b -> a) t -> a t = fun child -> match child with
    | {data=Derive (base,_); id; name} -> {id; name; data=base}

  let refine : type a b. a t -> b -> (b -> a) t =
    fun {id; name; data} data' -> {id; name; data=Derive (data,data')}

  let same x y = Id.equal x.id y.id

  let equal : type a b. a t -> b t -> (a obj,b obj) Type_equal.t option =
    fun x y -> Option.some_if (same x y) Type_equal.T

  let data : type a b. (b -> a) t -> b = fun {data=Derive (_,data)} -> data

  let name {name={name}} = name
  let package {name={package}} = package
  let fullname {name} = string_of_fname name

  let comparator : type a. a t ->
    (module Comparator.S
      with type t = a obj
       and type comparator_witness = a ord) = fun _ ->
    let module R = struct
      type t = a obj
      type comparator_witness = Id.comparator_witness
      let comparator = Id.comparator
    end in
    (module R)
end

module Record = struct
  module Dict = Univ_map
  type  t = Dict.t
  module Repr = struct
    type entry = {
      name : string;
      data : string;
    } [@@deriving bin_io]

    type t = entry list [@@deriving bin_io]
  end

  type slot_io = {
    reader : string -> Dict.Packed.t;
    writer : Dict.Packed.t -> string;
  }

  let empty = Dict.empty

  let put key v x = Dict.set v key x
  let get key {Domain.empty} data = match Dict.find data key with
    | None -> empty
    | Some x -> x

  let io : slot_io Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let register_parser (type p)
      (key : p Dict.Key.t)
      (module P : Binable.S with type t = p) =
    let slot = Dict.Key.name key in
    let writer (Dict.Packed.T (k,x)) =
      match Type_equal.Id.same_witness k key with
      | Some Type_equal.T -> Binable.to_string (module P) x
      | None -> assert false in
    let reader s =
      Dict.Packed.T (key,Binable.of_string (module P) s) in
    Hashtbl.add_exn io ~key:slot ~data:{reader;writer}

  include Binable.Of_binable(Repr)(struct
      type t = Dict.t
      let to_binable s =
        Dict.to_alist s |>
        List.rev_filter_map ~f:(fun (Dict.Packed.T(k,_) as x) ->
            let name = Dict.Key.name k in
            match Hashtbl.find io name with
            | None -> None
            | Some {writer=to_string} ->
              Some Repr.{name; data = to_string x;})

      let of_binable entries =
        List.fold entries ~init:Dict.empty ~f:(fun s {Repr.name; data} ->
            match Hashtbl.find io name with
            | None -> s
            | Some {reader=parse} ->
              let Dict.Packed.T (key,data) = parse data in
              Dict.set s key data)
    end)
end

module Knowledge = struct
  type 'a value = {
    cls  : 'a Class.t;
    data : Record.t
  }
  type 'a cls = 'a Class.t
  type 'a obj = Id.t
  type 'p domain = 'p Domain.t

  module Pid = Int63
  type pid = Pid.t

  type conflict = ..

  module Conflict = struct
    type t = conflict = ..
  end

  module Base = struct
    type objects = {
      data : Record.t Map.M(Id).t;
      reqs : Set.M(Id).t Map.M(Pid).t
    }
    let empty_class = {
      data = Map.empty (module Id);
      reqs = Map.empty (module Pid);
    }

    type t = objects Map.M(Id).t
  end

  type state = Base.t
  let empty : Base.t = Map.empty (module Id)

  module State = struct
    include Monad.State.T1(Base)(Monad.Ident)
    include Monad.State.Make(Base)(Monad.Ident)
  end

  module Knowledge = struct
    type 'a t = ('a,conflict) Result.t State.t
    include Monad.Result.Make(Conflict)(State)
  end

  type 'a knowledge = 'a Knowledge.t

  open Knowledge.Syntax

  module Slot = struct
    type 'p promise = {
      get : Id.t -> 'p Knowledge.t;
      pid : pid;
    }

    type ('a,'p) t = {
      cls : 'a Class.t;
      dom : 'p Domain.t;
      key : 'p Type_equal.Id.t;
      name : string;
      desc : string option;
      mutable promises : 'p promise list;
    }

    let declare ?desc ?serializer ?package cls name (dom : 'a Domain.t) =
      let slot = Registry.add_slot ?desc ?package name in
      let name = string_of_fname slot in
      let key = Type_equal.Id.create ~name dom.inspect in
      Option.iter serializer (Record.register_parser key);
      {cls; dom; key; name; desc; promises = []}
  end

  type ('a,'p) slot = ('a,'p) Slot.t

  module Class = struct
    include Class
    let property = Slot.declare
  end

  module Value = struct
    type 'a t = 'a value
    let empty cls = {cls; data=Record.empty}
    let clone cls {data} = {cls;data}
    let create cls data = {cls; data}
    let put {Slot.key} v x = {
      v with data = Record.put key v.data x
    }
    let get {Slot.key; dom} {data} = Record.get key dom data
  end

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)

  let provide : type a p. (a,p) slot -> a obj -> p -> unit Knowledge.t =
    fun slot obj x ->
      update @@ fun s ->
      let {Base.data; reqs} = match Map.find s slot.cls.id with
        | None -> Base.empty_class
        | Some objs -> objs in
      let data = Map.update data obj ~f:(function
          | None -> Record.(put slot.key empty x)
          | Some v -> Record.put slot.key v x) in
      Map.set s ~key:slot.cls.id ~data:{data; reqs}

  let pids = ref Pid.zero

  let promise (s : _ slot) get =
    Pid.incr pids;
    let pid = !pids in
    s.promises <- {get;pid} :: s.promises

  let is_active reqs pid id = match Map.find reqs pid with
    | None -> false
    | Some ids -> Set.mem ids id

  let activate reqs pid id = Map.update reqs pid ~f:(function
      | None -> Set.singleton (module Pid) id
      | Some ids -> Set.add ids id)

  let deactivate req pid id = Map.change req pid ~f:(function
      | None -> None
      | Some ids ->
        let ids = Set.remove ids id in
        if Set.is_empty ids then None else Some ids)

  let objects : _ cls -> _ = fun cls ->
    get () >>| fun base ->
    match Map.find base cls.id with
    | None -> Base.empty_class
    | Some objs -> objs

  let update_reqs : _ slot -> _ = fun slot reqs ->
    update @@ fun s ->
    Map.update s slot.cls.id ~f:(function
        | None -> {Base.empty_class with reqs}
        | Some objs -> {objs with reqs})

  let collect : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
      objects slot.cls >>= fun {Base.data} ->
      let init = match Map.find data id with
        | None -> slot.dom.empty
        | Some v -> Record.get slot.key slot.dom v in
      slot.promises |>
      Knowledge.List.fold ~init ~f:(fun curr req ->
          objects slot.cls >>= fun {Base.reqs} ->
          if is_active reqs req.pid id
          then Knowledge.return curr
          else
            update_reqs slot (activate reqs req.pid id) >>= fun () ->
            req.get id >>= fun next ->
            update_reqs slot (deactivate reqs req.pid id) >>= fun () ->
            match slot.dom.order curr next with
            | GT -> Knowledge.return curr
            | LT | EQ | NC -> Knowledge.return next) >>= fun max ->
      provide slot id max >>| fun () -> max



  module Object = struct
    type +'a t = 'a obj

    let with_new_object objs f = match Map.max_elt objs.Base.data with
      | None -> f Id.one {
          objs
          with data = Map.singleton (module Id) Id.one Record.empty
        }
      | Some (key,_) ->
        let key = Id.succ key in
        f key {
          objs
          with data = Map.add_exn objs.data ~key ~data:Record.empty
        }


    let create : 'a cls -> 'a obj Knowledge.t = fun cls ->
      objects cls >>= fun objs ->
      with_new_object objs @@ fun obj objs ->
      update (Map.set ~key:cls.id ~data:objs) >>| fun () ->
      obj

    let delete {Class.id} obj =
      update @@ fun base -> Map.change base id ~f:(function
          | None -> None
          | Some objs ->
            let data = Map.remove objs.data obj in
            if Map.is_empty data then None
            else Some {objs with data})

    let scoped cls scope =
      create cls >>= fun obj ->
      scope  obj >>= fun r ->
      delete cls obj >>| fun () ->
      r

    let repr {Class.name={name}} obj =
      Knowledge.return @@
      Format.asprintf "#<%s %a>" name Id.pp obj


    let read _ input =
      Knowledge.return @@
      Scanf.sscanf input "#<%s %s>" @@ fun _ obj ->
      Id.of_string obj

    let cast : type a b. (a obj, b obj) Type_equal.t -> a obj -> b obj =
      fun Type_equal.T x -> x

  end

  include Knowledge

  let get_value cls obj = objects cls >>| fun {Base.data} ->
    match Map.find data obj with
    | None -> Value.empty cls
    | Some x -> Value.create cls x

  let run x cls obj s =
    let x = x >>= fun () -> get_value cls obj in
    match State.run x s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err

end
