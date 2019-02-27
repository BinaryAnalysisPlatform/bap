open Core_kernel
open Monads.Std

module Order = struct
  type partial = LT | EQ | GT | NC
end

type conflict = ..

module Conflict = struct
  type t = conflict = ..
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
      (module K : Comparator.S with type t = k
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
      | Some x, Some y -> partial_of_total order x y in
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

  type 'a data =
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

  let assert_equal x y = match equal x y with
    | Some t -> t
    | None ->
      failwithf "assert_equal: wrong assertion, classes of %s and %s \
                 are different"
        (string_of_fname x.name)
        (string_of_fname y.name)
        ()

  let data : type a b. (b -> a) t -> b = fun {data=Derive (_,data)} -> data

  let name {name={name}} = name
  let package {name={package}} = package
  let fullname {name} = string_of_fname name

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

  type univ = Dict.Packed.t


  type slot_io = {
    reader : string -> univ;
    writer : univ -> string;
  }

  type slot_domain = {
    inspect : univ -> Sexp.t;
    order  : univ -> univ -> Order.partial;
    empty  : univ;
  }

  let io : slot_io Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let domains : slot_domain Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let empty = Dict.empty

  let (<:=) x y =
    Dict.to_alist x |> List.for_all ~f:(fun (Dict.Packed.T (k,_) as x) ->
        match Dict.find y k with
        | None -> false
        | Some y ->
          let dom = Hashtbl.find_exn domains (Dict.Key.name k) in
          match dom.order x (Dict.Packed.T (k,y)) with
          | LT | EQ -> true
          | GT | NC -> false)

  let order : t -> t -> Order.partial = fun x y ->
    match x <:= y, y <:= x with
    | true,false  -> LT
    | true,true   -> EQ
    | false,true  -> GT
    | false,false -> NC


  let put key v x = Dict.set v key x
  let get key {Domain.empty} data = match Dict.find data key with
    | None -> empty
    | Some x -> x

  exception Merge_conflict of string * Sexp.t * Sexp.t

  type conflict += Merge of string * Sexp.t * Sexp.t

  let merge ~on_conflict x y =
    Dict.to_alist x |>
    List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
        Dict.change v k ~f:(function
            | None -> Some x
            | Some y ->
              let name = Dict.Key.name k in
              let pack v = Dict.Packed.T (k,v) in
              let dom = Hashtbl.find_exn domains name in
              match dom.order (pack x) (pack y) with
              | EQ | LT -> Some x
              | GT -> Some y
              | NC -> match on_conflict with
                | `drop_both -> None
                | `drop_left -> Some y
                | `drop_right -> Some x
                | `fail ->
                  let x = dom.inspect (pack x)
                  and y = dom.inspect (pack y) in
                  raise (Merge_conflict (name,x,y))))

  let join x y =
    try Ok (merge ~on_conflict:`fail x y)
    with Merge_conflict (n,x,y) -> Error (Merge (n,x,y))

  let register_persistent (type p)
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

  let register_domain
    : type p. p Dict.Key.t -> p Domain.t -> unit =
    fun key dom ->
      let name = Dict.Key.name key in
      let eq = Type_equal.Id.same_witness_exn in
      let order (Dict.Packed.T (kx,x)) (Dict.Packed.T (ky,y)) =
        let Type_equal.T = eq kx ky in
        let Type_equal.T = eq kx key in
        dom.order x y in
      let empty = Dict.Packed.T(key, dom.empty) in
      let inspect (Dict.Packed.T(kx,x)) =
        let Type_equal.T = eq kx key in
        dom.inspect x in
      Hashtbl.add_exn domains ~key:name ~data:{
        inspect;
        empty;
        order;
      }

  let sexp_of_t x =
    Sexp.List (Dict.to_alist x |> List.map ~f:(function
          Dict.Packed.T (k,x) ->
          let name = Dict.Key.name k in
          let dom = Hashtbl.find_exn domains name in
          Sexp.List [
            Atom name;
            dom.inspect (Dict.Packed.T(k,x));
          ]))

  let t_of_sexp = opaque_of_sexp

  let inspect = sexp_of_t
end

module Knowledge = struct

  type 'a value = {
    cls  : 'a Class.t;
    data : Record.t;
    time : Id.t;
  }
  type 'a cls = 'a Class.t
  type 'a obj = Id.t
  type 'p domain = 'p Domain.t
  type 'a ord = Id.comparator_witness
  type conflict = Conflict.t = ..

  module Pid = Int63
  type pid = Pid.t


  module Data = struct
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

  type state = Data.t
  let empty : Data.t = Map.empty (module Id)

  module State = struct
    include Monad.State.T1(Data)(Monad.Ident)
    include Monad.State.Make(Data)(Monad.Ident)
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

    let declare ?desc ?persistent ?package cls name (dom : 'a Domain.t) =
      let slot = Registry.add_slot ?desc ?package name in
      let name = string_of_fname slot in
      let key = Type_equal.Id.create ~name dom.inspect in
      Option.iter persistent (Record.register_persistent key);
      Record.register_domain key dom;
      {cls; dom; key; name; desc; promises = []}
  end

  type ('a,'p) slot = ('a,'p) Slot.t

  module Value = struct
    type 'a t = 'a value

    (* we could use an extension variant or create a new OCaml object
       instead of incrementing a second, but they are less reliable
       and heavier *)
    let next_second =
      let current = ref Int63.zero in
      fun () -> Int63.incr current; !current

    let empty cls =
      {cls; data=Record.empty; time = next_second ()}

    let clone cls {data; time} = {cls; data; time}
    let cls {cls} = cls
    let create cls data = {cls; data; time = next_second ()}
    let put {Slot.key} v x = {
      v with data = Record.put key v.data x;
             time = next_second ()
    }
    let get {Slot.key; dom} {data} = Record.get key dom data
    let strip
      : type a b. (a value, b value) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T

    type strategy = [`drop_left | `drop_right | `drop_both]

    let merge ?(on_conflict=`drop_old) x y =
      let on_conflict : strategy = match on_conflict with
        | `drop_old -> if Id.(x.time < y.time)
          then `drop_left else `drop_right
        | `drop_new -> if Id.(x.time < y.time)
          then `drop_right else `drop_left
        | #strategy as other -> other in
      {
        x with time = next_second ();
               data = Record.merge ~on_conflict x.data y.data
      }


    let join x y = match Record.join x.data y.data with
      | Ok data -> Ok {x with data; time = next_second ()}
      | Error c -> Error c

    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    module Comparator = Base.Comparator.Make1(struct
        type 'a t = 'a value
        let sexp_of_t = sexp_of_opaque
        let compare x y = match Record.order x.data y.data with
          | LT -> -1
          | EQ -> 0
          | GT -> 1
          | NC -> Int63.compare x.time y.time
      end)

    include Comparator

    type 'a ord = comparator_witness

    let derive
      : type a. a cls ->
        (module S with type t = a t and type comparator_witness = a ord) =
      fun cls ->
        let module R = struct
          type t = a value
          let sexp_of_t x = Record.sexp_of_t x.data
          let t_of_sexp = opaque_of_sexp
          include Binable.Of_binable(Record)(struct
              type t = a value
              let to_binable : 'a value -> Record.t =
                fun {data} -> data
              let of_binable : Record.t -> 'a value =
                fun data -> {cls; data; time = next_second ()}
            end)
          type comparator_witness = Comparator.comparator_witness
          include Base.Comparable.Make_using_comparator(struct
              type t = a value
              let sexp_of_t = sexp_of_t
              include Comparator
            end)
        end in
        (module R)


  end

  module Class = struct
    include Class
    let property = Slot.declare
    let strip
      : type a b. (a t, b t) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T
  end

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)

  let provide : type a p. (a,p) slot -> a obj -> p -> unit Knowledge.t =
    fun slot obj x ->
      update @@ fun s ->
      let {Data.reqs; data} = match Map.find s slot.cls.id with
        | None -> Data.empty_class
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
    | None -> Data.empty_class
    | Some objs -> objs

  let update_reqs : _ slot -> _ = fun slot reqs ->
    update @@ fun s ->
    Map.update s slot.cls.id ~f:(function
        | None -> {Data.empty_class with reqs}
        | Some objs -> {objs with reqs})

  let collect : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
      objects slot.cls >>= fun {Data.data} ->
      let init = match Map.find data id with
        | None -> slot.dom.empty
        | Some v -> Record.get slot.key slot.dom v in
      slot.promises |>
      Knowledge.List.fold ~init ~f:(fun curr req ->
          objects slot.cls >>= fun {Data.reqs} ->
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
    type 'a ord = Id.comparator_witness

    let with_new_object objs f = match Map.max_elt objs.Data.data with
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

    let comparator : type a. a cls ->
      (module Comparator.S
        with type t = a obj
         and type comparator_witness = a ord) = fun _ ->
      let module R = struct
        type t = a obj
        type comparator_witness = a ord
        let comparator = Id.comparator
      end in
      (module R)
  end

  module Domain = Domain
  module Order = Order

  include Knowledge

  let get_value cls obj = objects cls >>| fun {Data.data} ->
    match Map.find data obj with
    | None -> Value.empty cls
    | Some x -> Value.create cls x

  let run x cls obj s =
    let x = x >>= fun () -> get_value cls obj in
    match State.run x s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err

end

type 'a knowledge = 'a Knowledge.t
