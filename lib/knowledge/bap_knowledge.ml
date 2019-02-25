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
end

type 'a obj = Id.t
type 'a ord = Id.comparator_witness

type fname = {
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
  let add_slot  = register "slot"  slots
end

let string_of_fname {package; name} =
  package ^ ":" ^ name

module Class = struct
  type +'a t = {
    id : Id.t;
    name : fname;
  }

  let classes = ref Id.zero

  let newclass ?desc ?package name =
    Id.incr classes;
    {
      id = !classes;
      name = Registry.add_class ?desc ?package name;
    }

  let declare ?desc ?package name =
    newclass ?desc ?package name

  let derived ?desc ?package name _parent =
    newclass ?desc ?package name

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

module Object = struct
  type +'a t = Id.t
end

module Record = struct
  module Dict = Univ_map
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
  include Dict
end

module Knowledge = struct
  type 'a value = Record.t
  type +'a cls = 'a Class.t
  type +'a obj = 'a Object.t
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

  module Value = struct
    type 'a t = 'a value
    let empty _ = Record.empty
    let put {Slot.key} v x = Record.set v key x
    let get {Slot.key; dom} data = match Record.find data key with
      | None -> dom.empty
      | Some x -> x
  end

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)


  let provide : type a p. (a,p) slot -> a Object.t -> p -> unit Knowledge.t =
    fun slot obj x ->
      update @@ fun s ->
      let {Base.data; reqs} = match Map.find s slot.cls.id with
        | None -> Base.empty_class
        | Some objs -> objs in
      let data = Map.update data obj ~f:(function
          | None -> Value.(put slot (empty slot.cls) x)
          | Some v -> Value.put slot v x) in
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

  let objects : _ slot -> _ = fun slot ->
    get () >>| fun base ->
    match Map.find base slot.cls.id with
    | None -> Base.empty_class
    | Some objs -> objs

  let update_reqs : _ slot -> _ = fun slot reqs ->
    update @@ fun s ->
    Map.update s slot.cls.id ~f:(function
        | None -> {Base.empty_class with reqs}
        | Some objs -> {objs with reqs})

  let collect : type a p. (a,p) slot -> a Object.t -> p Knowledge.t =
    fun slot id ->
      objects slot >>= fun {Base.data} ->
      let init = match Map.find data id with
        | None -> slot.dom.empty
        | Some v -> Value.get slot v in
      slot.promises |>
      Knowledge.List.fold ~init ~f:(fun curr req ->
          objects slot >>= fun {Base.reqs} ->
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


  include Knowledge

  let run x s = match State.run x s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err

end
