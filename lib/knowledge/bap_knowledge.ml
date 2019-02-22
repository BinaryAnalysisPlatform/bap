open Core_kernel
open Monads.Std

module Order = struct
  type partial = LT | EQ | GT | NC
end

module Id = Int63

module Domain = struct
  type 'a t = {
    serialize : (module Binable.S with type t = 'a) option;
    inspect : 'a -> Sexp.t;
    empty : 'a;
    order : 'a -> 'a -> Order.partial;
    name : string;
  }

  let empty_serializer (type a) empty =
    let module S = struct
      type t = a
      include Binable.Of_binable(Unit)(struct
          type t = a
          let to_binable _ = ()
          let of_binable () = empty
        end)
    end  in
    (module S : Binable.S with type t = a)

end

type 'a obj = Id.t
type 'a ord = Id.comparator_witness

module Class = struct
  type +'a t = {
    id : Id.t;
    name : string;
  }


  type info = {
    desc : string option;
    slots : (string * string option) Hashtbl.M(String).t;
  }

  let classes = ref Int63.zero

  let info desc = {
    desc;
    slots = Hashtbl.create (module String);
  }

  let public_classes = Hashtbl.create (module String)

  let register_class ?desc name =
    match Hashtbl.add public_classes ~key:name ~data:(info desc) with
    | `Ok -> ()
    | `Duplicate -> invalid_argf "Failed to register public class %s\
                                  - the name is not unique" name ()

  let register_slot ?desc cls name =
    let {slots} = Hashtbl.find_exn public_classes cls.name in
    match Hashtbl.add slots ~key:name ~data:(name,desc) with
    | `Ok -> ()
    | `Duplicate ->
      invalid_argf "Failed to register slot %s of class %s - \
                    the slot name is not unique, but made public"
        name cls.name ()

  let newclass name =
    Int63.incr classes;
    {id = !classes; name}

  let declare ?(public=false) ?desc name =
    if public then register_class ?desc name;
    newclass name

  let derived ?desc name parent =
    let name = parent.name ^ "/" ^ name in
    if Hashtbl.mem public_classes parent.name
    then register_class ?desc name;
    newclass name


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

module Record = Univ_map

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

    let declare ?desc cls name (dom : 'a Domain.t) =
      let key = Type_equal.Id.create ~name dom.inspect in
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
