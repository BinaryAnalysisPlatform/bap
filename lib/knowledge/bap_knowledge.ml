open Core_kernel
open Monads.Std

module Domain = Bap_knowledge_domain
module L = Bap_knowledge_label
module Semantics = Bap_knowledge_semantics

type semantics = Semantics.t
type label = L.t
type conflict = ..

module Conflict = struct
  type t = conflict = ..
end

module Pid = Int63
type pid = Pid.t

module Knowledge = struct
  module Base = struct
    type t = {
      data : semantics L.Map.t;
      reqs : L.Set.t Pid.Map.t;
    }
  end

  type state = Base.t
  let empty : Base.t = {
    data = L.Map.empty;
    reqs = Pid.Map.empty;
  }

  module State = struct
    include Monad.State.T1(Base)(Monad.Ident)
    include Monad.State.Make(Base)(Monad.Ident)
  end

  module Knowledge = struct
    type 'a t = ('a,conflict) result State.t
    include Monad.Result.Make(Conflict)(State)
  end

  open Knowledge.Syntax

  type 'a promise = {
    get : label -> 'a Knowledge.t;
    pid : pid;
  }

  type 'a content = {
    domain : 'a Semantics.domain;
    name : string;
    info : string option;
    mutable promises : 'a promise list;
  }

  let registry = String.Table.create ()

  let register_public_service name desc =
    if Hashtbl.mem registry name
    then invalid_argf "Content name is not uniqiue: %S" name ();
    Hashtbl.set registry ~key:name ~data:desc

  let declare ?(public=false) ?desc ~name domain =
    if public
    then register_public_service name desc;
    { domain; name; info=desc; promises = []}

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)


  let provide tag id info =
    update @@ fun s -> {
      s with
      data = Map.update s.data id ~f:(function
          | None -> Semantics.put tag.domain Semantics.empty info
          | Some sema -> Semantics.put tag.domain sema info)
    }


  let promises = ref Pid.zero
  let promise tag get =
    Pid.incr promises;
    let pid = !promises in
    tag.promises <- {get;pid} :: tag.promises

  let is_active reqs pid id = match Map.find reqs pid with
    | None -> false
    | Some ids -> Set.mem ids id

  let activate reqs pid id = Map.update reqs pid ~f:(function
      | None -> L.Set.singleton id
      | Some ids -> Set.add ids id)

  let deactivate req pid id = Map.change req pid ~f:(function
      | None -> None
      | Some ids ->
        let ids = Set.remove ids id in
        if Set.is_empty ids then None else Some ids)

  let collect (type t) tag id =
    let module Domain =
      (val Semantics.domain tag.domain : Domain.S with type t = t) in
    tag.promises |>
    Knowledge.List.fold ~init:Domain.empty ~f:(fun curr req ->
        get () >>= fun s ->
        if is_active s.reqs req.pid id
        then Knowledge.return curr
        else put {s with reqs = activate s.reqs req.pid id} >>= fun () ->
          req.get id >>= fun next ->
          put {s with reqs = deactivate s.reqs req.pid id} >>= fun () ->
          match Domain.partial curr next with
          | GE -> Knowledge.return curr
          | LE | EQ | NC -> Knowledge.return next) >>= fun max ->
    provide tag id max >>= fun () ->
    gets @@ fun {data} ->
    let sema = Map.find data id |> function
      | None -> Semantics.empty
      | Some sema -> sema in
    Semantics.get tag.domain sema

  include Knowledge
  let domain x = x.domain

  let run x s = match State.run x s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err
end

type state = Knowledge.state
type 'a knowledge = 'a Knowledge.t
type 'a domain = 'a Semantics.domain
type 'a content = 'a Knowledge.content


module Label = struct
  include L
  let label = Semantics.declare "label" (module Domain.Label)
  let labels = Knowledge.declare ~name:"labels-generator" label

  module Generator = struct
    open Knowledge.Syntax
    let fresh =
      Knowledge.collect labels L.root >>= fun last ->
      let next = L.succ last in
      Knowledge.provide labels L.root next >>| fun () ->
      next
  end





end
