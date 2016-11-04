open Core_kernel.Std
open Bap.Std
open Monads.Std
open Primus_types

module Error = Primus_error
module Observation = Primus_observation

type ('a,'e) result = ('a,'e) Monad.Result.result =
  | Ok of 'a
  | Error of 'e


module Make(M : Monad.S) : Machine
= struct
  module SM = struct
    include Monad.State.Multi.T2(M)
    include Monad.State.Multi.Make2(M)
  end

  type ('a,'e) t = (('a,Error.t) result,'e state) SM.t
  and 'e state = {
    ctxt : 'e;
    local : Univ_map.t;
    global : Univ_map.t;
    observations : 'e observations
  }
  and 'e observations = (unit,'e) t Observation.observations

  type ('a,'e) e = (('a,Error.t) result,'e) SM.e
  type 'a m = 'a M.t
  module Basic = struct
    open SM.Syntax
    type nonrec ('a,'e) t = ('a,'e) t
    let return x = SM.return (Ok x)

    let bind (m : ('a,'e) t) (f : 'a -> ('b,'e) t) : ('b,'e) t = m >>= function
      | Ok r -> f r
      | Error err -> SM.return (Error err)
    let map = `Define_using_bind
  end

  module Fail = struct
    let fail err = SM.return (Error err)
    let catch m f = SM.bind m (function
        | Error err -> f err
        | ok -> SM.return ok)
  end

  type _ error = Error.t
  include Fail
  include Monad.Make2(Basic)

  type id = Monad.State.Multi.id
  module Id = Monad.State.Multi.Id

  let lifts m = SM.map m ~f:(fun x -> Ok x)

  let with_global_context (f : (unit -> ('a,'b) t)) =
    lifts (SM.current ())       >>= fun id ->
    lifts (SM.switch SM.global) >>= fun () ->
    f ()                >>= fun r  ->
    lifts (SM.switch id)        >>| fun () ->
    r

  let get_local () = lifts (SM.gets @@ fun s -> s.local)
  let get_global () = with_global_context @@ fun () ->
    SM.gets @@ fun s -> Ok s.global

  let set_local local = lifts @@ SM.update @@ fun s ->
    {s with local}

  let set_global global = with_global_context @@ fun () ->
    lifts (SM.update @@ fun s -> {s with global})

  (* we can actually factor out the type 'a Observation.t of the monad *)
  module Observation = struct
    type ('a,'e) m = ('a,'e) t
    type nonrec 'a observation = 'a observation
    type nonrec 'a statement = 'a statement

    let provide ?inspect name =
      let named s = Sexp.(List [Atom "observation"; Atom name; s]) in
      let sexp_of = match inspect with
        | None -> fun _ -> named @@ Sexp.List []
        | Some f -> fun n -> named @@ f n in
      let k = Univ_map.Key.create ~name sexp_of in
      k,k


    let observations () = lifts (SM.gets @@ fun s -> s.observations)

    let set_observations observations = with_global_context @@ fun () ->
      lifts (SM.update @@ fun s -> {s with observations})

    let make key obs =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      Observation.with_observers os key ~f:(List.iter ~f:(fun observe -> observe obs))

    let observe key observer =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      set_observations (Observation.add_observer os key observer)
  end

  module State(S : sig
      val get : unit -> (Univ_map.t,'e) t
      val set : Univ_map.t -> (unit,'e) t
      val typ : string
    end) = struct
    module Dict = Univ_map
    module Key = Dict.Key
    type ('a,'e) m = ('a,'e) t
    type 'a t = {
      key : 'a Key.t;
      init : Context.t -> 'a; (* must be total...*)
    }

    let create ?(inspect=sexp_of_opaque) ~name init =
      let sexp x = Sexp.List [
          Sexp.Atom (sprintf "%s:%s" name S.typ);
          inspect x;
        ] in {
        key = Key.create ~name sexp;
        init
      }

    let get data =
      S.get () >>= fun d ->
      match Dict.find d data.key with
      | Some r -> return r
      | None -> lifts (SM.get ()) >>= fun {ctxt} ->
        return (data.init (ctxt :> Context.t))

    let put data x =
      S.get () >>= fun d -> S.set (Dict.set d data.key x)

    let update data ~f =
      get data >>= fun s -> put data (f s)
  end

  module Local = State(struct
      let typ = "local"
      let get = get_local
      let set = set_local
    end)

  module Global = State(struct
      let typ = "global"
      let get = get_global
      let set = set_global
    end)

  let put ctxt = lifts @@ SM.update @@ fun s -> {s with ctxt}
  let get () = lifts (SM.gets @@ fun s -> s.ctxt)
  let gets f = get () >>| f
  let update f = get () >>= fun s -> put (f s)
  let modify m f = m >>= fun x -> update f >>= fun () -> return x

  let run : ('a,'e) t -> ('a,'e) e = fun m ctxt ->
    M.bind (SM.run m {
        global = Univ_map.empty;
        local = Univ_map.empty;
        observations = Primus_observation.empty;
        ctxt}) @@ fun (x,{ctxt}) -> M.return (x,ctxt)

  let eval m s = M.map (run m s) ~f:fst
  let exec m s = M.map (run m s) ~f:snd
  let lift x = lifts (SM.lift x)
  let status x = lifts (SM.status x)
  let forks () = lifts (SM.forks ())
  let kill id = lifts (SM.kill id)
  let fork () = lifts (SM.fork ())
  let ancestor x  = lifts (SM.ancestor x)
  let parent () = lifts (SM.parent ())
  let switch id = lifts (SM.switch id)
  let global = SM.global
  let current () = lifts (SM.current ())

end


module Test = struct
  module Machine = Make(Monad.Ident)
  open Machine.Syntax

  module Interpreter = struct
    let (undefined_var : var observation),undefined =
      Observation.provide "undefined-variable"
  end

  module Concretizer = struct
    let init () : (unit,#Context.t) Machine.t =
      Machine.Observation.observe Interpreter.undefined_var @@ fun var ->
      Machine.get () >>= fun ctxt ->
      let arch = Project.arch ctxt#project in
      Format.printf "undefined var in %a\n" Arch.pp arch;
      Machine.return ()


  end
end
