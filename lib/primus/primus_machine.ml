open Core_kernel.Std
open Bap.Std
open Monads.Std
open Primus_types


module Multi = struct
  module Make(M : Monad.S) = struct
    module SM = struct
      include Monad.State.Multi.T2(M)
      include Monad.State.Multi.Make2(M)
    end
    type 'e state = {
      ctxt : 'e;
      local : Univ_map.t;
      global : Univ_map.t;
    }
    type ('a,'e) t = ('a,'e state) SM.t
    type ('a,'e) e = ('a,'e) SM.e
    type 'a m = 'a M.t
    module Basic = struct
      type ('a,'e) t = ('a,'e state) SM.t
      let return = SM.return
      let bind = SM.bind
      let map = `Custom SM.map
    end
    include Monad.Make2(Basic)

    type id = Monad.State.Multi.id
    module Id = Monad.State.Multi.Id

    let with_global_context f =
      SM.current ()       >>= fun id ->
      SM.switch SM.global >>= fun () ->
      f ()                >>= fun r  ->
      SM.switch id        >>| fun () ->
      r

    let get_local () = SM.gets @@ fun s -> s.local
    let get_global () = with_global_context @@ fun () ->
      SM.gets @@ fun s -> s.global

    let set_local local = SM.update @@ fun s ->
      {s with local}

    let set_global global = with_global_context @@ fun () ->
      SM.update @@ fun s -> {s with global}

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

      let create ?(observe=sexp_of_opaque) ~name init =
        let sexp x = Sexp.List [
            Sexp.Atom (sprintf "%s:%s" name S.typ);
            observe x;
          ] in {
          key = Key.create ~name sexp;
          init
        }

      let get data =
        S.get () >>= fun d ->
        match Dict.find d data.key with
        | Some r -> return r
        | None -> SM.get () >>= fun {ctxt} ->
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

    let put ctxt = SM.update @@ fun s -> {s with ctxt}
    let get () = SM.gets @@ fun s -> s.ctxt
    let gets f = get () >>| f
    let update f = get () >>= fun s -> put (f s)
    let modify m f = m >>= fun x -> update f >>= fun () -> SM.return x

    let run m ctxt =
      M.bind (SM.run m {
          global = Univ_map.empty;
          local = Univ_map.empty;
          ctxt}) @@ fun (x,{ctxt}) -> M.return (x,ctxt)

    let eval m s = M.map (run m s) ~f:fst
    let exec m s = M.map (run m s) ~f:snd
    let lift = SM.lift
    let status = SM.status
    let forks = SM.forks
    let kill = SM.kill
    let fork = SM.fork
    let ancestor = SM.ancestor
    let parent = SM.parent
    let switch = SM.switch
    let global = SM.global
    let current = SM.current
  end
end


module Error : sig
  type t = private error
  val register : (error -> string) -> error -> t
  val to_string : t -> string
end= struct
  type t = error = ..
  let str = ref (fun _ -> None)

  let to_string err = match !str err with
    | None -> assert false
    | Some msg -> msg

  let register to_string err =
    str := (fun e -> match !str e with
        | None -> Some (to_string e)
        | msg -> msg);
    err
end

module Make(M : Monad.S) = struct
  module ErrorM : Monad.S = struct
    type 'a m = 'a M.t
    type 'a t = ('a,Error.t) Monad.Result.result m
    type 'a e = ('a,Error.t) Monad.Result.result m
    include Monad.Result.Make(Error)(M)
  end

  include Multi.Make(ErrorM)
end
