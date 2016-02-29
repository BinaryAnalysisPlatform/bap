open Core_kernel.Std

module type S = sig
  type t
  val fresh : unit -> t
  val store : t -> unit
end

module Make(T : sig
    type t
    val create : unit -> t
  end) = struct
  type t = T.t
  let state = ref @@ T.create ()
  let fresh () =
    state := T.create ();
    !state
  let store t = state := t
end
