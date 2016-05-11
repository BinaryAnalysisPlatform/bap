open Bap.Std


(** Stack abstraction.

    Most architectures use stack to pass arguments. This is a helper
    module, that implements a default stack structure.
*)
module Stack : sig
  type direction = [`up | `down]
  (** [create ?direction arch] creates a [stack] abstraction for the
      given architecture. A [stack n], evaluates to an expression that
      loads a word of size [s] from address [SP # n * s], where [s] is
      a word size for the given architecture; [#] is [+] if the stack
      grows downward and [-] if it grows upwards. The [direction]
      parameter specifies the stack growth direction and defaults to
      [`down] ward *)
  val create : ?direction:direction -> arch -> (int -> exp)
end
