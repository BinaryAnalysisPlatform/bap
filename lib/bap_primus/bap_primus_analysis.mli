open Bap_primus_types
open Bap_knowledge

module Machine : sig
  open Knowledge
  include Machine with type 'a m = 'a Knowledge.t
                   and type 'a t = 'a Bap_primus_machine.Make(Knowledge).t

  val collect : ('a,'p) slot -> 'a obj -> 'p t
  val resolve : ('a,'p opinions) slot -> 'a obj -> 'p t
  val provide : ('a,'p) slot -> 'a obj -> 'p -> unit t
  val suggest : agent -> ('a,'p opinions) slot -> 'a obj -> 'p -> unit t
end
