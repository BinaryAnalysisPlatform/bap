open Bap_knowledge
open Bap_bil

val bil : stmt list domain
val exp : exp option domain
val semantics : stmt list content

module Bil : Domain.S
module Exp : Domain.S
