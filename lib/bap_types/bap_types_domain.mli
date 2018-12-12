open Bap_knowledge
open Bap_bil

val bil : stmt list domain
val exp : exp option domain

module Bil : Domain.S
module Exp : Domain.S
