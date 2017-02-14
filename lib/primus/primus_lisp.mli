open Bap.Std
open Primus_types

module Machine(Machine : Machine) : sig
  module Biri : Biri.S with type ('a,'e) state = ('a,'e) Machine.t


  val add_directory : string -> (unit,#Context.t) Machine.t
  val load_feature  : string -> (unit,#Context.t) Machine.t
  val run : (#Context.t as 'a) #Biri.t -> string -> word list -> (word, 'a) Machine.t

  val is_bound : string -> (bool,#Context.t) Machine.t
end
