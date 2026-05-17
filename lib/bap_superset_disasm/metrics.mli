open Core
open Bap.Std
  
module Cache : sig
  open Bap_knowledge
  open Bap_core_theory
  open Theory

  val sym_label : program Knowledge.obj KB.t
  val ground_truth_source : (program, string) Knowledge.slot

  val function_entrances : (program, Addr.Set.t option) Knowledge.slot

  val ground_truth : (program, Addr.Set.t option) Knowledge.slot

  val size : (program, int option) Knowledge.slot

  val time : (program, int option) Knowledge.slot

  val occlusive_space : (program, int option) Knowledge.slot

  val reduced_occlusion : (program, int option) Knowledge.slot

  val false_negatives : (program, int option) Knowledge.slot

  val false_positives : (program, int option) Knowledge.slot

  val true_positives : (program, int option) Knowledge.slot

  val clean_functions : (program, Addr.Set.t option) Knowledge.slot
    
end

val set_ground_truth : Superset.t -> unit
val compute_metrics : Superset.t -> unit
val true_positives : Superset.t -> string -> Addr.Hash_set.t     
type t = {
    size      : int option;
    time      : int option;
    occ       : int option;
    occ_space : int option;
    fe        : int option;
    clean     : int option;
    fns       : int option;
    fps       : int option;
    tps       : int option;
  }

val t_of_sexp : Sexp.t -> t
val sexp_of_t : t -> Sexp.t

val get_summary : unit -> t
