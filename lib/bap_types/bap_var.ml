open Core_kernel.Std
open Bap_common

module T = struct
  type t = {
    var : string;
    uid : int;
    typ : typ;
    tmp : bool;
  } with sexp, bin_io

  let compare v1 v2 = compare_int v1.uid v2.uid
  let hash v = v.uid
  let module_name = "Bap_var"

  let pp fmt v =
    Format.fprintf fmt "%s:%a" v.var Bap_type.pp v.typ
end

include T


let name v = v.var
let typ  v = v.typ
let is_tmp v = v.tmp



let create =
  let var_counter = ref 0 in
  fun ?(tmp=false) var typ ->
    incr var_counter;
    if var_counter.contents < 0
    then failwith "new_var: var_counter wrapped around"
    else {uid = !var_counter; var; typ; tmp}

module V1 = struct
  type r = string * int * typ
  let serialize v = v.var, v.uid, v.typ
  let deserialize (var,uid,typ) = {var; uid; typ; tmp=false}
end

include Regular.Make(T)
