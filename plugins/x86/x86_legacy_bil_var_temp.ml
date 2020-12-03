(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
module Bil = X86_legacy_bil
open Bil

let temp_prefix = "T_"
let temp_prefix_len = String.length temp_prefix

(** [is_temp_name s] returns true iff s denotes a temporary variable
    name.
*)
let is_temp_name s =
  (* First try VEX style vars *)
  (String.length s > temp_prefix_len) && (String.sub s 0 2 = temp_prefix)

let nt s t =
  if (is_temp_name s) then
    Var.newvar s t
  else
    let newname = temp_prefix^s in
    let () = assert (is_temp_name newname) in
    Var.newvar (newname) t
