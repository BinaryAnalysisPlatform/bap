(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Recognizing and creating temporary variables

    A temporary is a variable introduced by BAP's lifting process that
    is only referenced inside one assembly block.  The evaluator (and
    other BAP analyses) use this information to throw away any state
    stored for these temporaries once the temporary becomes out of
    scope (i.e., out of that assembly block).
*)

module Bil = X86_legacy_bil
open Bil

(** [nt n t] creates a new temporary variable with name [n] and type
    [t]. *)
val nt : string -> Type.typ -> Var.t
