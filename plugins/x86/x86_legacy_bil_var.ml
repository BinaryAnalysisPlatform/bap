(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(**
    The type used for variables and functions to create and use them.
*)
open Core_kernel

module Type = X86_legacy_bil_type

module V = struct
  type t = V of int * string * Type.typ
  [@@deriving sexp]

  let hash (V(i,_,_)) = i

  (* is it faster to use the generic compare, or < and = ? *)
  let compare (V(x,_,_)) (V(y,_,_)) = compare x y
end

include V

let newvar =
  let varcounter = ref 0 in
  (fun s t ->
     let n = !varcounter in
     if n = -1 then failwith "newvar: counter wrapped around";
     (varcounter := n+1;
      V(n,s,t))
  )


let typ (V(_,_,t)) = t

let name (V(_,n,_)) = n

type defuse = {defs : t list;
               uses : t list}

(* These define equality so include them at the end so we don't shadow the
 * int comparison in newvar.
*)
include Comparable.Make(V)
include Hashable.Make_and_derive_hash_fold_t(V)
