open Core_kernel.Std
open Bap.Std
open Bap_c_type

module Registry(T : T) = struct
  let registry : T.t list ref = ref []
  let register x = registry := x :: !registry
end

type 'a pass = attr -> 'a term -> 'a term

include Registry(struct type t = sub pass end)
let apply attr sub =
  List.fold !registry ~init:sub ~f:(fun sub f -> f attr sub)
