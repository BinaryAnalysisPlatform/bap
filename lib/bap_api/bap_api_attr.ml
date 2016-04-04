open Core_kernel.Std
open Bap.Std
open Bap_api

module Registry(T : T) = struct
  let registry : T.t list ref = ref []
  let register x = registry := x :: !registry
end

type 'a pass = attr -> 'a term -> 'a term

module Arg = struct
  include Registry(struct type t = pos -> arg pass end)
  let apply pos attr arg =
    List.fold !registry ~init:arg ~f:(fun arg f -> f pos attr arg)
end

module Sub = struct
  include Registry(struct type t = sub pass end)
  let apply attr sub =
    List.fold !registry ~init:sub ~f:(fun sub f -> f attr sub)
end
