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

module Gnu = struct
  let register_attr n f =
    let pass {Attr.name; args} sub =
      if n = name then f args sub else sub in
    register pass

  exception Attr_type   of string * string
  exception Attr_index  of string
  exception Attr_arity  of string

  let int n =
    try Int.of_string n with exn -> raise (Attr_type ("<int>",n))

  let set attr v arg sub =
    Term.set_attr arg attr v |>
    Term.update arg_t sub

  let mark_arg attr v sub i =
    match Term.nth arg_t sub (int i - 1) with
    | None -> raise (Attr_index i)
    | Some arg -> set attr v arg sub


  let mark_args attr args sub =
    List.fold args ~init:sub ~f:(mark_arg attr ())

  let alloc_size = mark_args Arg.alloc_size

  let format args sub = match args with
    | [l;i;_] -> mark_arg Arg.format l sub i
    | _ -> raise (Attr_arity "3")

  let nonnull = mark_args Arg.nonnull

  let wur args sub = match Term.last arg_t sub with
    | None -> sub
    | Some arg -> set Arg.warn_unused () arg sub

  let set attr args sub =
    Term.set_attr sub attr ()

  let () =
    register_attr "alloc_size" alloc_size;
    register_attr "format" format;
    register_attr "nonnull" nonnull;
    register_attr "warn_unused_result" wur;
    register_attr "const" (set Sub.const);
    register_attr "pure" (set Sub.pure);
    register_attr "malloc" (set Sub.malloc);
    register_attr "noreturn" (set Sub.noreturn);
    register_attr "returns_twice" (set Sub.returns_twice);
    register_attr "noreturn" (set Sub.nothrow)
end
