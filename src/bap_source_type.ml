open Core_kernel.Std
open Bap.Std
open Format
open Cmdliner

exception Unknown_arch of string
exception Unrecognized_source

type t = [
  | `Binary
  | `Project
  | `Memory of arch
] [@@deriving sexp]


let arch_exn str = match Arch.of_string str with
  | Some arch -> arch
  | None -> raise (Unknown_arch str)

let parse_source_type str = match String.split ~on:'-' str with
  | [arch;"code"] -> `Memory (arch_exn arch)
  | ["binary"] -> `Binary
  | ["project"] -> `Project
  | _ -> raise Unrecognized_source

let parse s =
  try `Ok (parse_source_type s) with
  | Unrecognized_source -> `Error "Bad option format"

let pp ppf = function
  | `Memory arch -> fprintf ppf "%a-code" Arch.pp arch
  | `Binary -> fprintf ppf "binary"
  | `Project -> fprintf ppf "project"

let t : 'a Arg.converter = parse,pp
