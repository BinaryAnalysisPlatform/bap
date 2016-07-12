open Core_kernel.Std
open Bap.Std
open Frontend
open Format

type output = [`file of string | `stdout] [@@deriving sexp]
type t = output * string * string option [@@deriving sexp]

let parse_fmt fmt =
  match String.split ~on:'-' fmt with
  | [fmt;ver] -> fmt, Some ver
  | _ -> fmt,None

let flatten (x,(y,z)) = x,y,z

let split str = match String.split ~on:':' str with
  | [fmt;dst] -> flatten (`file dst,parse_fmt fmt)
  | _ -> flatten (`stdout,parse_fmt str)

let parse str =
  let (_,fmt,ver) as r = split str in
  match Project.find_writer ?ver fmt with
  | Some _ -> `Ok r
  | None -> match Project.find_writer fmt with
    | None -> `Error ("unrecognized format: " ^ fmt)
    | Some _ -> `Error ("unavailable version for format " ^ fmt)

let printer ppf spec = match spec with
  | `file n,fmt,Some v -> fprintf ppf "%s-%s:%s" fmt v n
  | `file n,fmt,None   -> fprintf ppf "%s:%s" fmt n
  | `stdout,fmt,Some v -> fprintf ppf "%s-%s" fmt v
  | `stdout,fmt,None   -> fprintf ppf "%s" fmt

let as_flag : t = (`stdout, "bir", None)

let converter : t Config.converter =
  Config.converter parse printer as_flag
