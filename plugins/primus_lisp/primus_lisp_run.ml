let doc = "Runs the Primus lisp program."

open Core_kernel
open Bap_knowledge
open Bap_main
open Extension.Syntax

open Bap.Std
open Bap_primus.Std

type error =
  | Unknown_arch of string
  | Unknown_system of Knowledge.Name.t
  | Conflict of Knowledge.Conflict.t
  | Primus_exn of Primus.exn

type Extension.Error.t += Failed of error

module Spec = struct
  open Extension
  open Command
  let entry = argument Type.(some string)
  let arch = parameter Type.(string =? "x86_64") "arch"
  let system = parameter Type.(string =? "bap:stubbed-executor") "system"
  let t = args $entry $arch $system
end

module Linker = Primus.Linker.Make(Primus.Analysis)

let fail err = Error (Failed err)

let () = Extension.Command.declare ~doc "eval-lisp" Spec.t @@
  fun entry arch system _ctxt ->
  match Arch.of_string arch with
  | None -> fail (Unknown_arch arch)
  | Some arch ->
    let code = Memmap.empty and data = Memmap.empty in
    let input = Project.Input.create arch "<empty>" ~code ~data in
    let proj = ok_exn (Project.create input) in
    let start = Option.map entry ~f:(fun x -> Linker.exec (`symbol x)) in
    let system = Knowledge.Name.of_string system in
    let init = Knowledge.empty in
    match Primus.System.Repository.find system with
    | None -> fail (Unknown_system system)
    | Some system ->
      match Primus.System.run ?start system proj init with
      | Error err -> fail (Conflict err)
      | Ok (status,_proj,_) -> match status with
        | Primus.Normal | Exn Primus.Interpreter.Halt -> Ok ()
        | Exn other -> fail (Primus_exn other)

let string_of_error = function
  | Unknown_arch name ->
    sprintf
      "Unknown architecture %S. List of supported architectures: %s."
      name
      (String.concat ~sep:", " @@ List.map ~f:Arch.to_string Arch.all)
  | Unknown_system name ->
    sprintf
      "Unknown system %S, see `bap primus-systems` for the list of \
       known systems." (Knowledge.Name.to_string name)
  | Conflict err ->
    Format.asprintf "A conflict in the knowledge base: %a"
      Knowledge.Conflict.pp err
  | Primus_exn exn ->
    sprintf "An unexpected Primus exception: %s"
      (Primus.Exn.to_string exn)

let () = Extension.Error.register_printer @@ function
  | Failed err -> Some (string_of_error err)
  | _ -> None
