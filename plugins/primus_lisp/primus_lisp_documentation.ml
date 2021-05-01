open Core_kernel
open Bap_core_theory
open Bap_main
open Bap.Std
open Extension.Syntax
open KB.Syntax

open Bap_primus.Std
open Primus.Analysis.Syntax
open Format

type error = Conflict of KB.Conflict.t
           | Wrong_target of string
           | Wrong_system of string
           | Unexpected_status of Primus.exn

type Extension.Error.t += Failed of error

let fail prob = Error (Failed prob)

let print package index =
  List.iter index ~f:(fun (cat,elts) ->
      printf "* %a@\n" Primus.Lisp.Doc.Category.pp cat;
      List.iter elts ~f:(fun (name,desc) ->
          if String.equal (KB.Name.package name) package
          then printf "** ~%s~@\n%a@\n"
              (KB.Name.unqualified name)
              Primus.Lisp.Doc.Descr.pp desc))

let string_of_problem = function
  | Wrong_target s ->
    sprintf "Unknown target %S, see `bap list targets' \
             for the list of known targsts" s
  | Wrong_system s ->
    sprintf "Unknown system %S, see `bap primus-systems' \
             for the list of known systems" s
  | Conflict err ->
    sprintf "Failed to initialize Primus.@\n%s@."
      (KB.Conflict.to_string err)
  | Unexpected_status exn ->
    sprintf "Failed to initialize Primus.@\n%s@."
      (Primus.Exn.to_string exn)

let print_dynamic package target system =
  let proj = Project.empty target in
  let state = Toplevel.current () in
  let init =
    let open Primus.Lisp.Doc.Make(Primus.Analysis) in
    generate_index >>| print package in
  match Primus.System.run system proj state ~init with
  | Ok (Normal,_,_)
  | Ok (Exn Primus.Interpreter.Halt,_,_) -> Ok ()
  | Ok (Exn err,_,_) -> fail (Unexpected_status err)
  | Error problem -> fail (Conflict problem)

let print_static package target =
  let open KB.Syntax in
  Result.map_error ~f:(fun prob -> Failed (Conflict prob)) @@
  Toplevel.try_exec begin
    KB.Object.create Theory.Unit.cls >>= fun unit ->
    KB.provide Theory.Unit.target unit target >>= fun () ->
    Primus.Lisp.Semantics.documentation unit >>| print package
  end


let system = Extension.Command.parameter
    Extension.Type.(string =? "bap:legacy-main") "system"
    ~doc:"Print the documentation for the specified system"

let target = Extension.Command.parameter
    Extension.Type.(string =? ":unknown") "target"
    ~doc:"Print the documenentation for the specified target"

let semantics = Extension.Command.flag "semantics"
    ~doc:"Print the documentation for Primus Lisp semantics lifter"

let package = Extension.Command.parameter
    Extension.Type.(string =? "user") "package"
    ~doc:"Print the documentation for the specified package."

let spec = Extension.Command.(args $package $semantics $target $system)

let () = Extension.Error.register_printer @@ function
  | Failed problem -> Some (string_of_problem problem)
  | _ -> None

let () =
  Extension.Command.declare "primus-lisp-documentation" spec @@
  fun package semantics target system _ctxt ->
  match Theory.Target.lookup ~package:"bap" target with
  | None -> fail (Wrong_target target)
  | Some target ->
    if semantics then print_static package target
    else
      let name = KB.Name.read ~package:"bap" system in
      match Primus.System.Repository.find name with
      | None -> fail (Wrong_system system)
      | Some system ->
        print_dynamic package target system
