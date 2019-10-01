open Bap_knowledge
open Core_kernel
open Bap.Std
open Regular.Std
open Monads.Std
open Format
open Bap_plugins.Std

include Self()
open Bap_main

let features_used = [
  "disassembler";
  "lifter";
  "symbolizer";
  "rooter";
  "reconstructor";
  "brancher";
]

type Extension.Error.t += Expects_a_regular_file
type Extension.Error.t += Old_and_new_style_passes
type Extension.Error.t += Unknown_pass of string
type Extension.Error.t += Incompatible_options of string * string
type Extension.Error.t += Project of Error.t
type Extension.Error.t += Pass of Project.Pass.error
type Extension.Error.t += Unknown_format of string
type Extension.Error.t += Unavailable_format_version of string

module Err = Monad.Result.Make(Extension.Error)(Monad.Ident)
open Err.Syntax

let pass_error = Result.map_error ~f:(fun err -> Pass err)
let proj_error = Result.map_error ~f:(fun err -> Project err)

let run_passes base proj =
  Err.List.fold ~init:(0,proj) ~f:(fun (step,proj) pass ->
      report_progress
        ~stage:(step+base)
        ~note:(Project.Pass.name pass) ();
      Project.Pass.run pass proj |> pass_error >>= fun proj ->
      Ok (step+1,proj))

let knowledge_cache () =
  let reader = Data.Read.create
      ~of_bigstring:Knowledge.of_bigstring () in
  let writer = Data.Write.create
      ~to_bigstring:Knowledge.to_bigstring () in
  Data.Cache.Service.request reader writer

let project_state_cache () =
  let module State = struct
    type t = Project.state [@@deriving bin_io]
  end in
  let of_bigstring = Binable.of_bigstring (module State) in
  let to_bigstring = Binable.to_bigstring (module State) in
  let reader = Data.Read.create ~of_bigstring () in
  let writer = Data.Write.create ~to_bigstring () in
  Data.Cache.Service.request reader writer

let import_knowledge_from_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "looking for knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  match Data.Cache.load cache digest with
  | None -> ()
  | Some state ->
    info "importing knowledge from cache";
    Toplevel.set state

let load_project_state_from_cache digest =
  let digest = digest ~namespace:"project" in
  let cache = project_state_cache () in
  Data.Cache.load cache digest

let save_project_state_to_cache digest state =
  let digest = digest ~namespace:"project" in
  let cache = project_state_cache () in
  Data.Cache.save cache digest state

let store_knowledge_in_cache digest =
  let digest = digest ~namespace:"knowledge" in
  info "caching knowledge with digest %a"
    Data.Cache.Digest.pp digest;
  let cache = knowledge_cache () in
  Toplevel.current () |>
  Data.Cache.save cache digest


let process passes outputs project =
  let autoruns = Project.passes () |>
                 List.filter ~f:Project.Pass.autorun in
  let autos = List.length autoruns in
  let total = List.length passes + autos in
  report_progress ~note:"analyzing" ~total ();
  run_passes 0 project autoruns >>= fun (step,proj) ->
  run_passes step proj passes >>| fun (_,proj) ->
  List.iter outputs ~f:(function
      | `file dst,fmt,ver ->
        Out_channel.with_file dst ~f:(fun ch ->
            Project.Io.save ~fmt ?ver ch proj)
      | `stdout,fmt,ver ->
        Project.Io.show ~fmt ?ver proj)

let old_style_passes =
  Extension.Command.switches
    ~doc:(sprintf "Enables the pass %s in the old style (DEPRECATED)")
    ident
    (Plugins.list () |> List.map ~f:Plugin.name)

let passes =
  Extension.Command.parameters
    ~docv:"PASSES"
    ~doc:"Run the selected passes (in the specified order)"
    ~short:'p' "passes" Extension.Type.string

let outputs =
  Extension.Command.parameters
    ~docv:"[<FMT>[:<FILE>]]"
    ~doc:"Dumps the program to <FILE> (defaults to stdout) \
          in the <FMT> format (defaults to bir)."
    ~as_flag:"bir"
    ~short:'d'
    "dump" Extension.Type.string

let input =
  Extension.Command.argument
    ~docv:"FILE"
    ~doc:"The input file" Extension.Type.string

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader"
    "loader"
    Extension.Type.(some string)

let raw_bytes =
  let parse s = match Arch.of_string s with
    | None -> invalid_argf "Unknown architecture %s" s ()
    | Some a -> a in
  let default = `x86_64 in
  let arch = Extension.Type.define
      ~parse ~print:Arch.to_string default in
  Extension.Command.parameter
    ~doc:"If specified, then the input will be treated as a \
          raw binary using the specified ISA"
    ~as_flag:(Some default) ~short:'r' "raw"
    (Extension.Type.some arch)


let validate_input file =
  Result.ok_if_true (Sys.file_exists file)
    ~error:Expects_a_regular_file

let validate_passes_style old_style_passes new_style_passes =
  match old_style_passes, new_style_passes with
  | xs,[] | [],xs -> Ok xs
  | _ -> Error Old_and_new_style_passes

let validate_passes passes =
  let known = Project.passes () |>
              List.map ~f:(fun p -> Project.Pass.name p, p) |>
              Map.of_alist_exn (module String) in
  Result.all @@
  List.map passes ~f:(fun p -> match Map.find known p with
      | Some p -> Ok p
      | None -> Error (Unknown_pass p))

let assert_only_one name1 name2 opt1 opt2 = match opt1,opt2 with
  | Some _, Some _ -> Error (Incompatible_options (name1,name2))
  | _ -> Ok ()

let option_digest f = function
  | None -> "none"
  | Some s -> f s

let make_digest inputs =
  let inputs = String.concat inputs in
  fun ~namespace ->
    let d = Data.Cache.Digest.create ~namespace in
    Data.Cache.Digest.add d "%s" inputs

module Dump_formats = struct
  let parse_fmt fmt =
    match String.split ~on:'-' fmt with
    | [fmt;ver] -> fmt, Some ver
    | _ -> fmt,None

  let flatten (x,(y,z)) = x,y,z

  let split str = match String.split ~on:':' str with
    | [fmt;dst] -> flatten (`file dst,parse_fmt fmt)
    | _ -> flatten (`stdout,parse_fmt str)

  let parse_format str =
    let (_,fmt,ver) as r = split str in
    match Project.find_writer ?ver fmt with
    | Some _ -> Ok r
    | None -> match Project.find_writer fmt with
      | None -> Error (Unknown_format fmt)
      | Some _ -> Error (Unavailable_format_version fmt)

  let parse outputs =
    Result.all @@
    List.map outputs ~f:parse_format
end


let _disassemble_command_registered : unit =
  Extension.Command.(begin
      declare "disassemble"
        (args $input $outputs $old_style_passes $passes $loader $raw_bytes)
    end) @@
  fun input outputs old_style_passes passes loader raw_bytes ctxt ->
  validate_input input >>= fun () ->
  validate_passes_style old_style_passes passes >>=
  validate_passes >>= fun passes ->
  assert_only_one "loader" "raw" loader raw_bytes >>= fun () ->
  Dump_formats.parse outputs >>= fun outputs ->
  let digest = make_digest [
      Extension.Context.digest ~features:features_used ctxt;
      Caml.Digest.file input;
      option_digest ident loader;
      option_digest Arch.to_string raw_bytes;
    ] in
  import_knowledge_from_cache digest;
  let state = load_project_state_from_cache digest in
  let input = match raw_bytes with
    | Some arch -> Project.Input.binary arch ~filename:input
    | None -> Project.Input.file ?loader ~filename:input in
  Project.create ?state input |> proj_error >>= fun proj ->
  if Option.is_none state then begin
    store_knowledge_in_cache digest;
    save_project_state_to_cache digest (Project.state proj);
  end;
  process passes outputs proj


(* let () =
 *   let () =
 *     try if Sys.getenv "BAP_DEBUG" <> "0" then
 *         Printexc.record_backtrace true
 *     with Caml.Not_found -> () in
 *   if not (has_env "OCAMLRUNPARAM" || has_env "CAMLRUNPARAM")
 *   then setup_gc ()
 *   else info "GC parameters are overriden by a user";
 *   Sys.(set_signal sigint (Signal_handle exit));
 *   let argv = load_recipe () in
 *   Log.start ?logdir:(get_logdir argv)();
 *   at_exit (pp_print_flush err_formatter);
 *   let argv,passes = run_loader argv in
 *   try main (parse passes argv); exit 0 with
 *   | Unknown_arch arch ->
 *     error "Invalid arch `%s', should be one of %s." arch
 *       (String.concat ~sep:"," (List.map Arch.all ~f:Arch.to_string))
 *   | Unrecognized_source ->
 *     error "Invalid format of source type argument"
 *   | Bap_plugin_loader.Plugin_not_found name ->
 *     error "Can't find a plugin bundle `%s'" name
 *   | Failed_to_create_project err ->
 *     error "Failed to create a project: %a" nice_pp_error err
 *   | Project.Pass.Failed (Project.Pass.Unsat_dep (p,n)) ->
 *     error "Dependency `%s' of pass `%s' is not loaded"
 *       n (Project.Pass.name p)
 *   | Pass_not_found p -> error "Failed to find pass: %s" p
 *   | Project.Pass.Failed
 *       (Project.Pass.Runtime_error
 *          (p, Exn.Reraised (backtrace, (Invalid_argument msg | Failure msg)))) ->
 *     error "Pass `%s' failed at runtime with %s\nBacktrace:\n%s"
 *       (Project.Pass.name p) msg backtrace
 *   | Project.Pass.Failed
 *       (Project.Pass.Runtime_error (p, Exn.Reraised (backtrace, exn))) ->
 *     error "Pass `%s' failed at runtime with: %a\nBacktrace:\n%s"
 *       (Project.Pass.name p) Exn.pp exn backtrace
 *   | exn ->
 *     error "Failed with an unexpected exception: %a\nBacktrace:\n%s"
 *       Exn.pp exn
 *     @@ Exn.to_string exn *)
