let man = {|
  # DESCRIPTION

  Disassembles and analyzes the input file. This is the default
  command of the bap frontend which is assumed when no other command
  was specified.

  The input file is automatically parsed (unless the $(b,--raw) or
  $(b,--loader) options were specified), then the extracted code, if
  any, is disassembled, and the `Bap.Std.project` data structure is
  built, on which the specified passes are run.


  # PASSES

  The passes are specified by the $(b,--pass) option and are run in the
  order in which they specified. In addition, all passes that are
  flagged with `autorun' are run before the explicitly specified
  passes. Finally, if a pass specifies other passes as its
  dependencies, then they are run before it, in the order in which
  they were specified (modulo their own dependencies).

  It's also possible to specify the passes using the old style syntax,
  e.g., `$(b,--<PASS>)`, which is discouraged and later could be disabled.
  Additionaly, it is not allowed to mix passes the old and the new
  style.

  # OUTPUT

  After all passes are run on the input, the resulting project data
  structure could be dumped using the $(b,--dump) (or $(b,-d) for short)
  option, whichaccepts the desired format and, optionally, the output
  file name.

  It is possible to specify the $(b,--dump) option multiple times, in
  which case the project will be dumped in several formats.

  # WRITING A NEW PASS

  To implement your own analysis as a pass on the project data
  structure, you need to write a program in OCaml.

  Start in a fresh new folder and create the `my_analysis.ml` file
  with the following contents:

  ```
  open Core_kernel
  open Bap_main
  open Bap.Std

  let main proj =
    print_endline "My analysis is running!";
    proj

  let () = Extension.declare @@ fun _ctxt ->
     Project.register_pass main
  ```

  Then run

  ```
  bapbuild my_analysis.plugin
  bapbundle install my_analysis.plugin
  ```

  The analysis is ready and could be run using the `$(b,--passes)
  option, e.g.,

  ```
  bap dis /bin/echo --pass=my-analysis
  ```

  Please note, that the name of the pass is automatically derived from
  the plugin name. If the latter contains underscores then they are
  substituted with dashes. If a plugin registers a pass with a
  different name it will be still prefixed with the plugin name.

  # EXAMPLES

```
  bap dis /bin/echo --passes=run,check -dbir:out.bir -dasm:out.asm
```
|}

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
  "loader";
]

type failure =
  | Expects_a_regular_file
  | Old_and_new_style_passes
  | Unknown_pass of string
  | Incompatible_options of string * string
  | Project of Error.t
  | Pass of Project.Pass.error
  | Unknown_format of string
  | Unavailable_format_version of string

type Extension.Error.t += Fail of failure

module Err = Monad.Result.Make(Extension.Error)(Monad.Ident)
open Err.Syntax

let pass_error = Result.map_error ~f:(fun err -> Fail (Pass err))
let proj_error = Result.map_error ~f:(fun err -> Fail (Project err))

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
    (Plugins.list () |> List.map ~f:Plugin.name)
    ident

let passes =
  Extension.Command.parameters
    ~doc:"Run the selected passes (in the specified order)"
    ~aliases:["p"]
    Extension.Type.("PASSES" %: string) "passes"

let outputs =
  Extension.Command.parameters
    ~doc:"Dumps the program to <FILE> (defaults to stdout) \
          in the <FMT> format (defaults to bir)."
    ~as_flag:"bir"
    ~aliases:["d"]
    Extension.Type.("[<FMT>[:<FILE>]]" %: string)
    "dump"

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string)

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader.
          Use the loader `raw' to loade unstructured files"
    Extension.Type.(string =? "llvm")
    "loader"

let validate_input file =
  Result.ok_if_true (Sys.file_exists file)
    ~error:(Fail Expects_a_regular_file)

let validate_passes_style old_style_passes new_style_passes =
  match old_style_passes, new_style_passes with
  | xs,[] | [],xs -> Ok xs
  | _ -> Error (Fail Old_and_new_style_passes)

let validate_passes passes =
  let known = Project.passes () |>
              List.map ~f:(fun p -> Project.Pass.name p, p) |>
              Map.of_alist_exn (module String) in
  Result.all @@
  List.map passes ~f:(fun p -> match Map.find known p with
      | Some p -> Ok p
      | None -> Error (Fail (Unknown_pass p)))

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
      | None -> Error (Fail (Unknown_format fmt))
      | Some _ -> Error (Fail (Unavailable_format_version fmt))

  let parse outputs =
    Result.all @@
    List.map outputs ~f:parse_format
end

let setup_gc () =
  let opts = Caml.Gc.get () in
  info "Setting GC parameters";
  Caml.Gc.set {
    opts with
    window_size = 20;
    minor_heap_size = 1024 * 1024;
    major_heap_increment = 64 * 1024 * 1024;
    space_overhead = 200;
  }

let has_env var = match Sys.getenv var with
  | exception _ -> false
  | _ -> true

let _disassemble_command_registered : unit =
  Extension.Command.(begin
      declare ~doc:man "disassemble"
        ~requires:features_used
        (args $input $outputs $old_style_passes $passes $loader)
    end) @@
  fun input outputs old_style_passes passes loader ctxt ->
  if not (has_env "OCAMLRUNPARAM" || has_env "CAMLRUNPARAM")
  then setup_gc ()
  else info "GC parameters are overriden by a user";
  validate_input input >>= fun () ->
  validate_passes_style old_style_passes passes >>=
  validate_passes >>= fun passes ->
  Dump_formats.parse outputs >>= fun outputs ->
  let digest = make_digest [
      Extension.Configuration.digest ctxt;
      Caml.Digest.file input;
      loader;
    ] in
  import_knowledge_from_cache digest;
  let state = load_project_state_from_cache digest in
  let input = Project.Input.file ~loader ~filename:input in
  Project.create
    input |> proj_error >>= fun proj ->
  if Option.is_none state then begin
    store_knowledge_in_cache digest;
    save_project_state_to_cache digest (Project.state proj);
  end;
  process passes outputs proj

let pp_guesses ppf badname =
  let guess = String.map badname ~f:(function
      | '_' -> '-'
      | c -> Char.lowercase c) in
  let suffix = "-" ^ name in
  let good_guess name =
    name = guess || String.is_suffix ~suffix name in
  let guesses =
    Project.passes () |>
    List.filter_map ~f:(fun p ->
        let name = Project.Pass.name p in
        Option.some_if (good_guess name) name) in
  let pp_sep ppf () = Format.pp_print_string ppf ", or" in
  match guesses with
  | [] -> Format.fprintf ppf "make sure that your plugin is installed"
  | guesses ->
    Format.fprintf ppf "did you mean %a?"
      (Format.pp_print_list ~pp_sep Format.pp_print_string) guesses

let nice_pp_error fmt er =
  let module R = Info.Internal_repr in
  let rec pp_sexp fmt = function
    | Sexp.Atom x -> Format.fprintf fmt "%s\n" x
    | Sexp.List xs -> List.iter ~f:(pp_sexp fmt) xs in
  let rec pp fmt r =
    let open R in
    match r with
    | With_backtrace (r, backtrace) ->
      Format.fprintf fmt "%a\n" pp r;
      Format.fprintf fmt "Backtrace:\n%s" @@ String.strip backtrace
    | String s -> Format.fprintf fmt "%s" s
    | r -> pp_sexp fmt (R.sexp_of_t r) in
  Format.fprintf fmt "%a" pp (R.of_info (Error.to_info er))

let string_of_failure = function
  | Expects_a_regular_file ->
    "expected a regular file as input"
  | Old_and_new_style_passes ->
    "passes are specified in both old an new style, \
     please switch to the new style, e.g., `-p<p1>,<p2>,<p3>'"
  | Unknown_pass name ->
    asprintf "failed to find the pass named %S, %a" name pp_guesses name
  | Incompatible_options (o1,o2) ->
    sprintf "options `%s' and `%s' can not be used together" o1 o2
  | Project err ->
    asprintf "failed to build a project: %a" nice_pp_error err
  | Pass (Project.Pass.Unsat_dep (p,s)) ->
    sprintf "dependency %S of pass %S is not available"
      s (Project.Pass.name p)
  | Pass (Project.Pass.Runtime_error (p, Exn.Reraised (bt,exn))) ->
    asprintf "pass %S failed in runtime with %a@\nBacktrace:@\n%s@\n"
      (Project.Pass.name p) Exn.pp exn bt
  | Pass (Project.Pass.Runtime_error (p, exn)) ->
    asprintf "pass %S failed at runtime with %a"
      (Project.Pass.name p) Exn.pp exn
  | Unknown_format fmt ->
    sprintf "unknown format %S" fmt
  | Unavailable_format_version fmt ->
    sprintf "unsupported version of the format %S" fmt

let () = Extension.Error.register_printer @@ function
  | Fail err -> Some (string_of_failure err)
  | _ -> None
