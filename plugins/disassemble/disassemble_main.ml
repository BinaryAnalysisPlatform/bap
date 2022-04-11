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
open Bap_core_theory
open Regular.Std
open Monads.Std
open Format
open Bap_plugins.Std

include Self()
open Bap_main

let features_used = [
  "semantics";
  "function-starts";
  "disassembler";
  "lifter";
  "symbolizer";
  "rooter";
  "reconstructor";
  "brancher";
  "loader";
  "abi";
]

type failure =
  | Expects_a_regular_file of string
  | Old_and_new_style_passes
  | Unknown_pass of string
  | Incompatible_options of string * string
  | Project of Error.t
  | Pass of Project.Pass.error
  | Unknown_format of string
  | Unavailable_format_version of string
  | Unknown_collator of string
  | Unknown_analysis of string
  | No_knowledge


type Extension.Error.t += Fail of failure

module Err = Monad.Result.Make(Extension.Error)(Monad.Ident)
open Err.Syntax

let pass_error = Result.map_error ~f:(fun err -> Fail (Pass err))
let proj_error = Result.map_error ~f:(fun err -> Fail (Project err))

module Missing : sig
  val enable : unit -> unit
  val print : unit -> unit
end = struct
  open KB.Syntax
  module Basic = Disasm_expert.Basic.Insn

  let bool = KB.Domain.flat ~equal:Bool.equal "flat-bool"
      ~inspect:sexp_of_bool ~empty:false
  let has_semantics = KB.Class.property ~public:true ~package:"bap"
      Theory.Semantics.cls "has-semantics" bool

  let present eff =
    eff >>| fun x -> KB.Value.put has_semantics x true

  module Present : Theory.Core = struct
    include Theory.Empty
    let blk lbl data ctrl = present @@ blk lbl data ctrl
    let perform s = present @@ perform s
    let seq x y = present @@ seq x y
    let branch cnd yes nay = present @@ branch cnd yes nay
  end

  let pp_code ppf code =
    let dom = KB.Slot.domain Theory.Semantics.code in
    match KB.Domain.inspect dom code with
    | Sexp.List [Atom s] -> Format.fprintf ppf "%s" s
    | _ -> Format.fprintf ppf "not disassembled"

  let pp_ops ppf ops =
    if Array.is_empty ops then Format.fprintf ppf ""
    else Format.fprintf ppf " %s"
        (String.concat_array ~sep:" " @@
         Array.map ~f:Op.to_string ops)

  let pp_basic ppf insn =
    Format.fprintf ppf "(%s:%s%a)"
      (Basic.encoding insn)
      (Basic.name insn)
      pp_ops (Basic.ops insn)

  let update_missing insn histo =
    Map.update histo (Basic.name insn) ~f:(function
        | None -> 1
        | Some c -> c + 1)

  let build_histo =
    Map.fold ~init:Int.Map.empty ~f:(fun ~key ~data ->
        Map.add_multi ~key:data ~data:key)

  let pp_histo ppf stats =
    build_histo stats |>
    Map.to_sequence ~order:`Increasing_key |>
    Seq.iter ~f:(fun (count,codes) ->
        List.iter codes ~f:(Format.fprintf ppf "%-4d %s@\n" count))

  let print_missing () =
    let lifted = ref 0 and missed = ref 0 and failed = ref 0 in
    KB.objects Theory.Program.cls >>=
    KB.Seq.fold ~init:String.Map.empty ~f:(fun stats insn ->
        let* sema = KB.collect Theory.Semantics.slot insn in
        let code = KB.Value.get Theory.Semantics.code sema in
        if Option.is_none code then KB.return stats
        else KB.collect Theory.Label.addr insn >>= function
          | None -> KB.return stats
          | Some addr ->
            KB.collect Basic.slot insn >>| function
            | None ->
              Format.printf "%a: %a ; not disassembled@\n" Bitvec.pp
                addr pp_code code;
              incr failed;
              stats
            | Some _ when KB.Value.get has_semantics sema ->
              incr lifted;
              stats
            | Some basic ->
              incr missed;
              Format.printf "%a: %a ; %a ; %a@\n" Bitvec.pp addr
                pp_code code Insn.pp sema
                pp_basic basic;
              update_missing basic stats) >>| fun stats ->
    Format.printf "@\nHistogram:@\n%a@\n\
                   %-8s %d@\n\
                   %-8s %d@\n\
                   %-8s %d@\n"
      pp_histo stats "Lifted:" !lifted "Failed:" !failed "Missed:" !missed

  let print () =
    Toplevel.exec @@ print_missing ()

  let declare_theory () =
    Theory.declare
      ~package:"bap"
      ~name:"present"
      ~desc:"tracks the presence of semantics"
      (KB.return (module Present : Theory.Core))

  let enable () =
    declare_theory ()
end

let run_passes base proj =
  Err.List.fold ~init:(0,proj) ~f:(fun (step,proj) pass ->
      report_progress
        ~stage:(step+base)
        ~note:(Project.Pass.name pass) ();
      Project.Pass.run pass proj |> pass_error >>= fun proj ->
      Ok (step+1,proj))

let knowledge_reader = Data.Read.create
    ~of_bigstring:Knowledge.of_bigstring ()

let knowledge_writer = Data.Write.create
    ~to_bigstring:Knowledge.to_bigstring ()

let knowledge_cache () =
  Data.Cache.Service.request
    knowledge_reader
    knowledge_writer

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
  | None -> false
  | Some state ->
    info "importing knowledge from cache";
    Toplevel.set state;
    true

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
        Project.Io.show ~fmt ?ver proj);
  proj

let old_style_passes =
  Extension.Command.switches
    ~doc:(sprintf "Enables the pass %s in the old style (DEPRECATED)")
    (Plugins.list () |> List.map ~f:Plugin.name)
    ident

let passes =
  Extension.Command.parameters
    ~doc:"Run the selected passes (in the specified order)"
    ~aliases:["p"]
    Extension.Type.("PASSES" %: list string) "passes"

let outputs =
  Extension.Command.parameters
    ~doc:"Dumps the program to <FILE> (defaults to stdout) \
          in the <FMT> format (defaults to bir)."
    ~as_flag:"bir"
    ~aliases:["d"]
    Extension.Type.("[<FMT>[:<FILE>]]" %: string)
    "dump"

let rw_file = Extension.Type.define
    ~name:"<FILE>" ~print:ident ~parse:ident
    ~digest:(fun path ->
        if Sys.file_exists path
        then Caml.Digest.file path
        else Caml.Digest.string "empty")
    ""

let update =
  Extension.Command.flag "update" ~aliases:["u"]
    ~doc: "Preserve the knowledge base, i.e., do not change it."

let knowledge =
  Extension.Command.parameter
    ~doc:"Import the knowledge to the provided knowledge base. \
          If the $(b,--update) flag is set the the knowledge base \
          will be also updated with the new information. If \
          $(b,--update) is set, the the knowledge base might not \
          exist and it will be created"
    ~aliases:["k"; "knowledge-base";]
    (Extension.Type.some rw_file) "project"

let print_missing =
  Extension.Command.flag
    ~doc:"Print missing instructions. \
          This option disables cache and redisassembles the binary \
          from scratch. It then prints the list of all instructions \
          that do not have a representable semantics, followed by \
          the histogram of all missed opcodes, and finally prints \
          the number of lifted opcodes, the number of addresses \
          that wasn't disassembled at all, and the number of opcodes \
          that do not have semantics." "print-missing"

let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out" )

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader.
          The loader could be either an identifier or a filename. \
          The filename has to be explicit, i.e., to start with an \
          explicit reference to the root directory or to the current \
          directory (e.g., $(b,./), or $(b,../), or $(b,/) in Unix). \
          The contents of the file should be a well-formed OGRE \
          document that contains the necessary meta-information \
          about the binary. \
          The default loader is named $(b,llvm) and uses LLVM loaders \
          to parse the input binary and supports ELF, MachO, and COFF \
          (including Windows PE), formats. \
          To load unstructured files use the $(b,raw) loader and \
          specify the loader parameters via the $(b,raw) plugin."
    Extension.Type.(string =? "llvm")
    "loader"

let target =
  let t = Extension.Type.define Theory.Target.unknown
      ~name:"NAME"
      ~digest:(fun t -> Caml.Digest.string@@Theory.Target.to_string t)
      ~parse:(fun s -> match Theory.Target.lookup ~package:"bap" s with
          | Some t -> t
          | None ->
            invalid_argf "unknown target %S, please see \
                          `bap list targets' for the full list \
                          of targets" s ())
      ~print:Theory.Target.to_string in
  Extension.Command.parameter t "target"
    ~doc:"Refines the target architecture of the binary. \
          See `bap list targets` for the full hierarchy of targets. \
          The specified target must be a refinement of the actual \
          target stored in the binary, otherwise an error is signaled."

let validate_input file =
  Result.ok_if_true (Sys.file_exists file)
    ~error:(Fail (Expects_a_regular_file file))

let validate_knowledge update kb = match kb with
  | None -> Result.ok_if_true (not update)
              ~error:(Fail No_knowledge)
  | Some path ->
    Result.ok_if_true (Sys.file_exists path || update)
      ~error:(Fail No_knowledge)

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

let setup_gc_unless_overriden () =
  if not (has_env "OCAMLRUNPARAM" || has_env "CAMLRUNPARAM")
  then setup_gc ()
  else info "GC parameters are overriden by a user"

let load_knowledge disable digest = function
  | _ when disable -> false
  | None -> import_knowledge_from_cache digest
  | Some path when not (Sys.file_exists path) ->
    import_knowledge_from_cache digest
  | Some path ->
    info "importing knowledge from %S" path;
    Toplevel.set @@ Knowledge.load path;
    true

let save_knowledge ~had_knowledge ~update digest = function
  | None ->
    if not had_knowledge then store_knowledge_in_cache digest
  | Some path when update ->
    info "storing knowledge base to %S" path;
    Knowledge.save (Toplevel.current ()) path
  | Some _ -> ()

let create_and_process input outputs passes loader target update
    kb print_missing ctxt =
  let uses_file_loader = Sys.file_exists loader &&
                         Fn.non Filename.is_implicit loader in
  let package = input in
  let digest = make_digest [
      Extension.Configuration.digest ctxt;
      Caml.Digest.file input;
      if uses_file_loader then Caml.Digest.file loader else loader;
    ] in
  let had_knowledge = load_knowledge print_missing digest kb in
  let input = Project.Input.load ~target ~loader input in
  if print_missing then Missing.enable ();
  Project.create ~package
    input |> proj_error >>= fun proj ->
  process passes outputs proj >>| fun proj ->
  if print_missing then Missing.print ();
  save_knowledge ~had_knowledge ~update digest kb;
  proj

let _disassemble_command_registered : unit =
  let args =
    let open Extension.Command in
    args $input $outputs $old_style_passes $passes $loader $target
    $update $knowledge $print_missing in
  Extension.Command.declare ~doc:man "disassemble"
    ~requires:features_used args @@
  fun input outputs old_style_passes passes loader target update
    kb print_missing ctxt ->
  setup_gc_unless_overriden ();
  validate_knowledge update kb >>= fun () ->
  validate_input input >>= fun () ->
  validate_passes_style old_style_passes (List.concat passes) >>=
  validate_passes >>= fun passes ->
  Dump_formats.parse outputs >>= fun outputs ->
  create_and_process input outputs passes loader target update kb
    print_missing ctxt >>= fun _ ->
  Ok ()

let _compare_command_registered : unit =
  let base = Extension.Command.argument
      ~doc:"The base version."
      Extension.Type.("BASE" %: string =? "a.out") in

  let inputs = Extension.Command.arguments
      ~doc:"The alternative versions."
      Extension.Type.("ALT" %: string =? "b.out") in

  let collator = Extension.Command.argument
      ~doc:"The collator to use." Extension.Type.("COLLATOR" %: string) in

  let doc = {|
    # DESCRIPTION

    Compares several alternative versions of the binary with the base
    version, using the specified $(b,COLLATOR). For the list of
    available collators use $(b,bap list collators).

    # EXAMPLE

```
    bap compare callgraph testsuite/bin/*-echo
```
|} in

  let args =
    let open Extension.Command in
    args
    $collator
    $base
    $inputs
    $outputs
    $old_style_passes
    $passes
    $loader
    $target
    $update
    $knowledge in
  Extension.Command.declare "compare" ~doc ~requires:features_used args @@
  fun collator input inputs outputs old_style_passes passes
    loader target update kb ctxt ->
  match Project.Collator.find ~package:"bap" collator with
  | None -> Error (Fail (Unknown_collator collator))
  | Some collator ->
    setup_gc_unless_overriden ();
    Err.all_unit @@ List.map (input::inputs) ~f:validate_input >>= fun () ->
    validate_passes_style old_style_passes (List.concat passes) >>=
    validate_passes >>= fun passes ->
    Dump_formats.parse outputs >>= fun outputs ->
    let projs =
      Seq.map (Seq.of_list (input::inputs)) ~f:(fun input ->
          create_and_process input outputs passes loader target
            update kb false ctxt) in
    let exception Escape of Extension.Error.t in
    try
      let projs = Seq.map projs ~f:(function
          | Ok proj -> proj
          | Error e -> raise (Escape e))  in
      Project.Collator.apply collator projs;
      Ok ()
    with Escape failed -> Error failed

let pp_guesses ppf badname =
  let guess = String.map badname ~f:(function
      | '_' -> '-'
      | c -> Char.lowercase c) in
  let suffix = "-" ^ name in
  let good_guess name =
    String.equal name guess || String.is_suffix ~suffix name in
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

let is_verbose = Option.is_some (Sys.getenv_opt "BAP_DEBUG")

let pp_backtrace ppf bt =
  if is_verbose then fprintf ppf "Backtrace:@\n%s" bt

let pp_exn ppf = function
  | Invalid_argument s ->
    fprintf ppf "%s" s
  | Failure s ->
    fprintf ppf "%s" s
  | other -> fprintf ppf "%a" Exn.pp other

let nice_pp_error ppf er =
  let module R = Info.Internal_repr in
  let rec pp ppf r =
    let open R in
    match r with
    | With_backtrace (r, backtrace) ->
      Format.fprintf ppf "%a@\n%a" pp r
        pp_backtrace (String.strip backtrace);
    | Sexp (Sexp.List [Atom "Invalid_argument"; Atom s])
    | String s ->
      Format.fprintf ppf "%a" pp_print_text s;
    | _ ->
      let msg = Error.to_string_hum er in
      Format.fprintf ppf "%s" msg  in
  Format.fprintf ppf "%a" pp (R.of_info (Error.to_info er))

let string_of_failure = function
  | Expects_a_regular_file name ->
    sprintf "Unable to open file `%s'." name
  | Old_and_new_style_passes ->
    "Bad invocation: passes are specified in both old an new style, \
     please switch to the new style, e.g., `-p<p1>,<p2>,<p3>'"
  | Unknown_pass name ->
    asprintf "Bad invocation: failed to find the pass named %S, %a" name pp_guesses name
  | Incompatible_options (o1,o2) ->
    sprintf "Bad invocation: the options `%s' and `%s' can not be used together" o1 o2
  | Project err ->
    asprintf "@[Failed to build the project:@ %a@]" nice_pp_error err
  | Pass (Project.Pass.Unsat_dep (p,s)) ->
    sprintf "Can't run passes - the dependency %S of pass %S is not available."
      s (Project.Pass.name p)
  | Pass (Project.Pass.Runtime_error (p, Exn.Reraised (bt,exn))) ->
    asprintf "The pass %S failed with:@\n%a@\n%a"
      (Project.Pass.name p) pp_exn exn pp_backtrace bt
  | Pass (Project.Pass.Runtime_error (p, exn)) ->
    asprintf "The pass %S failed with:@\n%a"
      (Project.Pass.name p) pp_exn exn
  | Unknown_format fmt ->
    sprintf "The format %S is not known." fmt
  | Unavailable_format_version fmt ->
    sprintf "The selected version of the format %S is not supported." fmt
  | Unknown_collator "" ->
    "Please specify the collator that you want to use."
  | Unknown_collator s ->
    sprintf "The collator `%s' is not registered." s
  | Unknown_analysis s ->
    sprintf "There is no analysis with the name `%s'" s
  | No_knowledge ->
    sprintf "Expected the path to an existing knowledge base \
             (either add or remove the --update option)"

let () = Extension.Error.register_printer @@ function
  | Fail err -> Some (string_of_failure err)
  | _ -> None
