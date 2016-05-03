open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Regular.Std
open Re_perl
open Cmdliner
open Format
open Option.Monad_infix
include Self()

let objdump_opts = "-rd --no-show-raw-insn"
let version = "0.1"
(* a list of common names for objdump *)
let objdump_cmds = ["objdump"; (* default in general *)
                    "x86_64-elf-objdump"; (* macports default *)
                    "i386-elf-objdump" (*macports default *)
                   ]

(* expected format: [num] <[name]>:
   Note the use of "^\s" to stop greedy globing of the re "+"
   If you are not getting what you think you should,
   this regular expression is a good place to start with debugging.
*)
let func_start_re = "([0-9A-Fa-f^\\s]+) <(.*)>:"

let re r =
  Re_pcre.re r |> Re.compile |> Re.execp

let objdump_strip  =
  String.strip ~drop:(function '<' | '>' | ':' | ' ' -> true | _ -> false)

let text_to_addr l =
  objdump_strip l |> (^) "0x" |> Int64.of_string

let parse_func_start l =
  if re func_start_re l then
    let xs = String.split_on_chars ~on:[' '; '@'] l in
    match xs with
      addr::name::[]  (* name w/o @plt case *)
    | addr::name::_::[] -> (* name@plt case *)
      Some(objdump_strip name, text_to_addr addr)
    | _ -> None
  else
    None

let run_objdump cmd opts arch file : symbolizer =
  let fullcmd = cmd ^ " " ^ opts ^ " " ^ file in
  let names = Addr.Table.create () in
  let ic = Unix.open_process_in fullcmd in
  let width = Arch.addr_size arch |> Size.in_bits in
  let add name addr =
    Hashtbl.set names ~key:(Addr.of_int64 ~width addr) ~data:name in
  In_channel.iter_lines ~f:(fun line -> match parse_func_start line with
      | None -> ()
      | Some (name,addr) -> add name addr) ic;
  In_channel.close ic;
  if Hashtbl.length names = 0
  then warning "failed to obtain symbols";
  Symbolizer.create (Hashtbl.find names)

let register opts cmd =
  let symbolizer =
    Stream.merge Project.Info.arch Project.Info.file ~f:(fun arch file ->
        Or_error.try_with (fun () -> run_objdump cmd opts arch file)) in
  Symbolizer.Factory.register name symbolizer

let main cmd opts =
  let is_executable exe =
    try Some (FileUtil.which exe) with Not_found -> None in
  let command = List.find_map ~f:is_executable in
  match cmd with
  | Some s -> register opts s (* a specific path to objdump  given *)
  | None ->
    (* no specific path; lets try to infer, and return () even if *)
    (* we fail *)
    match command objdump_cmds >>| register opts with
    | _ -> ()

let path : string option Term.t =
  let doc = "Specify the path to objdump." in
  Arg.(value & opt (some file) None & info ["path"] ~doc)

let opts : string Term.t =
  let doc = "Specify objdump options. \
             Warning! We rely on *no* raw instructions, i.e., \
             --no-show-raw-insn, during parsing." in
  Arg.(value & opt string objdump_opts & info ["opts"] ~doc)

let info =
  let man = [
    `S "DESCRIPTION";
    `P "This plugin provides a symbolizer based on objdump. \
        Note that we parse objdump output, thus this symbolizer \
        is potentially fragile to changes in objdumps output.";
    `S "DIAGNOSTICS";
    `P  "This plugin makes several assumptions:";
    `P  "1) objdump does not output raw instructions. This is \
         currently fulfilled by the $(b,--no-show-raw-insn) default. \
         Beware changing this without checking that you are getting \
         the symbols you expect.";
    `P  "2) objdump output lines for symbols are left flush as \
         $(i,<number>: <name>) signify the start of a \
         $(i,name) at address $(i,number). There is no \
         command line option to change this currently.";
    `P  "If you get the wrong output but objdump is running, \
         you will likely need to edit the source (and submit a PR when\
         working!) and change $(b,func_start_re).";
    `S  "EXAMPLES";
    `P  "To view the symbols after running the plugin:";
    `P  "$(b, bap --symbolizer=objdump --dump-symbols) $(i,executable)";
    `P  "To use the internal extractor and *not* this plugin:";
    `P  "$(b, bap --symbolizer=internal --dump-symbols) $(i,executable)";
  ] in
  Term.info ~man ~doc name ~version

let () =
  let run = Term.(const main $path $opts) in
  match Term.eval ~argv ~catch:false (run, info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
