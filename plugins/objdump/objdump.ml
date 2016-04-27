open Core_kernel.Std
open Re_perl
open Bap.Std
open Regular.Std
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

module Symbols = Data.Make(struct
    type t = (string * int64 ) list
    let version = version
  end)


(* expected format: [num] <[name]>:
   Note the use of "^\s" to stop greedy globing of the re "+"
   If you are not getting what you think you should for a start,
   this regular expression being wrong is a good place to look.
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

let run_objdump cmd opts file : symbolizer =
  let fullcmd = cmd ^ " " ^ opts ^ " " ^ file in
  let names = Addr.Table.create () in
  let ic = Unix.open_process_in fullcmd in
  let add name addr =
    Hashtbl.set names ~key:(Addr.of_int64 addr) ~data:name in
  In_channel.iter_lines ~f:(fun line -> match parse_func_start line with
                                     | None -> ()
                                     | Some (name,addr)
                                       -> add name addr) ic;
  In_channel.close ic;
  Symbolizer.create (Hashtbl.find names)

let register opts cmd =
  let symbolizer img =
    Image.filename img >>| run_objdump cmd opts
  in
  Symbolizer.Factory.register Source.Binary name symbolizer

let main cmd opts =
  let rec try_defaults cmds  =
    match cmds with
    | hd :: tl ->
      begin
        try FileUtil.which hd |>  register opts with
          Not_found -> try_defaults tl
      end
    | [] -> ()
  in
   match cmd with
       Some s ->
       (* a full path was specified. Note Cmdliner checks if the file *)
       (* exists for us *)
       register opts s
     | None -> (* no explicit option; let's search some defaults *)
       try_defaults objdump_cmds

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
        is potentially fragile to changes in objdumps output."
  ] in
  Term.info ~man ~doc name ~version

let () =
  let run = Term.(const main $path $opts) in
  match Term.eval ~argv ~catch:false (run, info) with
  | `Ok () -> ()
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
