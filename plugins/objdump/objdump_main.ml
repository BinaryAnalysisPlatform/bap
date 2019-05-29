open Core_kernel
open Bap_future.Std
open Bap.Std
open Regular.Std
open Format
open Option.Monad_infix
open Objdump_config
include Self()

let objdump_opts = "-rd --no-show-raw-insn"

let objdump_cmds =
  objdump ::
  List.map targets ~f:(fun p -> p^"-objdump") |>
  String.Set.stable_dedup_list |>
  List.map ~f:(fun cmd -> cmd ^ " " ^ objdump_opts)


(* expected format: [num] <[name]>:
   Note the use of "^\s" to stop greedy globing of the re "+"
   If you are not getting what you think you should,
   this regular expression is a good place to start with debugging.
*)
let func_start_re = "([0-9A-Fa-f^\\s]+) <(.*)>:"

let re r =
  Re_pcre.re r |> Re.compile |> Re.execp
[@@warning "-D"]

let objdump_strip  =
  String.strip ~drop:(function '<' | '>' | ':' | ' ' -> true | _ -> false)

let text_to_addr l =
  objdump_strip l |> (^) "0x" |> Int64.of_string

let is_section_start s =
  String.is_substring s ~substring:"Disassembly of section"

(** "Disassembly of section .fini:" -> ".fini" *)
let section_name s =
  match String.split_on_chars ~on:[' '; ':'] s with
  | _ :: _ :: _ :: name :: _ -> Some name
  | _ -> None

let parse_func_start section l =
  if re func_start_re l then
    let xs = String.split_on_chars ~on:[' '; '@'] l in
    match xs with
    | addr::name::[]  (* name w/o @plt case *)
    | addr::name::_::[] -> (* name@plt case *)
      let name = objdump_strip name in
      if Some name = section then None
      else
        Some (name, text_to_addr addr)
    | _ -> None
  else None

let popen cmd =
  let env = Unix.environment () in
  let ic,oc,ec = Unix.open_process_full cmd env in
  let r = In_channel.input_lines ic in
  In_channel.iter_lines ec ~f:(fun msg -> debug "%s" msg);
  match Unix.close_process_full (ic,oc,ec) with
  | Unix.WEXITED 0 -> Some r
  | Unix.WEXITED n ->
    info "command `%s' terminated abnormally with exit code %d" cmd n;
    None
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    (* a signal number is internal to OCaml, so don't print it *)
    info "command `%s' was terminated by a signal" cmd;
    None

let provide_symbolizer arch file =
  let popen = fun cmd -> popen (cmd ^ " " ^ file) in
  let names = Addr.Table.create () in
  let width = Arch.addr_size arch |> Size.in_bits in
  let add (name,addr) =
    Hashtbl.set names ~key:(Addr.of_int64 ~width addr) ~data:name in
  let () = match List.find_map objdump_cmds ~f:popen with
    | None -> ()
    | Some lines ->
      List.fold ~init:None lines ~f:(fun sec line ->
          if is_section_start line then section_name line
          else
            let () = Option.iter (parse_func_start sec line) ~f:add in
            sec) |> ignore in
  if Hashtbl.length names = 0
  then warning "failed to obtain symbols";
  let s = Symbolizer.create (Hashtbl.find names) in
  Symbolizer.provide s

let main () =
  let inputs = Stream.zip Project.Info.arch Project.Info.file in
  Stream.observe inputs @@ fun (arch,file) ->
  provide_symbolizer arch file

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "This plugin provides a symbolizer based on objdump. \
        Note that we parse objdump output, thus this symbolizer \
        is potentially fragile to changes in objdumps output.";
    `S  "EXAMPLES";
    `P  "To view the symbols after running the plugin:";
    `P  "$(b, bap --symbolizer=objdump --dump-symbols) $(i,executable)";
    `P  "To use the internal extractor and *not* this plugin:";
    `P  "$(b, bap --symbolizer=internal --dump-symbols) $(i,executable)";
    `S  "SEE ALSO";
    `P  "$(b,bap-plugin-ida)(1)"
  ];
  Config.when_ready (fun _ -> main ())
