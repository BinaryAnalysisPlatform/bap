open Bap_core_theory
open Core_kernel
open Bap_future.Std
open Bap.Std
open Objdump_config
include Self()

open KB.Syntax

let default_objdump_opts = "-rd --no-show-raw-insn"

let objdump_cmds demangler=
  objdump ::
  List.map targets ~f:(fun p -> p^"-objdump") |>
  String.Set.stable_dedup_list |>
  List.map ~f:(fun cmd ->
      sprintf "%s %s %s" cmd default_objdump_opts @@
      match demangler with
      | Some "disabled" -> ""
      | None -> "-C"
      | Some other -> "--demangle="^other)

(* func_start ::=
    | addr,space, "<", name, ">", ":"
    | addr,space, "<", name, "@plt", ">", ":" *)
let parse_func_start =
  let parse =
    let func_start_re = {|([0-9A-Fa-f]+?) <(.*?)(@plt)?>:|} in
    Re.Pcre.re func_start_re |> Re.compile |> Re.exec in
  let parse_addr input ~start ~stop =
    Z.of_substring_base 16 input ~pos:start ~len:(stop - start) in
  fun input ~accept -> try
      let groups = parse input in
      let addr = parse_addr input
          ~start:(Re.Group.start groups 1)
          ~stop:(Re.Group.stop groups 1)
      and name = Re.Group.get groups 2 in
      info "%s => %s" (Z.format "%x" addr) name;
      accept name addr
    with _ -> ()

let run cmd ~f : _ Base.Continue_or_stop.t =
  let env = Unix.environment () in
  let stdin,stdout,stderr = Unix.open_process_full cmd env in
  In_channel.iter_lines stdin  ~f;
  match Unix.close_process_full (stdin,stdout,stderr) with
  | Unix.WEXITED 0 -> Stop ()
  | Unix.WEXITED n ->
    info "`%s' has failed with %d" cmd n;
    Continue ()
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    (* a signal number is internal to OCaml, so don't print it *)
    info "command `%s' was terminated by a signal" cmd;
    Continue ()

let with_objdump_output demangler ~file ~f =
  objdump_cmds demangler |>
  List.fold_until ~init:() ~f:(fun () objdump ->
      let cmd = sprintf "%s %S" objdump file in
      run cmd ~f)
    ~finish:ident

let agent =
  KB.Agent.register ~package:"bap.std" "objdump-symbolizer"

let provide_roots funcs =
  let promise_property slot =
    KB.promise slot @@ fun label ->
    KB.collect Theory.Label.addr label >>| function
    | None -> None
    | Some addr ->
      let addr = Bitvec.to_bigint addr in
      Option.some_if (Hashtbl.mem funcs addr) true in
  promise_property Theory.Label.is_valid;
  promise_property Theory.Label.is_subroutine

let provide_objdump demangler file =
  let funcs = Hashtbl.create (module struct
      type t = Z.t
      let compare = Z.compare and hash = Z.hash
      let sexp_of_t x = Sexp.Atom (Z.to_string x)
    end) in
  let accept name addr = Hashtbl.set funcs addr name in
  with_objdump_output demangler ~file ~f:(parse_func_start ~accept);
  if Hashtbl.length funcs = 0
  then warning "failed to obtain symbols";
  let symbolizer = Symbolizer.create @@ fun addr ->
    Hashtbl.find funcs @@
    Bitvec.to_bigint (Word.to_bitvec addr) in
  Symbolizer.provide agent symbolizer;
  provide_roots funcs

let main demangler =
  Stream.observe Project.Info.file @@
  provide_objdump demangler

let () =
  let demangler =
    let doc = "Specify the demangler name. \
               Set to $(i,disabled) to disable demangling." in
    Config.(param ~doc (some string) "demangler") in
  Config.manpage [
    `S "DESCRIPTION";
    `P "This plugin provides a symbolizer based on objdump. \
        Note that we parse objdump output, thus this symbolizer \
        is potentially fragile to changes in objdumps output.";
    `S  "EXAMPLES";
    `P  "To view the symbols after running the plugin:";
    `P  "$(b, bap) $(i,executable) --dump-symbols ";
    `P  "To view symbols without this plugin:";
    `P  "$(b, bap) $(i,executable) --no-objdump --dump-symbols";
    `S  "SEE ALSO";
    `P  "$(b,bap-plugin-ida)(1)"
  ];
  Config.when_ready (fun {get=(!!)} -> main !!demangler)
