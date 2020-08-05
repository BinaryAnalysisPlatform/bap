open Bap_core_theory
open Core_kernel
open Bap_future.Std
open Bap.Std
include Self()

open KB.Syntax

let agent =
  KB.Agent.register ~package:"bap" "radare2-symbolizer"
    ~reliability:KB.Agent.doubtful
    ~desc:"extracts symbols radare2"

let provide_roots file funcs =
  let promise_property slot =
    KB.promise slot @@ fun label ->
    KB.collect Theory.Label.addr label >>=? fun addr ->
    KB.collect Theory.Label.unit label >>=? fun unit ->
    KB.collect Theory.Unit.bias unit >>= fun bias ->
    KB.collect Theory.Unit.Target.bits unit >>=? fun bits ->
    KB.collect Theory.Unit.path unit >>|? fun path ->
    if String.equal path file then
      let bias = Option.value bias ~default:Bitvec.zero in
      let addr =
        Bitvec.to_bigint @@
        Bitvec.((addr - bias) mod modulus bits) in
      Option.some_if (Hashtbl.mem funcs addr) true
    else None in
  promise_property Theory.Label.is_valid;
  promise_property Theory.Label.is_subroutine

let strip str =
  if String.is_prefix ~prefix:"func." str then None
  else Option.some @@ match String.chop_prefix str ~prefix:"imp." with
    | Some str -> str
    | None -> str

let to_zarith = function
  | `Int i -> Z.of_int i
  | `Intlit s -> Z.of_string s
  | s -> invalid_argf "expected an address got %s"
           (Yojson.Safe.to_string s) ()

let parse =
  let open Yojson.Safe.Util in
  convert_each @@ fun x ->
  to_string @@ member "name" x,
  to_zarith @@ member "vaddr" x,
  to_string @@ member "type" x

let extract_symbols file =
  let cmd = sprintf "radare2 -2 -q -cisj %s" file in
  let input = Unix.open_process_in cmd in
  let out = try parse@@Yojson.Safe.from_channel input with
    | exn ->
      warning "failed to extract symbols: %s" (Exn.to_string exn);
      [] in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 -> out
  | WEXITED n ->
    warning "radare2 failed with the exit code %d" n;
    out
  | WSIGNALED _ | WSTOPPED _ ->
    warning "radare2 was interrupted with a signal";
    out

let string_of_addr addrs =
  List.map addrs ~f:(Z.format "0x%x") |>
  String.concat ~sep:", "

let report_missing = function
  | Bap_relation.Non_injective_fwd (addrs,name) ->
    info "skipping (%s), as they all have the same name %s"
      (string_of_addr addrs) name
  | Bap_relation.Non_injective_bwd (names,addr) ->
    info "skipping (%s), as they all have the same address %s"
      (String.concat names ~sep:", ") (Z.format "0x%x" addr)

let provide_radare2 file =
  let funcs = Hashtbl.create (module struct
      type t = Z.t
      let compare = Z.compare and hash = Z.hash
      let sexp_of_t x = Sexp.Atom (Z.to_string x)
    end) in
  let rels =
    let init = Bap_relation.empty Z.compare String.compare in
    List.fold ~init (extract_symbols file) ~f: (fun rels (name,addr,typ) ->
        if typ = "FUNC" then match strip name with
          | None -> rels
          | Some name -> Bap_relation.add rels addr name
        else rels) in
  Bap_relation.matching rels ()
    ~saturated:(fun addr name () -> Hashtbl.add_exn funcs addr name)
    ~unmatched:(fun reason () -> report_missing reason);
  if Hashtbl.length funcs = 0
  then warning "failed to obtain symbols";
  let symbolizer = Symbolizer.create @@ fun addr ->
    let addr = Bitvec.to_bigint (Word.to_bitvec addr) in
    Hashtbl.find funcs addr in
  let symbolizer = Symbolizer.set_path symbolizer file in
  Symbolizer.provide agent symbolizer;
  provide_roots file funcs

let main () = Stream.observe Project.Info.file @@ provide_radare2

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "This plugin provides a symbolizer based on radare2.";
    `S  "EXAMPLES";
    `P  "To view the symbols after running the plugin:";
    `P  "$(b, bap) $(i,executable) --dump-symbols ";
    `P  "To view symbols without this plugin:";
    `P  "$(b, bap) $(i,executable) --no-radare2 --dump-symbols";
    `S  "SEE ALSO";
    `P  "$(b,bap-plugin-objdump)(1)"
  ];
  Config.when_ready (fun {get=_} -> main ())
