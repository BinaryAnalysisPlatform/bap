open Bap_core_theory
open Core_kernel
open Bap_future.Std
open Bap.Std
include Self()

open KB.Syntax

let agent =
  KB.Agent.register ~package:"bap" "radare2-symbolizer"
    ~desc:"extracts symbols radare2"

let provide_roots file funcs =
  let promise_property slot =
    KB.promise slot @@ fun label ->
    KB.collect Theory.Label.addr label >>=? fun addr ->
    KB.collect Theory.Label.unit label >>=?
    KB.collect Theory.Unit.path >>|? fun path ->
    if String.equal path file then
      let addr = Bitvec.to_bigint addr in
      Option.some_if (Hashtbl.mem funcs addr) true
    else None in
  promise_property Theory.Label.is_valid;
  promise_property Theory.Label.is_subroutine

module Field = struct
  (* unfortunately, we can't use Yojson.*.Utils because they are
     undefined for the Yojson type *)

  let member name = function
    | `Assoc xs -> List.Assoc.find xs ~equal:String.equal name
    | _ -> invalid_arg "expects and assoc list"

  let string = function
    | `String x -> x
    | x -> invalid_argf "expects a string got %s"
             (Yojson.to_string x) ()

  let number = function
    | `Int i -> Z.of_int i
    | `Intlit s -> Z.of_string s
    | s -> invalid_argf "expected an address got %s"
             (Yojson.to_string s) ()

  let field name parse obj = match member name obj with
    | None -> invalid_argf "expected member %S in the object %s"
                name (Yojson.to_string obj) ()
    | Some field -> parse field

  let addr = field "vaddr" number
  let name = field "name" string
  let kind = field "type" string
end

let strip str =
  if String.is_prefix ~prefix:"func." str then None
  else Option.some @@ match String.chop_prefix str ~prefix:"imp." with
    | Some str -> str
    | None -> str

let provide_radare2 file =
  let funcs = Hashtbl.create (module struct
      type t = Z.t
      let compare = Z.compare and hash = Z.hash
      let sexp_of_t x = Sexp.Atom (Z.to_string x)
    end) in
  let accept name addr =  Hashtbl.set funcs addr name in
  let symbols = match R2.with_command_j "isj" file with
    | `List list -> Some list
    | s -> warning "unexpected radare2 output: %a" Yojson.pp s; None
    | exception Invalid_argument msg ->
      warning "failed to get symbols: %s" msg; None in
  Option.iter symbols ~f:(List.iter ~f:(fun s ->
      match Field.name s, Field.addr s, Field.kind s with
      | name,addr, "FUNC" -> accept (strip name) addr
      | _ -> debug "skipping json item %a" Yojson.pp s));
  if Hashtbl.length funcs = 0
  then warning "failed to obtain symbols";
  let symbolizer = Symbolizer.create @@ fun addr ->
    let addr = Bitvec.to_bigint (Word.to_bitvec addr) in
    match Hashtbl.find funcs addr with
    | Some name -> name
    | None -> None in
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
