open Bap_core_theory
open Core_kernel
open Bap_future.Std
open Bap.Std
include Self()

open KB.Syntax

let agent =
  KB.Agent.register ~package:"bap.std" "radare2-symbolizer"
    ~desc:"extracts symbols radare2"

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

exception Radare2_failed

let provide_radare2 file = 
  let funcs = Hashtbl.create (module struct
      type t = Z.t
      let compare = Z.compare and hash = Z.hash
      let sexp_of_t x = Sexp.Atom (Z.to_string x)
    end) in
    let accept name addr = Hashtbl.set funcs addr name in
    let extract name json = let open Yojson in
      match json with
      | `Assoc list -> 
        (match List.find list ~f:(fun (key, _) -> String.equal key name) with
        | Some (_, v) -> v
        | _ -> raise Radare2_failed)
      | _ -> raise Radare2_failed in
    try
     let symbol_list = let open Yojson in
      match R2.with_command_j "isj" file with
      | `List list -> list
      | _ -> raise Radare2_failed in
     let strip str = let open String in
      match chop_prefix str ~prefix:"sym.imp." with
      | Some str -> str
      | None -> str in
     let open Yojson in
     let to_string json = 
      match json with
      | `String str -> str
      | _ -> raise Radare2_failed in
     let to_int json = 
      match json with
      | `Int i -> Z.of_int i
      | `Intlit s -> Z.of_string s
      | _ -> raise Radare2_failed in
     List.fold symbol_list ~init:() ~f:(fun () symbol -> 
     accept (to_string (extract "name" symbol) |> strip) 
            (to_int (extract "vaddr" symbol))
     );
     if Hashtbl.length funcs = 0
     then warning "failed to obtain symbols";
     let symbolizer = Symbolizer.create @@ fun addr ->
     Hashtbl.find funcs @@
     Bitvec.to_bigint (Word.to_bitvec addr) in
     Symbolizer.provide agent symbolizer;
     provide_roots funcs
    with _ -> warning "failed to use radare2";()



let main = Stream.observe Project.Info.file @@ provide_radare2

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
  Config.when_ready (fun {get=_} -> main)