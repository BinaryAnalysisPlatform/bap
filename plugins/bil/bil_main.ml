open Core_kernel
open Bap.Std
open Bil_const_propagation
include Self ()

module Memo = struct
  let max = 500
  let bils = String.Table.create ()
  let actual = Int.Table.create ()

  let min = ref 0
  let pnt = ref 0

  let digest_of_bil b = Digest.string (Bil.to_string b)

  let clean () =
    if !pnt > max then
      let d = Hashtbl.find_exn actual !min in
      Hashtbl.remove actual !min;
      Hashtbl.remove bils d;
      incr min

  (* todo: add in actual *)
  let add digest bil =
    Hashtbl.set bils digest bil;
    Hashtbl.set actual !pnt digest;
    incr pnt;
    clean ()

  let find = Hashtbl.find bils
end


let simpl = Bil.fold_consts
let norml = Stmt.normalize ~keep_ites:false ~normalize_exp:false
let propg = opropagate_consts

let apply bil =
  List.fold ~init:bil ~f:(fun bil f -> f bil)

let process pipe =
  let pipe = List.filter_map pipe
      ~f:(fun (test, f) -> Option.some_if test f) in
  fun bil ->
    let digest = Memo.digest_of_bil bil in
    match Memo.find digest with
    | Some bil -> bil
    | None ->
      let bil' = apply bil pipe in
      if Bil.compare bil bil' <> 0 then
        Memo.add digest bil';
      bil'

let run if_norml if_simpl =
  provide_bil_transformation "internal" @@
  process [
    if_simpl,simpl;
    if_norml,norml;
  ]

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
The following bil analysises are in default pipeline:
Constant Folding and Simplification
Bil Normalization
Constant Propagation
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let simpl =
    let doc = "Applies expressions simplification." in
    Config.(param int ~default:1 "simplification" ~doc) in
  let norm =
    let doc = "Produces a normalized BIL program" in
    Config.(param int ~default:1 "normalization" ~doc) in
  Config.when_ready (fun {Config.get=(!)} -> run !norm !simpl)
