open Core_kernel
open Bap.Std

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

  let add digest bil =
    Hashtbl.set bils digest bil;
    Hashtbl.set actual !pnt digest;
    incr pnt;
    clean ()

  let find = Hashtbl.find bils
end

let eliminate_dead_code bil =
  let open Bil.Types in
  let free = Bil.free_vars bil in
  let is_dead v = Var.is_virtual v && not (Set.mem free v) in
  let rec loop acc = function
    | [] -> List.rev acc
    | st :: bil ->
      match st with
      | Move (var, Int _) when is_dead var -> loop acc bil
      | If (cond, yes, no) ->
	let yes = loop [] yes in
	let no  = loop [] no in
	loop (If (cond, yes, no) :: acc) bil
      | While (cond,body) ->
        let body = loop [] body in
        loop (While (cond,body) :: acc) bil
      | st -> loop (st :: acc) bil in
  loop [] bil

let apply bil =
  List.fold ~init:bil ~f:(fun bil f -> f bil)

let process pipe =
  fun bil ->
    let digest = Memo.digest_of_bil bil in
    match Memo.find digest with
    | Some bil -> bil
    | None ->
      let bil' = apply bil pipe in
      if Bil.compare bil bil' <> 0 then
        Memo.add digest bil';
      bil'

let norml level =
  if level = 0 then ident
  else
    Stmt.normalize ~keep_ites:false ~normalize_exp:false

let simpl level =
  if level = 0 then ident
  else
    Bil.fixpoint
      (fun bil -> Bil.fold_consts bil |> Bil_const_propagation.run |> eliminate_dead_code)

let run norml_level simpl_level =
  provide_bil_transformation "internal" @@
  process [
    norml norml_level;
    simpl simpl_level;
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
