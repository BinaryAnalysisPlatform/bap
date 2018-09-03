open Core_kernel
open Bap.Std
open Regular.Std

include Self ()

(* variable is considred dead if it's a virtual and equal to const and
   assigned only once. Assuming const propagation was ran before *)
let eliminate_dead_code bil =
  let open Bil.Types in
  let assigned_once =
    fst @@
    ((object
      inherit [Var.Set.t * Var.Set.t] Stmt.visitor
      method! enter_move var _ (once, many) =
        if Set.mem once var then
          Set.remove once var, Set.add many var
        else if Set.mem many var then once,many
        else Set.add once var, many
    end)#run bil (Var.Set.empty, Var.Set.empty)) in
  let is_dead v = Var.is_virtual v && Set.mem assigned_once v in
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
  fun bil -> apply bil [
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
