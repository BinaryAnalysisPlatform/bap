open Core_kernel
open Bap.Std
open Regular.Std

include Self ()

let collect_def_use bil =
  (object
    inherit [Var.Set.t * Var.Set.t] Stmt.visitor
    method! enter_move var _ (def, used) = Set.add def var, used
    method! enter_stmt s (def, used) =
      def, Set.union used (Stmt.free_vars s)
  end)#run bil (Var.Set.empty, Var.Set.empty)

let eliminate_dead_code bil =
  let open Bil.Types in
  let def, used = collect_def_use bil in
  let dead = Set.diff def used in
  let is_dead var = Var.is_virtual var && Set.mem dead var in
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

let () =
  Bil.register_pass "normalization" @@
  Stmt.normalize ~keep_ites:false ~normalize_exp:false

let () =
  Bil.register_pass "optimization" @@
  Bil.fixpoint
    (fun bil -> Bil.fold_consts bil |> Bil_const_propagation.run |> eliminate_dead_code)

let run passes =
  let passes = ["normalization"; "optimization"] @ passes in
  let is_selected p =
    List.mem passes ~equal:String.equal (Bil.Pass.to_string p) in
  let all = Bil.passes () in
  List.filter all ~f:is_selected |>
  Bil.select_passes

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Establishes a pipeline of BIL analysis";
      `P "The pipeline is a list of transformations, applicable to
        a BIL code. And a BIL code of any instruction is an
        output of this pipeline, i.e. a result of the last
        applied transformation.
        By default, pipeline begins with BIL normalization and
        few optimizations, that include constant folding,
        constant propagation and dead code elimination.
        New analysis could be added with $(b,Bil.register_pass)
        and selected later by $(b,-passes) option. Note, that the order
        does matter, so";
      `Pre
"$(b,bap) exe --$(mname)$(b,-passes)=foo,bar
and
$(b,bap) exe --$(mname)$(b,-passes)=bar,foo
may produce different results";
    ] in
  let passes =
    let doc =
      "Selects the list and the order of analyses to be applied during
       the lifing to BIL code." in
    Config.(param (list string) ~default:[] ~doc "passes") in
  Config.when_ready (fun {Config.get=(!)} -> run !passes)
