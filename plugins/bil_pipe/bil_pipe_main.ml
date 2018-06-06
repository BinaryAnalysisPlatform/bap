open Core_kernel
open Bap.Std
open Bil_const_propagation

include Self ()

let numerator () =
  let num = ref 0 in
  fun typ ->
    let name = sprintf "v%d" !num in
    incr num;
    Var.create ~is_virtual:true name typ

let virtuals bil =
  let num = numerator () in
  let add_if_virtual v vs =
    if Map.mem vs v || Var.is_physical v then vs
    else Map.add vs v (num (Var.typ v)) in
  (object
    inherit [var Var.Map.t] Stmt.visitor as super

    method! enter_exp e vs = match e with
      | Let (v,x,y) ->
        add_if_virtual v vs |>
        super#enter_exp x |>
        super#enter_exp y
      | e -> super#enter_exp e vs

    method! enter_move v e vs =
      add_if_virtual v vs |>
      super#enter_exp e

  end)#run bil Var.Map.empty

class renumerator vars =
  let renum v = Option.value ~default:v (Map.find vars v) in
  object
    inherit Stmt.mapper as super

    method! map_let v ~exp ~body =
      Let (renum v, super#map_exp exp, super#map_exp body)

    method! map_var v = Var (renum v)

    method! map_stmt s = match s with
      | Move (v, e) -> [Move (renum v, super#map_exp e)]
      | s -> super#map_stmt s
  end

let renum bil =
  let vs = virtuals bil in
  Stmt.map (new renumerator vs) bil

type switch = Enable | Disable

let parse = function
  | "enable" -> `Ok Enable
  | "disable" -> `Ok Disable
  | s ->
    `Error (sprintf "unknown value %s, possible are: disable | enable" s)

let print fmt s =
  Format.fprintf fmt "%s" @@
  match s with
  | Enable  -> "enable"
  | Disable -> "disable"

let switcher ?(default=Enable) =
  Config.(param (converter parse print Enable) ~default)

let switch f s = Option.some_if (s = Enable) f

let add norml simpl propg =
  let norml = switch (Stmt.normalize ~normalize_exp:false) norml in
  let simpl = switch Bil.fold_consts simpl in
  let propg = switch propagate_copy propg in
  let apply bil = function
    | None -> bil
    | Some f -> f bil in
  let (>>=) = apply in
  register_bass "internal" @@
  fun bil -> Ok (apply bil norml >>= simpl >>= propg)

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P "Applies analysises to a instruction bil code" ;
      `Pre "
The following bil analysises are in default pipeline:
Bil Normalization
Constant Folding
Constant Propagation
";
      `S "SEE ALSO";
      `P "$(b,bap.mli)"
    ] in
  let norm =
    switcher "norm" ~doc:"Produces a normalized BIL program" in
  let simpl =
    switcher "simpl" ~doc:"Applies expressions simplification." in
  let prop =
    switcher "prop" ~doc:"Const propagation" in
  Config.when_ready (fun {Config.get=(!)} -> add !norm !simpl !prop)
