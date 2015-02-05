open Core_kernel.Std
open Bap.Std
open Options

module Make(Env : Printing.Env) = struct
  module Printing = Printing.Make(Env)
  open Env
  open Printing

  (* all let bindings create fresh new variables,
     so we needn't worry about shadowing. *)
  let rec is_bound x = List.exists ~f:(stmt x)
  and stmt x = Stmt.(function
      | Move (y,e) -> Var.(x = y) || expr x e
      | While (e,bil) -> is_bound x bil || expr x e
      | If (e,b1,b2) -> is_bound x b1 || is_bound x b2 || expr x e
      | Jmp _ | Special _ | CpuExn _ -> false)
  and expr x = Exp.(function
      | Var y -> Var.(x = y)
      | Int _ | Unknown _ -> false
      | UnOp (_,e) | Extract (_,_,e) | Cast (_,_,e) -> expr x e
      | Load (e1,e2,_,_) | BinOp (_,e1,e2)
      | Let (_,e1,e2) | Concat (e1,e2) -> expr x e1 || expr x e2
      | Ite (e1,e2,e3) | Store (e1,e2,e3,_,_) ->
        List.exists [e1;e2;e3] ~f:(expr x))

  let remove_dead_variables stmt =
    let rec loop ss = function
      | [] -> List.rev ss
      | Stmt.Move (x,_) as s :: xs when Var.is_tmp x ->
        if is_bound x xs then loop (s::ss) xs else loop ss xs
      | s :: xs -> loop (s::ss) xs in
    loop [] stmt

  let jump_type = match Arch.addr_size arch with
    | `r32 -> reg32_t
    | `r64 -> reg64_t

  let resolve_jumps bil =
    let fn name = Exp.var (Var.create name jump_type) in
    let resolve_addr addr =
      match Table.find_addr cfg addr with
      | Some (_,blk) ->
        let name = Format.asprintf "%a" pp_blk_name blk in
        Exp.(fn name)
      | None -> Exp.Int addr in
    let open Stmt in
    let rec resolve bil =
      List.map bil ~f:(function
          | Jmp (Exp.Int addr) -> Jmp (resolve_addr addr)
          | Jmp _ as jmp -> jmp
          | While (e,bil) -> While (e, resolve bil)
          | If (e,b1,b2) -> If (e,resolve b1, resolve b2)
          | Move _ | Special _ | CpuExn _ as s -> s) in
    resolve bil

  let resolve_jumps =
    if options.target_format = `numeric then ident else resolve_jumps

  let remove_dead_variables =
    if options.keep_alive then ident else remove_dead_variables

  let bil_of_insns insns =
    let bs = Seq.(insns >>| Insn.bil |> to_list) in
    List.(bs >>| remove_dead_variables >>| resolve_jumps |> concat)

  let bil_of_block blk : bil =
    bil_of_insns Seq.(Block.insns blk >>| snd)
end
