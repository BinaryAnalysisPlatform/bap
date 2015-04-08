open Core_kernel.Std
open Bap_types.Std
open Bil.Types

module Seq = Sequence

exception Abort of string


(** Given 8*n, return n.
  * useful for operating on memory. *)
let bits_to_bytes n =
  if n mod 8 <> 0 then raise (Abort "Width should be multiple of 8.")
  else n / 8

module Mem = Addr.Map

module T = struct
  type t =
    | BV of Bitvector.t
    | Mem of memory
    | Un of string * typ
  and memory = t Mem.t
  with bin_io, compare, sexp

  let module_name = "Bap_conceval"
  let hash = Hashtbl.hash

  open Format
  let rec pp fmt = function
    | Un (n,typ) -> fprintf fmt "Un[%s]:%a" n Type.pp typ
    | BV v -> Bitvector.pp fmt v
    | Mem m ->
      fprintf fmt "@[<v0>%a@]" pp_list (Mem.to_alist m)
  and pp_list fmt = function
    | [] -> ()
    | (a,v) :: ms ->
      fprintf fmt "%a: %a@;%a" Addr.pp a pp v pp_list ms
end

include T

type value = T.t
with bin_io, compare, sexp

(** Represents memory.
  * Supports: load and store byte sequences. *)
module Memory = struct
  type t = memory
  let empty = Mem Mem.empty
  let load ~mem ~idx endianness sz = match mem, idx with
    | (Mem mem, BV idx) ->
      if Mem.is_empty mem then None
      else
        let bytes = Size.to_bytes sz in
        let max = Addr.(idx ++ Int.(bytes - 1)) in
        let data =
          List.map ~f:snd (Mem.range_to_alist mem ~min:idx ~max) in
        if List.length data = bytes then
          let data = match endianness with
            | LittleEndian -> List.rev data
            | BigEndian ->  data in
          List.fold data ~init:None
            ~f:(fun a b -> match (a, b) with
                | None, a  -> Some a
                | Some (BV a), (BV b) -> Some (BV Word.(a @. b))
                | _, other -> Some other)
        else None
    | (Mem _, (Un (_, _) as un)) -> Some un
    | (_, _) -> raise (Abort "Memory or index has wrong type.")


  let store ~mem ~idx ~data:v endianness sz = match mem, idx, v with
    | (Mem mem, BV idx, BV v) ->
      let v = Word.to_bytes v endianness in
      let (ret, _) = Seq.fold v ~init:(mem, idx)
          ~f:(fun (mem, i) byte ->
              (Mem.add mem ~key:i ~data:(BV byte)),
              Word.(i ++ 1))
      in
      Mem ret
    | (Mem mem, BV idx, (Un (_, _) as un)) ->
      let ret = ref mem in
      let ii = ref idx in
      for i = 1 to Size.to_bits sz do
        ret := Mem.add !ret ~key:!ii ~data:un ;
        ii :=  Word.(!ii ++ 1);
      done;
      Mem !ret
    | (Mem mem, Un (_, _), _) ->
      print_endline "Warning: writing to unknown memory index";
      Mem mem
    | (_, _, _) -> raise (Abort "Memory, index, or value has wrong type.")
end

module State = struct
  module StateMap = Var.Map
  type t = T.t StateMap.t
  with bin_io, compare, sexp

  let empty = StateMap.empty
  let move = StateMap.add
  let peek_exn = StateMap.find_exn
  let peek = StateMap.find

  (** Remove all temporary variables from a state. *)
  let remove_tmp state =
    StateMap.filter state ~f:(fun ~key:k ~data:_ -> not (Var.is_tmp k))
end

type state = State.t
with bin_io, compare, sexp

(** If v is a bitvector, perform some action on it.
  * Otherwise, handle the other value. *)
let bv_action_or_unknown v action =
  match v with
  | Mem _ -> raise (Abort "Operation cannot be performed on memory.")
  | Un (a, b) -> Un (a, b)
  | BV v -> action v


module Z = Bitvector.Int_exn

(** Handle a unary operator. *)
let handle_unop op v =
  bv_action_or_unknown v
    (fun v -> match op with
       | NEG -> BV (Z.neg v)
       | NOT -> BV (Z.lnot v))



(** Handle a binary operator. *)
let handle_binop op l r : value =
  match (l, r) with
  | (Mem _, _) | (_, Mem _) ->
    raise (Abort "Operation cannot be performed on memory.")
  | (Un (a, b), _) | (_, Un (a, b)) -> Un (a, b)
  | (BV l, BV r) ->
    let lift_bool op x y =
      if (op x y) then Z.one else Z.zero in
    let signed op x y = op (Word.signed x) y in
    let op = match op with
      | PLUS    -> Z.add
      | MINUS   -> Z.sub
      | TIMES   -> Z.mul
      | DIVIDE  -> Z.div
      | SDIVIDE -> signed Z.div
      | MOD     -> Z.modulo
      | SMOD    -> signed Z.modulo
      | LSHIFT  -> Z.lshift
      | RSHIFT  -> Z.rshift
      | ARSHIFT -> Z.arshift
      | AND     -> Z.logand
      | OR      -> Z.logor
      | XOR     -> Z.logxor
      | EQ      -> lift_bool Bitvector.(=)
      | NEQ     -> lift_bool Bitvector.(<>)
      | LT      -> lift_bool Bitvector.(<)
      | LE      -> lift_bool Bitvector.(<=)
      | SLT     -> lift_bool Bitvector.(<=)
      | SLE     -> lift_bool Bitvector.(<=) in
    BV (op l r)

let handle_cast cast_kind size v =
  let hi = size - 1 in
  let cast v = match cast_kind with
    | UNSIGNED -> Word.extract_exn ~hi v
    | SIGNED   -> Word.extract_exn ~hi (Word.signed v)
    | HIGH     -> Word.extract_exn ~lo:(Word.bitwidth v - size) v
    | LOW      -> Word.extract_exn ~hi v in
  bv_action_or_unknown v (fun v -> BV (cast v))

(** Given state, evaluate a single BIL expression. *)
let rec eval_exp state exp =
  let result = match exp with
    | Load (arr, idx, endian, t) ->
      (match Memory.load (eval_exp state arr) (eval_exp state idx) endian t with
       | Some v -> v
       | None -> Un ("Load from uninitialized memory",
                     Type.imm Size.(to_bits t)))
    | Store (arr, idx, v, endian, t) ->
      Memory.store (eval_exp state arr) (eval_exp state idx) (eval_exp state v)
        endian t
    | BinOp (op, l, r) -> handle_binop op (eval_exp state l) (eval_exp state r)
    | UnOp (op, v) -> handle_unop op (eval_exp state v)
    | Var v -> State.peek_exn state v
    | Int v -> BV v
    | Cast (cast_kind, new_type, v) ->
      handle_cast cast_kind new_type (eval_exp state v)
    | Let (v, a, b) -> (* FIXME Should there be typechecking done here? *)
      let state = State.move  state ~key:v ~data:(eval_exp state a) in
      eval_exp state b
    | Unknown (str, typ) -> Un (str, typ)
    | Ite (cond, t_case, f_case) ->
      bv_action_or_unknown (eval_exp state cond)
        (fun v ->
           if not (Word.is_zero v) then eval_exp state t_case
           else eval_exp state f_case)
    | Extract (hi, lo, v) -> bv_action_or_unknown (eval_exp state v)
                               (fun v -> BV (Word.extract_exn ~hi ~lo v))
    | Concat (l, r) -> (match eval_exp state l, eval_exp state r with
        | (Mem _, _) | (_, Mem _) ->
          raise (Abort "Operation cannot be performed on memory.")
        | ((Un (_, _) as un), _) | (_, (Un (_, _) as un)) -> un
        | (BV l, BV r) -> BV (Bitvector.concat l r))
  in result

(** Take a detailed state and a BIL statement and yield the successor state and
  * a location to jump to, if any. *)
let rec eval_stmt state =
  let open Stmt in function
    | Move (v, exp) ->
      (State.move ~key:v ~data:(eval_exp state exp) state), None
    | Jmp (exp) -> state, Some (eval_exp state exp)
    | While (cond, stmts) ->
      (match eval_exp state cond with
       | Mem _ -> raise (Abort "Operation cannot be performed on memory.")
       | Un (a, b) -> raise (Abort "Condition in While loop is Unknown.")
       | BV v ->
         if not(Word.is_zero v) then
           let (state, addr) = (eval_stmt_list state stmts) in
           (match addr with
            | None -> eval_stmt state (While (cond, stmts))
            | Some addr as jump_to -> (state, jump_to))
         else (state, None))
    | If (cond, t_case_stmts, f_case_stmts) ->
      (match eval_exp state cond with
       | Mem _ ->
         raise (Abort "Operation cannot be performed on memory.")
       | Un (_, _) -> raise (Abort "Condition in If statement is Unknown.")
       | BV v -> eval_stmt_list state
                   (if not(Bitvector.is_zero v) then t_case_stmts
                    else f_case_stmts))
    | Special str ->
      raise (Abort (Printf.sprintf "Aborting with Special '%s'" str))
    | CpuExn i -> raise (Abort (Printf.sprintf "Aborting with CpuExn %d" i))
(** Helper function:
  * evaluate a list of BIL statements from a starting state. *)
and eval_stmt_list state = function
  | [] -> state, None
  | (hd::tl) -> let (state, addr) = eval_stmt state hd in
    (match addr with
     | None -> eval_stmt_list state tl
     | Some _ as jump_to -> (state, jump_to))

(** Evaluate a list of instructions and discard temporary state,
  * as when evaluating an assembly instruction. *)
let eval_stmts state instructions =
  let (state, addr) = eval_stmt_list state instructions in
  (State.remove_tmp state, addr)

include Regular.Make(T)
