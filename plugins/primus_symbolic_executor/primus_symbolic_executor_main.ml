open Core_kernel
open Bap.Std
open Bap_primus.Std

(* Algorithm.

   For each system, just when we start, we create the init machine
   that will be used to fork all other machines and re-execute the
   path N times, where N is then number of linearly independent paths
   that are reachable from the entry point. The init machine spawns
   a new worker for each model in its queue. We start with an initial
   empty model, that starts the pioneer machine.

   Each worker (including the pioneer) runs from the entry point and
   builds the path formular. With each value we associate a
   formula, which we optimize on the fly. We designate certain values
   as inputs and keep them symbolic. When we hit the branch that we
   didn't see before and that leads to the desitantion which we
   neither saw nor plan to see, we take the formula associated with
   that branch condition and switch back to the init machine, and push
   its negation to the queue.

   When a worker finishes, the control is passed back to the init
   machine that, first, clears the queue from formulae that are not
   actual anymore, and then, pick then pick the first actual formula
   finds a satisfying model for it, and spawns a new worker process,
   that works as above.

   To identify which formula is actual, we associate with each
   formula the branch destination that will be taken if the formula
   is satisfiable. A formula is actual if this destination wasn't
   visited by a worker before. An unresolved destination is treated
   as unvisited, therefore it is always actual.

*)


module Input : sig
  type t
  val ptr : addr -> t
  val var : var -> t
  val to_symbol : t -> string
  val size : t -> int
  include Base.Comparable.S with type t := t
end = struct
  type t = Ptr of word | Var of var [@@deriving compare, sexp]

  let ptr p = Ptr p
  let var p = Var p

  let to_symbol = function
    | Var v -> Format.asprintf "R_%a" Var.pp v
    | Ptr p -> Format.asprintf "M_%a" Addr.pp_hex p

  let size = function
    | Ptr _ -> 8
    | Var v -> match Var.typ v with
      | Imm m -> m
      | _ -> 1

  include Base.Comparable.Make(struct
      type nonrec t = t [@@deriving compare, sexp]
    end)
end

module Formula : sig
  type t
  type formula = t
  type model

  val word : Word.t -> t
  val var : string -> int -> t

  val binop : binop -> t -> t -> t
  val unop : unop -> t -> t
  val extract : int -> int -> t -> t
  val concat : t -> t -> t
  val ite : t -> t -> t -> t
  val cast : cast -> int -> t -> t

  val solve : ?inverse:bool -> t -> model option

  module Model : sig
    type t = model
    val get : model -> Input.t -> formula option
  end

  val to_string : t -> string
end = struct
  module Bitv = Z3.BitVector
  module Bool = Z3.Boolean
  module Expr = Z3.Expr
  module Sort = Z3.Sort

  type t = Expr.expr
  type formula = t

  let ctxt = Z3.mk_context [
      "model", "true";
    ]

  let to_string = Expr.to_string

  let coerce_shift op ctxt x y =
    let sx = Bitv.get_size (Expr.get_sort x)
    and sy = Bitv.get_size (Expr.get_sort y) in
    if sx = sy then op ctxt x y
    else
      let y = Bitv.mk_zero_ext ctxt (sx-sy) y in
      op ctxt x y


  let z3_of_binop : binop -> _ = function
    | PLUS -> Bitv.mk_add
    | MINUS -> Bitv.mk_sub
    | TIMES -> Bitv.mk_mul
    | DIVIDE -> Bitv.mk_udiv
    | SDIVIDE -> Bitv.mk_sdiv
    | MOD -> Bitv.mk_srem
    | SMOD -> Bitv.mk_urem
    | LSHIFT -> coerce_shift Bitv.mk_shl
    | RSHIFT -> coerce_shift Bitv.mk_lshr
    | ARSHIFT -> coerce_shift Bitv.mk_ashr
    | AND -> Bitv.mk_and
    | OR -> Bitv.mk_or
    | XOR -> Bitv.mk_xor
    | LT -> Bitv.mk_ult
    | LE -> Bitv.mk_ule
    | SLT -> Bitv.mk_slt
    | SLE -> Bitv.mk_sle
    | EQ -> fun ctxt x y ->
      Bitv.mk_not ctxt @@
      Bitv.mk_redor ctxt @@
      Bitv.mk_xor ctxt x y
    | NEQ -> fun ctxt x y ->
      Bitv.mk_redor ctxt @@
      Bitv.mk_xor ctxt x y

  let do_simpl x = Expr.simplify x None

  let bit0 = Expr.mk_numeral_int ctxt 0 (Bitv.mk_sort ctxt 1)
  let bit1 = Expr.mk_numeral_int ctxt 1 (Bitv.mk_sort ctxt 1)

  let is_bool = Bool.is_bool

  let bit_of_bool x =
    if Bool.is_bool x
    then do_simpl @@ Bool.mk_ite ctxt x bit1 bit0
    else x

  let z3_of_unop : unop -> _ = function
    | NEG -> Bitv.mk_neg
    | NOT -> Bitv.mk_not

  let coerce_to_bit_if_necessary x =
    if is_bool x then bit_of_bool x
    else x

  let simpl x = Expr.simplify x None |>
                coerce_to_bit_if_necessary

  let binop op x y = simpl (z3_of_binop op ctxt x y)
  let unop op x = simpl (z3_of_unop op ctxt x)

  (* let binop op x y =
   *   Format.eprintf "(%s %s:%s %s:%s)@\n%!"
   *     (Bil.string_of_binop op)
   *     (to_string x) (Z3.Sort.to_string (Expr.get_sort x))
   *     (to_string y) (Z3.Sort.to_string (Expr.get_sort y));
   *   binop op x y
   *
   * let unop op x =
   *   Format.eprintf "(%s %s:%s)@\n%!"
   *     (Bil.string_of_unop op)
   *     (to_string x) (Z3.Sort.to_string (Expr.get_sort x));
   *   unop op x *)

  let word x =
    let s = Bitv.mk_sort ctxt (Word.bitwidth x) in
    let x = Word.to_bitvec x in
    if Bitvec.fits_int x
    then Expr.mk_numeral_int ctxt (Bitvec.to_int x) s
    else
      let x = Bitvec.to_bigint x in
      Expr.mk_numeral_string ctxt (Z.to_string x) s

  let extract hi lo x =
    simpl @@ Bitv.mk_extract ctxt hi lo x

  let concat x y = simpl @@ Bitv.mk_concat ctxt x y

  let ite c x y = simpl @@ Bool.mk_ite ctxt c x y

  let cast (c:cast) s x =
    let old = Bitv.get_size (Expr.get_sort x) in
    simpl @@ match c with
    | SIGNED -> if old < s
      then Bitv.mk_sign_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | UNSIGNED -> if old < s
      then Bitv.mk_zero_ext ctxt (s-old) x
      else extract (s-1) 0 x
    | HIGH -> extract (old-1) (old-s) x
    | LOW -> extract (s-1) 0 x

  let var name size =
    let sort = match size with
      | 1 -> Bool.mk_sort ctxt
      | n -> Bitv.mk_sort ctxt n in
    Expr.mk_const_s ctxt name sort

  type model = {
    model : Z3.Model.model;
  }

  let solve ?(inverse=false) expr =
    let solver = Z3.Solver.mk_simple_solver ctxt in
    let lhs = Expr.mk_numeral_int ctxt 0 (Expr.get_sort expr) in
    let formula = Bool.mk_eq ctxt lhs expr in
    let formula = if inverse then formula
      else Bool.mk_not ctxt formula in
    match Z3.Solver.check solver [formula] with
    | UNSATISFIABLE | UNKNOWN -> None
    | SATISFIABLE -> match Z3.Solver.get_model solver with
      | None -> None
      | Some model -> Some {model}

  module Model = struct
    type t = model
    let get {model} input =
      let var = var (Input.to_symbol input) (Input.size input) in
      Z3.Model.eval model var true
  end

end

type executor = {
  inputs : Set.M(Input).t;
  formulae : Formula.t Primus.Value.Id.Map.t;
}

let executor = Primus.Machine.State.declare
    ~uuid:"e21aa0fe-bc37-48e3-b398-ce7e764843c8"
    ~name:"symbolic-executor-formulae" @@ fun _ -> {
    formulae = Primus.Value.Id.Map.empty;
    inputs = Set.empty (module Input);
  }

let sexp_of_formula x = Sexp.Atom (Formula.to_string x)

let new_formula,on_formula = Primus.Observation.provide "new-formula"
    ~package:"bap"
    ~inspect:(fun (id,x) -> Sexp.List [
        Primus.Value.sexp_of_t id;
        sexp_of_formula x;
      ])

module Executor(Machine : Primus.Machine.S) : sig
  val formula : Primus.Value.t -> Formula.t Machine.t
  val inputs : Input.t seq Machine.t
  val init : unit -> unit Machine.t
end = struct
  open Machine.Syntax

  let get_formula s v k =
    let id = Primus.Value.id v in
    match Map.find s.formulae id with
    | Some e -> k s e
    | None ->
      let x = Formula.word @@ Primus.Value.to_word v in
      k {s with formulae = Map.add_exn s.formulae id x} x

  let add_formula s v f =
    Machine.Observation.make on_formula (v,f) >>= fun () ->
    Machine.Local.put executor {
      s with
      formulae = Map.add_exn s.formulae (Primus.Value.id v) f
    }

  let on_binop ((op,x,y),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    get_formula s y @@ fun s y ->
    add_formula s z (Formula.binop op x y)

  let on_unop ((op,x),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    add_formula s z (Formula.unop op x)

  let on_extract ((hi,lo,x),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    add_formula s z (Formula.extract hi lo x)

  let on_concat ((x,y),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    get_formula s y @@ fun s y ->
    add_formula s z (Formula.concat x y)

  let on_ite ((c,x,y),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s c @@ fun s c ->
    get_formula s x @@ fun s x ->
    get_formula s y @@ fun s y ->
    add_formula s z (Formula.ite c x y)

  let on_cast ((c,w,x),z) =
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    add_formula s z (Formula.cast c w x)

  let set_input origin x =
    let id = Primus.Value.id x in
    let size = Input.size origin  in
    let x = Formula.var (Input.to_symbol origin) size in
    Machine.Local.update executor ~f:(fun s -> {
          inputs = Set.add s.inputs origin;
          formulae = Map.add_exn s.formulae id x;
        })

  let on_memory_input (p,x) = set_input (Input.ptr p) x

  let on_env_input (v,x) = set_input (Input.var v) x

  let formula v =
    Machine.Local.get executor >>= fun s ->
    get_formula s v @@ fun s x ->
    Machine.Local.put executor s >>| fun () ->
    x

  let inputs =
    Machine.Local.get executor >>| fun s ->
    Set.to_sequence s.inputs

  let init () = Machine.sequence Primus.Interpreter.[
      binop >>> on_binop;
      unop >>> on_unop;
      extract >>> on_extract;
      concat >>> on_concat;
      ite >>> on_ite;
      cast >>> on_cast;
      Primus.Memory.generated >>> on_memory_input;
      Primus.Env.generated >>> on_env_input;
    ]
end


module Forker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Executor = Executor(Machine)
  module Eval = Primus.Interpreter.Make(Machine)

  let is_branch = function
    | Primus.Pos.Jmp {up={me=blk}} -> Term.length jmp_t blk > 1
    | _ -> false


  let on_cond cnd =
    Executor.formula cnd >>= fun expr ->
    let inverse = Word.is_one (Primus.Value.to_word cnd) in
    Eval.pc >>= fun pc ->
    Eval.pos >>= fun pos ->
    if is_branch pos then match Formula.solve ~inverse expr with
      | None ->
        Format.eprintf "%a: unreachable branch@\n%!" Addr.pp pc;
        Machine.return ()
      | Some model ->
        Format.eprintf "%a: constraint@\n%!" Addr.pp pc;
        Executor.inputs >>| fun inputs ->
        Seq.iter inputs ~f:(fun input ->
            let var = Input.to_symbol input in
            match Formula.Model.get model input with
            | None ->
              Format.eprintf "  %s -> SKIP@\n%!" var
            | Some x ->
              Format.eprintf "  %s -> %s@\n%!" var (Formula.to_string x))
    else Machine.return ()



  let init () = Machine.sequence Primus.Interpreter.[
      eval_cond >>> on_cond;
    ]

end
let () = Bap_main.Extension.declare  @@ fun _ ->
  Primus.Machine.add_component (module Executor)
  [@warning "-D"];
  Primus.Machine.add_component (module Forker)
  [@warning "-D"];
  Ok ()
