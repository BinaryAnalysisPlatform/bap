open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std

(* Algorithm.

   For each system, just when we start, we create the init machine
   that will be used to fork all other machines and re-execute the
   path N times, where N is the number of linearly independent paths
   that are reachable from the entry point. The init machine spawns
   a new worker for each model in its queue. We start with an initial
   empty model that starts the pioneer machine.

   Each worker (including the pioneer) runs from the entry point and
   builds the path formula. With each value we associate a
   formula, which we optimize on the fly. We designate certain values
   as inputs and keep them symbolic. When we hit the branch that we
   didn't see before and that leads to the desitantion which we
   neither saw nor plan to see, we take the formula associated with
   that branch condition and switch back to the init machine, and push
   its negation to the queue.

   When a worker finishes, the control is passed back to the init
   machine that picks the first actual formula and finds a satisfying
   model for it, then spawns a new worker process, which works as above.

   To identify which formula is actual, we associate with each
   formula the branch destination that will be taken if the formula
   is satisfiable. A formula is actual if this destination wasn't
   visited by a worker before. An unresolved destination is treated
   as unvisited, therefore it is always actual.

*)

type memory = {
  bank : Primus.Memory.Descriptor.t;
  ptr : Word.t;
  len : int;
} [@@deriving compare, equal]

let memories = Primus.Machine.State.declare
    ~uuid:"15dbb89a-3bfb-421c-8b19-16605425c3f5"
    ~name:"symbolic-memories" @@ fun _ ->
  String.Map.empty

module Id = Monad.State.Multi.Id
let debug_msg,post_msg = Primus.Observation.provide "executor-debug"
    ~inspect:sexp_of_string

module Debug(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  let msg fmt = Format.kasprintf (fun msg ->
      Machine.current () >>= fun id ->
      let msg = Format.asprintf "%a: %s" Id.pp id msg in
      Machine.Observation.make post_msg msg) fmt
end

module Input : sig
  type t
  val ptr : Primus.Memory.Descriptor.t -> addr -> t
  val var : var -> t
  val to_symbol : t -> string
  val size : t -> int

  module Make(Machine : Primus.Machine.S) : sig
    val set : t -> Primus.Value.t -> unit Machine.t
  end
  include Base.Comparable.S with type t := t
end = struct
  type t =
    | Var of var
    | Ptr of {
        bank : Primus.Memory.Descriptor.t;
        addr : addr;
      } [@@deriving compare, sexp_of]

  let ptr bank addr = Ptr {bank; addr}
  let var p = Var p

  let to_symbol = function
    | Var v -> Format.asprintf "%a" Var.pp v
    | Ptr {bank;addr} ->
      Format.asprintf "%s[%a]"
        (Primus.Memory.Descriptor.name bank)
        Addr.pp_hex addr

  let size = function
    | Ptr {bank} -> Primus.Memory.Descriptor.data_size bank
    | Var v -> match Var.typ v with
      | Imm m -> m
      | _ -> 1

  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Mems = Primus.Memory.Make(Machine)
    module Env = Primus.Env.Make(Machine)
    module Lisp = Primus.Lisp.Make(Machine)

    let allocate_if_not bank addr =
      Mems.is_mapped addr >>= function
      | true -> Machine.return ()
      | false ->
        let name = Primus.Memory.Descriptor.name bank in
        Machine.Global.get memories >>= fun memories ->
        match Map.find memories name with
        | None -> Lisp.failf "unknown symbolic memory %s" name ()
        | Some {ptr; len} -> Mems.allocate ptr len

    let set input value = match input with
      | Ptr {bank; addr} ->
        Mems.memory >>= fun current ->
        Mems.switch bank >>= fun () ->
        allocate_if_not bank addr >>= fun () ->
        Mems.set addr value >>= fun () ->
        Mems.switch current
      | Var v ->
        Env.set v value
  end

  include Base.Comparable.Make(struct
      type nonrec t = t [@@deriving compare, sexp_of]
    end)
end

module Formula : sig
  type t
  type formula = t
  type model
  type value

  val word : Word.t -> t
  val var : string -> int -> t

  val binop : binop -> t -> t -> t
  val unop : unop -> t -> t
  val extract : int -> int -> t -> t
  val concat : t -> t -> t
  val ite : t -> t -> t -> t
  val cast : cast -> int -> t -> t

  val solve : ?constraints:t list -> ?refute:bool -> t -> model option


  module Model : sig
    type t = model
    val get : model -> Input.t -> value option
  end

  module Value : sig
    type t = value
    val to_word : value -> word
    val to_formula : value -> formula
    val to_string : value -> string
  end

  val to_string : t -> string
end = struct
  module Bitv = Z3.BitVector
  module Bool = Z3.Boolean
  module Expr = Z3.Expr
  module Sort = Z3.Sort

  type t = Expr.expr
  type formula = t
  type value = t

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

  let bool_of_bit x =
    do_simpl @@ Bool.mk_eq ctxt x bit1

  let z3_of_unop : unop -> _ = function
    | NEG -> Bitv.mk_neg
    | NOT -> Bitv.mk_not

  let coerce_to_bit x =
    if is_bool x then bit_of_bool x else x

  let coerce_to_bool x =
    if is_bool x then x else bool_of_bit x


  let simpl x = Expr.simplify x None |>
                coerce_to_bit

  let binop op x y = simpl (z3_of_binop op ctxt x y)
  let unop op x = simpl (z3_of_unop op ctxt x)

  let word x =
    let s = Bitv.mk_sort ctxt (Word.bitwidth x) in
    let x = Word.to_bitvec x in
    if Bitvec.fits_int x
    then Expr.mk_numeral_int ctxt (Bitvec.to_int x) s
    else
      let x = Bitvec.to_bigint x in
      Expr.mk_numeral_string ctxt (Z.to_string x) s

  let extract hi lo x =
    let xs = Bitv.get_size (Expr.get_sort x)
    and ns = hi-lo+1 in
    simpl @@
    if ns > xs
    then if lo = 0
      then Bitv.mk_zero_ext ctxt (ns-xs) x
      else
        Bitv.mk_extract ctxt hi lo @@
        Bitv.mk_zero_ext ctxt (ns-xs) x
    else
      Bitv.mk_extract ctxt hi lo x

  let concat x y = simpl @@ Bitv.mk_concat ctxt x y

  let ite c x y = simpl @@ Bool.mk_ite ctxt (coerce_to_bool c) x y

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
    Expr.mk_const_s ctxt name (Bitv.mk_sort ctxt size)

  type model = {
    model : Z3.Model.model;
  }

  let solve ?(constraints=[]) ?(refute=false) expr =
    let solver = Z3.Solver.mk_simple_solver ctxt in
    let constraints = List.map constraints ~f:bool_of_bit in
    let lhs = Expr.mk_numeral_int ctxt 0 (Expr.get_sort expr) in
    let formula = Bool.mk_eq ctxt lhs expr in
    let formula = if refute then formula
      else Bool.mk_not ctxt formula in
    match Z3.Solver.check solver (formula::constraints) with
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

  module Value = struct
    type t = value
    let to_formula = ident
    let to_bigint x =
      let s = Expr.to_string x in
      let len = String.length s in
      if String.length s > 2 && Char.(s.[0] = '#')
      then
        let base = match s.[1] with
          | 'b' -> 2
          | 'x' -> 16
          | _ -> failwithf "Not a literal value %s" s () in
        Z.of_substring_base base s ~pos:2 ~len:(len-2)
      else Z.of_string s

    let to_word x =
      let s = Bitv.get_size (Expr.get_sort x)
      and z = to_bigint x in
      Word.create Bitvec.(bigint z mod modulus s) s

    let to_string x = Expr.to_string x
  end
end

type executor = {
  inputs : Set.M(Input).t;
  constraints : Formula.t list;
  formulae : Formula.t Primus.Value.Id.Map.t;
}

let executor = Primus.Machine.State.declare
    ~uuid:"e21aa0fe-bc37-48e3-b398-ce7e764843c8"
    ~name:"symbolic-executor-formulae" @@ fun _ -> {
    formulae = Primus.Value.Id.Map.empty;
    inputs = Set.empty (module Input);
    constraints = [];
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
  val set_input : Input.t -> Primus.Value.t -> unit Machine.t
  val constraints : Formula.t list Machine.t
  val add_constraint : Primus.Value.t -> unit Machine.t
end = struct
  open Machine.Syntax

  module Value = Primus.Value.Make(Machine)
  module Mems = Primus.Memory.Make(Machine)

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
          s with
          inputs = Set.add s.inputs origin;
          formulae = Map.set s.formulae id x;
        })

  let on_memory_input (p,x) =
    Mems.memory >>= fun desc ->
    set_input (Input.ptr desc p) x

  let on_env_input (v,x) = set_input (Input.var v) x

  let formula v =
    Machine.Local.get executor >>= fun s ->
    get_formula s v @@ fun s x ->
    Machine.Local.put executor s >>| fun () ->
    x

  let inputs =
    Machine.Local.get executor >>| fun s ->
    Set.to_sequence s.inputs

  let add_constraint x =
    let inverse = Value.is_zero x in
    Machine.Local.get executor >>= fun s ->
    get_formula s x @@ fun s x ->
    let x = if inverse then Formula.unop NOT x else x in
    Machine.Local.put executor {
      s with constraints = x :: s.constraints;
    }

  let constraints =
    Machine.Local.get executor >>| fun s -> s.constraints

  let init () = Machine.sequence Primus.Interpreter.[
      binop >>> on_binop;
      unop >>> on_unop;
      extract >>> on_extract;
      concat >>> on_concat;
      ite >>> on_ite;
      cast >>> on_cast;
      Primus.Memory.generated >>> on_memory_input;
      Primus.Env.generated >>> on_env_input;
      eval_cond >>> add_constraint;
    ]
end

type task = {
  inputs : Input.t Seq.t;
  refute : bool;
  rhs : Formula.t;
  blk : blk term;
  src : jmp term;
  dst : Jmp.dst;
}

type master = {
  self : Id.t option;
  worklist : task list;
  forks : Set.M(Tid).t;
  dests : Set.M(Tid).t;
}

let master = Primus.Machine.State.declare
    ~uuid:"8d2b41d4-4852-40e9-917a-33f1f5af01a1"
    ~name:"symbolic-executor-master" @@ fun _ -> {
    self = None;
    worklist = [];
    forks = Tid.Set.empty;      (* queued or finished forks *)
    dests = Tid.Set.empty;      (* queued of finished dests *)
  }

let pp_dst ppf dst = match Jmp.resolve dst with
  | First tid -> Format.fprintf ppf "%a" Tid.pp tid
  | Second _ -> Format.fprintf ppf "unk"

let pp_task ppf {rhs; blk; src; dst; refute} =
  Format.fprintf ppf "%a(%a) -> %a s.t.%s:@\n%s"
    Tid.pp (Term.tid blk)
    Tid.pp (Term.tid src) pp_dst dst
    (if refute then " not" else "")
    (Formula.to_string rhs)


module Forker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Executor = Executor(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Visited = Bap_primus_track_visited.Set.Make(Machine)
  module Debug = Debug(Machine)

  let is_conditional jmp = match Jmp.cond jmp with
    | Bil.Int _ -> false
    | _ -> true

  (* is Some (blk,jmp) when the current position is a branch point *)
  let branch_point = function
    | Primus.Pos.Jmp {up={me=blk}; me=jmp}
      when Term.length jmp_t blk > 1 &&
           is_conditional jmp ->
      Some (blk,jmp)
    | _ -> None


  let other_dst ~taken blk jmp =
    let is_other = if taken
      then Fn.non (Term.same jmp)
      else Term.same jmp in
    Term.enum jmp_t blk |>
    Seq.find_map ~f:(fun jmp ->
        if not (is_other jmp) then None
        else match Jmp.alt jmp with
          | None -> Jmp.dst jmp
          | dst -> dst) |> function
    | Some dst -> dst
    | None ->
      failwithf "Broken branch %a at:@\n%a"
        Jmp.pps jmp Blk.pps blk ()

  let is_retired visited master src dst =
    Set.mem master.forks (Term.tid src) ||
    match Jmp.resolve dst with
    | First tid -> Set.mem master.dests tid || Set.mem visited tid
    | Second _ -> true

  let is_actual visited master src dst =
    not (is_retired visited master src dst)

  let on_cond cnd =
    let taken = Word.is_one (Primus.Value.to_word cnd) in
    Eval.pos >>= fun pos ->
    match branch_point pos with
    | None -> Machine.return ()
    | Some (blk,src) ->
      Machine.Global.get master >>= fun s ->
      Visited.all >>= fun visited ->
      let dst = other_dst ~taken blk src in
      if is_retired visited s src dst
      then Machine.return ()
      else
        Executor.formula cnd >>= fun rhs ->
        Executor.inputs >>= fun inputs ->
        let task = {refute = taken; inputs; rhs; src; dst; blk} in
        Machine.Global.put master {
          s with
          worklist = task :: s.worklist;
          forks = Set.add s.forks (Term.tid src);
          dests = match Jmp.resolve dst with
            | First dst -> Set.add s.dests dst
            | Second _ -> s.dests
        }

  let pop_task () =
    let rec pop visited = function
      | [] -> None
      | t :: ts -> match Jmp.resolve t.dst with
        | First dst when Set.mem visited dst -> pop visited ts
        | _ -> Some (t, ts) in
    Machine.Global.get master >>= fun s ->
    Visited.all >>= fun visited ->
    match pop visited s.worklist with
    | None -> Machine.Global.put master {
        s with worklist=[]
      } >>| fun () -> None
    | Some (t,ts) -> Machine.Global.put master {
        s with worklist = ts
      } >>| fun () ->
      Some t

  let exec_task ({inputs; refute; rhs} as task) =
    Debug.msg "starting a new task %a" pp_task task >>= fun () ->
    match Formula.solve ~refute rhs with
    | None ->
      Debug.msg "can't find a solution!" >>= fun () ->
      Machine.return ()
    | Some model ->
      let string_of_input = Input.to_symbol in
      let module Input = Input.Make(Machine) in
      Debug.msg "solution was found!" >>= fun () ->
      Machine.Seq.iter inputs ~f:(fun input ->
          match Formula.Model.get model input with
          | None -> assert false
          | Some value ->
            Debug.msg "path-constraint %s = %s"
              (string_of_input input)
              (Formula.Value.to_string value) >>= fun () ->
            Value.of_word (Formula.Value.to_word value) >>= fun value ->
            Executor.set_input input value  >>= fun () ->
            Input.set input value)

  let rec run_master system =
    Machine.Global.get master >>= fun s ->
    match s.self with
    | None ->
      Machine.current () >>= fun master_pid ->
      Debug.msg "starting the master process with pid %a"
        Id.pp master_pid >>= fun () ->
      Machine.Global.put master {
        s with self = Some master_pid
      } >>= fun () ->
      Machine.fork () >>= fun () ->
      Machine.current () >>= fun client_pid ->
      Debug.msg "the first fork returns at %a"
        Id.pp client_pid >>= fun () ->
      if Id.equal master_pid client_pid
      then run_master system
      else Machine.return ()
    | Some master_pid ->
      pop_task () >>= function
      | None ->
        Debug.msg "Worklist is empty, finishing" >>= fun () ->
        Machine.return ()
      | Some t  ->
        Debug.msg "We have some tasks" >>= fun () ->
        Machine.fork () >>= fun () ->
        Machine.current () >>= fun client_pid ->
        if Id.equal master_pid client_pid
        then
          Debug.msg "Client is finished, master is resumed" >>= fun () ->
          run_master system
        else
          Debug.msg "Forked a new machine" >>= fun () ->
          exec_task t

  let init () = Machine.sequence Primus.Interpreter.[
      eval_cond >>> on_cond;
      Primus.System.start >>> run_master;
    ]
end

let sexp_of_assert_failure (name,model,formula,inputs) =
  Sexp.List (
    Atom name ::
    sexp_of_formula formula ::
    List.filter_map inputs ~f:(fun input ->
        match Formula.Model.get model input with
        | None -> None
        | Some value -> Option.some @@ Sexp.List [
            Sexp.Atom (Input.to_symbol input);
            Sexp.Atom (Formula.Value.to_string value)
          ]))

let assert_failure,failed_assertion =
  Primus.Observation.provide "assert-failure"
    ~inspect:sexp_of_assert_failure
    ~desc:"occurs when a symbolic executor assertion doesn't hold"

let to_word x = Primus.Value.to_word x
let to_int x = to_word x |> Word.to_int_exn

module SymbolicPrimitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Lisp = Primus.Lisp.Make(Machine)
  module Mems = Primus.Memory.Make(Machine)
  module Val = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Executor = Executor(Machine)
  module Bank = Primus.Memory.Descriptor
  module Closure = Primus.Lisp.Closure.Make(Machine)
  module Debug = Debug(Machine)

  let addr_size arch_addr_size = function
    | [addr_size; _data_size] -> to_int addr_size
    | [] | [_] -> arch_addr_size
    | _ -> failwith "symbolic-memory: expects 3 to 5 arguments"

  let data_size = function
    | [_addr_size; data_size] -> to_int data_size
    | _ -> 8

  let rec set_inputs bank data_size lower upper =
    if Word.(lower > upper) then Machine.return ()
    else
      Val.zero data_size >>=
      Executor.set_input (Input.ptr bank lower) >>= fun () ->
      set_inputs bank data_size (Word.succ lower) upper

  let register_memory m =
    let name = Bank.name m.bank in
    Machine.Global.update memories ~f:(fun mem ->
        Map.update mem name ~f:(function
            | None -> m
            | Some m' when equal_memory m m' -> m
            | _ ->
              invalid_argf "symbolic-memory: %s was previously \
                            define with different parameters"
                name ()))

  let with_memory {bank} perform =
    Mems.memory >>= fun current ->
    Mems.switch bank >>= fun () ->
    perform >>= fun x ->
    Mems.switch current >>| fun () ->
    x

  let open_bank id =
    Machine.Global.get memories >>= fun mems ->
    Val.Symbol.of_value id >>= fun name ->
    match Map.find mems name with
    | None ->
      Lisp.failf "unknown memory %s" name ()
    | Some bank -> Machine.return bank

  let read id ptr =
    open_bank id >>= fun bank ->
    with_memory bank (Mems.get (to_word ptr))

  let write id ptr data =
    open_bank id >>= fun bank ->
    with_memory bank (Mems.set (to_word ptr) data) >>| fun () ->
    ptr

  let width default_width = function
    | [] -> default_width
    | s :: _ -> to_int s

  let value width = function
    | [_; x] -> Machine.return x
    | _ -> Val.of_int ~width 0

  let arch_addr_size =
    Machine.gets Project.arch >>|
    Arch.addr_size >>|
    Size.in_bits

  let create_value var rest =
    arch_addr_size >>= fun default_width ->
    let width = width default_width rest in
    Val.Symbol.of_value var >>= fun name ->
    let var = Var.create ("R_" ^ name) (Imm width) in
    Env.has var >>= function
    | true -> Env.get var
    | false ->
      value width rest >>= fun x ->
      Executor.set_input (Input.var var) x >>= fun () ->
      Env.set var x >>| fun () ->
      x

  let create_memory id lower upper rest =
    arch_addr_size >>= fun arch_addr_size ->
    Val.Symbol.of_value id >>= fun name ->
    Machine.Global.get memories >>= fun memories ->
    if Map.mem memories name then Machine.return id
    else
      let bank = Bank.create name
          ~addr_size:(addr_size arch_addr_size rest)
          ~data_size:(data_size rest) in
      let lower = to_word lower and upper = to_word upper in
      let len = match Word.(to_int (upper - lower)) with
        | Error _ ->
          invalid_arg "symbolic-memory: the region is too large"
        | Ok diff -> diff + 1 in
      let memory = {bank; ptr=lower; len} in
      register_memory memory >>= fun () ->
      with_memory memory (Mems.allocate lower len) >>= fun () ->
      set_inputs bank (data_size rest) lower upper >>| fun () ->
      id

  let assume assumptions =
    Machine.List.iter assumptions ~f:Executor.add_constraint
    >>= fun () ->
    Val.b1

  let assert_ name assertions =
    Val.Symbol.of_value name >>= fun name ->
    Machine.List.iter assertions ~f:(fun assertion ->
        Executor.constraints >>= fun constraints ->
        Executor.formula assertion >>= fun x ->
        Debug.msg "checking that %s holds"
          (Formula.to_string x) >>= fun () ->
        Machine.List.iter constraints ~f:(fun constr ->
            Debug.msg "s.t. %s" (Formula.to_string constr)) >>= fun () ->
        Machine.Observation.post failed_assertion ~f:(fun report ->
            match Formula.solve ~constraints ~refute:true x with
            | None ->
              Debug.msg "it holds!" >>= fun () ->
              Machine.return ()
            | Some model ->
              Debug.msg "it doesn't hold!" >>= fun () ->
              Executor.inputs >>| Seq.to_list >>= fun inputs ->
              report (name,model,x,inputs))) >>= fun () ->
    Val.b1

  let run args =
    Closure.name >>= fun name -> match name,args with
    | "symbolic-memory", (id :: lower :: upper :: rest) ->
      create_memory id lower upper rest
    | "symbolic-memory-read", [id; ptr] -> read id ptr
    | "symbolic-memory-write", [id; ptr; data] -> write id ptr data
    | "symbolic-assume", args -> assume args
    | "symbolic-assert", name :: args -> assert_ name args
    | _ -> Lisp.failf "%s: invalid number of arguments" name ()
end

module Primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let def name types docs =
    Lisp.define name (module SymbolicPrimitives) ~types ~docs

  let init () = Machine.sequence [
      def "symbolic-assume" (all bool @-> bool)
        "Assumes the specified list of constraints.";
      def "symbolic-assert" (one sym // all bool @-> bool)
        "(assert NAME C1 ... CM) verifies the specified
         list of assertions C1 and posts the assert-failure observation
         for each condition that doesn't hold.";
      def "symbolic-value" (one sym // all int @-> a)
        "(symbolic-value NAME &rest) creates a new symbolic value.
        Accepts up to two optional arguments. The first argument
        denotes the bitwidth of the created value and the second is
        the initial value.";
      def "symbolic-memory" (tuple [sym; int; int] // all int @-> sym)
        "(symbolic-memory NAME LOWER UPPER &rest) creates a new
        symbolic memory region with the given NAME and LOWER and UPPER
        bounds. The symbolic memory could be read and written with
        the SYMBOLIC-MEMORY-READ and SYMBOLIC-MEMORY-WRITE primitives.
        Optionally accepts one or two more arguments. If two optional
        arguments are provided, then the first one stands for the
        address bus size of the symbolic memory and the second for the
        data bus size, all in bits. If only one is provided then it is
        interpreted as the data bus size and the address size defaults
        to the architecture-specific address size (e.g., 64 bits for
        amd64). If none are provided then the address size is
        architecture-specific and the data size defaults to 8 bits."
    ]
end

let () = Bap_main.Extension.declare  @@ fun _ ->
  Primus.Components.register_generic "symbolic-computer"
    (module Executor) ~package:"bap"
    ~desc:"Computes a symbolic formula for each Primus value.";
  Primus.Components.register_generic "symbolic-path-explorer"
    (module Forker) ~package:"bap"
    ~desc:"Computes a path constraint for each branch and forks a \
           new machine if the constraint is satisfiable.";
  Primus.Components.register_generic "symbolic-lisp-primitives"
    ~package:"bap"
    ~desc:"Provides assume and assert primitives."
    (module Primitives);
  Ok ()
