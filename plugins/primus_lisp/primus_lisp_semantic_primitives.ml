open Core_kernel
open Bap_core_theory
open Bap_primus.Std
open KB.Syntax
open KB.Let
module Z = Bitvec

let export = Primus.Lisp.Type.Spec.[
    "+", all any @-> any,
    "(+ X Y ... Z) returns X + Y + ... + Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 0 is \
     returned.";

    "-", all any @-> any,
    "(- X Y ... Z) returns X - Y - ... - Z, performing any necessary \
     type conversions in the process. If no numbers are supplied \
     returns 0. If one number is supplied returns its negation.";

    "neg", one any @-> any,
    "(neg X) returns the negation (2-complement) of X. Same as (- X).";

    "lnot", one a @-> a,
    "(lnot X) returns a bitwise logical negation of X.";

    "*", all any @-> any,
    "(*  X Y ... Z) returns X * Y * ... * Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 1 is \
     returned.";

    "/", all any @-> any,
    "(/  X Y ... Z) returns X / Y / ... / Z, performing any necessary \
     type conversions in the process. If no numbers are supplied, 1 is \
     returned. If one number is provided returns its reciprocal.";


    "s/", all any @-> any,
    "(s/ X Y ... Z) returns signed X / Y / ... / Z, performing any \
     necessary type conversions in the process. If no numbers are \
     supplied, 1 is returned. If one number is provided returns its \
     signed reciprocal.";

    "mod", all any @-> any,
    "(/ X Y ... Z) returns mod(X,Y) Y mod ... mod Z, performing any \
     necessary type conversions in the process. Where `X mod Y` is the \
     remainder of [X / Y]. If no numbers are supplied, 1 is \
     returned. If one number is provided returns that number.";

    "signed-mod", all any @-> any,
    "(/ X Y ... Z) returns signed mod(X,Y) Y mod ... mod Z, performing \
     any necessary type conversions in the process. Where `X mod Y` is \
     the remainder of [X / Y]. If no numbers are supplied, 1 is \
     returned. If one number is provided returns that number.";

    "lshift", all any @-> any,
    "(lshift X Y ... Z) returns X << Y << ... << Z, performing \
     any necessary type conversions in the process. Where `X << Y` is \
     logical shift left of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "rshift", all any @-> any,
    "(rshift X Y ... Z) returns X >> Y >> ... >> Z, performing \
     any necessary type conversions in the process. Where `X >> Y` is \
     logical shift right of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "arshift", all any @-> any,
    "(arshift X Y ... Z) returns X ~>> Y ~>> ... ~>> Z, performing \
     any necessary type conversions in the process. Where `X ~>> Y` is \
     arithmetic shift right of X by Y bits. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logand", all any @-> any,
    "(logand X Y ... Z) returns X land Y land ... land Z, performing \
     any necessary type conversions in the process. Where `X land Y` is \
     bitwise (logical) /\ (AND) of X and Y. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logor", all any @-> any,
    "(logor X Y ... Z) returns X lor Y lor ... lor Z, performing \
     any necessary type conversions in the process. Where `X lor Y` is \
     bitwise (logical) \\/ (OR) of X and Y. If no numbers are supplied, 1 \
     is returned. If one number is provided returns that number";

    "logxor", all any @-> any,
    "(loxgor X Y ... Z) returns X lxor Y lxor ... lxor Z, performing \
     any necessary type conversions in the process. Where `X lor Y` is \
     bitwise (logical) exclusive \\/ (XOR) of X and Y. If no numbers \
     are supplied, 1 is returned. If one number is provided returns \
     that number";

    "=", all any @-> any,
    "(= X Y ... Z) returns one if all numbers are equal in value.";

    "/=", all any @-> any,
    "(/= X Y ... Z) returns one if all numbers are distinct.";

    "<", all any @-> any,
    "(< X Y ... Z) returns one if all numbers are in monotonically \
     increasing order.";

    ">", all any @-> any,
    "(> X Y ... Z) returns one if all numbers are in monotonically \
     decreasing order.";

    "<=", all any @-> any,
    "(<= X Y ... Z) returns one if all numbers are in monotonically \
     nondecreasing order.";

    ">=", all any @-> any,
    "(> X Y ... Z) returns one if all numbers are in monotonically \
     nonincreasing order.";

    "s<", all any @-> any,
    "(s< X Y ... Z) returns one if all numbers are in monotonically \
     increasing signed order.";

    "s>", all any @-> any,
    "(s> X Y ... Z) returns one if all numbers are in monotonically \
     decreasing signed order.";

    "s<=", all any @-> any,
    "(s<= X Y ... Z) returns one if all numbers are in monotonically \
     nondecreasing signed order.";

    "s>=", all any @-> any,
    "(> X Y ... Z) returns one if all numbers are in monotonically \
     nonincreasing signed order.";

    "is-zero", all any @-> any,
    "(is-zero X Y ... Z) returns one if all numbers are zero.";

    "not", all any @-> any,
    "(not X Y ... Z) returns one if all numbers are not \
     true. Equivalent to (is-zero X Y Z)";

    "is-positive", all any @-> any,
    "(is-positive X Y ... Z) returns one if all numbers are positive.";

    "is-negative", all any @-> any,
    "(is-negative X Y ... Z) returns one if all numbers are negative.";

    "word-width", all any @-> any,
    "(word-width X Y ... Z) returns the maximum width of its \
     arguments. If no arguments provided returns the size of the \
     machine word.";

    "exec-addr", one int @-> any,
    "(exec-addr ADDR) transfers control to ADDR.";

    "invoke-subroutine", one sym @-> any,
    "(invoke-subroutine NAME) passes control to the subroutine NAME.";

    "goto-subinstruction", one int @-> any,
    "(goto-subinstruction N) transfers control flow to a
    subinstruction that is N instructions away from the current (N
    could be negative).";

    "load-byte", one int @-> byte,
    "(load-byte PTR) loads one byte from the address PTR";

    "memory-read", one int @-> byte,
    "(memory-read PTR) loads one byte from the address PTR \
     (synonymous to load-byte)";

    "load-word", one int @-> int,
    "(load-word PTR) loads one word from the address PTR";

    "load-bits", tuple [any; int] @-> int,
    "(load-word SIZE PTR) loads a SIZE-bit long word from the address PTR";

    "load-hword", one int @-> any,
    "(load-hword PTR) loads half-word from the address PTR";

    "load-dword", one int @-> any,
    "(load-hword PTR) loads double-word from the address PTR";

    "load-qword", one int @-> any,
    "(load-hword PTR) loads quad-word from the address PTR";

    "store-byte", tuple[int; any] @-> any,
    "(store-byte POS VAL) stores byte VAL at the memory position POS";

    "store-word", tuple[int; any] @-> any,
    "(store-word POS VAL) stores VAL at the memory position POS";

    "memory-write", tuple [int; byte] @-> int,
    "(memory-write PTR X) stores byte X at PTR.";

    "get-program-counter", unit @-> int,
    "(get-program-counter) returns the address of the current instruction";

    "get-current-program-counter", unit @-> int,
    "(get-current-program-counter) is an alias to (get-program-counter)";

    "set-symbol-value", tuple [any; a] @-> a,
    "(set-symbol-value S X) sets the value of the symbol S to X.
         Returns X";

    "symbol", one any @-> sym,
    "(symbol X) returns a symbol representation of X.";

    "is-symbol", one any @-> bool,
    "(is-symbol X) is true if X has a symbolic value.";

    "symbol-concat", all sym @-> sym,
    "(symbol-concat S1 S2 .. SN OPT?) concatenates symbols
    S1 till S2. An optional separator keyworded argument
    takes form :sep SEP, where SEP is the separator that is
    used to concatenate symbols";

    "alias-base-register", one int @-> int,
    "(alias-base-register x) if X has a symbolic value that is an
     aliased register returns the base register";

    "cast-low", tuple [int; a] @-> b,
    "(cast-low S X) extracts low S bits from X.";

    "cast-high", tuple [int; a] @-> b,
    "(cast-high S X) extracts high S bits from X.";

    "cast-signed", tuple [int; a] @-> b,
    "(cast-signed S X) performs signed extension of X to the size of S bits";

    "cast-unsigned", tuple [int; a] @-> b,
    "(cast-unsigned S X) performs unsigned extension of X to the size of S bits";

    "extract", tuple [any; any; any] @-> any,
    "(extract HI LO X) extracts bits from HI to LO (both ends
    including) of X, the returned value has HI-LO+1 bits.
    Both HI and LO must be static.";

    "concat", (all any @-> any),
    "(concat X Y Z ...) concatenates words X, Y, Z, ... into one big word";

    "select", (all any @-> any),
    "(select B1 B2 ... BN X) returns a word that is a concatenation
    of bits B1, B2, ... BN of X. All of the B1, ..., BN must be
    static.";
    "nth", (one any @-> bool),
    "(nth N X) returns the Nth bit of X. N must be static. \
     The function is equivalent to (select N X)";
    "empty", (unit @-> any),
    "(empty) denotes an instruction that does nothing, i.e., a nop.";
    "special", (one sym @-> any),
    "(special :NAME) produces a special effect denoted by the keyword :NAME.
    The effect will be reified into the to the special:name subroutine. ";
    "intrinsic", tuple [sym] // all any @-> any,
    "(intrinsic 'NAME ARG1 ARG2 ... ARGN PARAMS..) produces a call to
     an intrinsic function with the given NAME. Arguments could be
     regular semantic values. They are passed to the intrinsic
     function in order, with the first argument passed via the
     intrinsic:x0 ... intrinsic:xN variables. The keyworded parameters
     are optional. And could be either :return SIZE, which indicates
     that the call evaluates to a bitvector value of the given SIZE;
     :writes REG SIZE? denotes that writes the result to a variable
     REG (the size is optional if REG is a known target register);
     :stores PTR SIZE? denotes that the intrinsic is storing output of
     the given SIZE in bits at the memory location pointer by PTR.
     When SIZE is omitted, it defaults to the data address size of the
     target. The :result parameter may occur at most once. All other
     parameters can be repeated. The intrinisic output is passed via
     the intrinisic:y0,...,intrinisic:yM vector, with the first
     element assigned to the :result, then to all :writes registers,
     and after that to all :stores addresses." ]

type KB.conflict += Illformed of string
                 | Failed_primitive of KB.Name.t * unit Theory.value list * string

let illformed fmt =
  Format.kasprintf (fun msg ->
      KB.fail (Illformed msg)) fmt

let string_of_error name args err =
  Format.asprintf
    "Failed to apply primitive %a: %s@\nApplied as:@\n@[<hov2>(%a %a)@]"
    KB.Name.pp name err KB.Name.pp name
    Format.(pp_print_list ~pp_sep:pp_print_space KB.Value.pp) args

let () = KB.Conflict.register_printer (function
    | Illformed msg -> Some ("illformed lisp program " ^ msg)
    | Failed_primitive (name,args,err) ->
      Some (string_of_error name args err)
    | _ -> None)



let nothing = KB.Value.empty Theory.Semantics.cls
let size = Theory.Bitv.size
let forget x = x >>| Theory.Value.forget
let empty s = Theory.Value.(forget @@ empty s)
let null = KB.Object.null Theory.Program.cls
let sort = Theory.Value.sort
let bits x = size @@ sort x

module type Target = sig
  val target : Theory.Target.t
end

module Primitives(CT : Theory.Core)(T : Target) = struct
  open T

  let rec seq = function
    | [] -> CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs -> CT.seq x @@ seq xs

  let undefined = seq []
  let pass = seq []
  let skip = seq []

  let negone s =
    CT.int s @@ Bitvec.(ones mod modulus (size s))


  let unary = function
    | [x] -> !!x
    | _ -> illformed "requires exactly one argument"

  let binary xs f = match xs with
    | [x; y] -> f x y
    | _ -> illformed "requires exactly two arguments"

  let ternary xs f = match xs with
    | [x; y; z] -> f x y z
    | _ -> illformed "requires exactly three arguments"

  let const x = KB.Value.get Primus.Lisp.Semantics.static x
  let symbol x = KB.Value.get Primus.Lisp.Semantics.symbol x
  let set_const v x =
    KB.Value.put Primus.Lisp.Semantics.static v (Some x)
  let const_int s x = CT.int s x >>| fun v -> set_const v x
  let set_sym n v =
    KB.Value.put Primus.Lisp.Semantics.symbol v (Some n)
  let int s x = const_int s @@ Bitvec.(int x mod modulus (size s))
  let true_ = CT.b1 >>| fun v -> set_const v Bitvec.one
  let false_ = CT.b0 >>| fun v -> set_const v Bitvec.zero
  let const_bool x = if x then true_ else false_

  let bool b =
    let s = Theory.Bitv.define 1 in
    let b1 = const_int s Bitvec.M1.one
    and b0 = const_int s Bitvec.M1.zero in
    match const b with
    | Some r ->
      if Bitvec.(equal r zero)
      then b0
      else b1
    | None -> CT.ite !!b b1 b0

  let intern name =
    let name = KB.Name.read name in
    KB.Symbol.intern (KB.Name.unqualified name) Theory.Value.cls >>|
    KB.Object.id >>| Int63.to_int64 >>|
    Bitvec.M64.int64

  let bitv x =
    match Theory.Value.resort Theory.Bitv.refine x with
    | Some x -> !!x
    | None -> match Theory.Value.resort Theory.Bool.refine x with
      | Some b -> bool b
      | None -> match symbol x with
        | None -> illformed "defined for bitvecs, bools, or syms"
        | Some name ->
          let s = Theory.Bitv.define 64 in
          let x = KB.Value.refine x s in
          intern name >>| set_const x

  let to_int x =
    bitv x >>| const >>| function
    | Some sz when Bitvec.fits_int sz -> Some (Bitvec.to_int sz)
    | _ -> None

  let static x =
    to_int x >>= function
    | None -> illformed "expects a staticly known value"
    | Some x -> !!x


  let nbitv = KB.List.map ~f:bitv

  let join s xs =
    List.max_elt xs ~compare:(fun x y ->
        let xs = sort x and ys = sort y in
        Theory.Bitv.(compare_int (size xs) (size ys))) |> function
    | None -> s
    | Some v -> sort v

  let first s = function
    | [] -> s
    | x::_ -> sort x

  let with_nbitv s cast xs f = match xs with
    | [] -> f s []
    | xs ->
      nbitv xs >>= fun xs ->
      f (cast s xs) xs

  type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

  let coerce s x =
    if Theory.Value.Sort.same (Theory.Value.sort x) s then !!x
    else match const x with
      | Some x -> const_int s x
      | None -> CT.signed s !!x

  let monoid s cast sf df init xs =
    with_nbitv s cast xs @@ fun s xs ->
    let m = Z.modulus (size s) in
    match xs with
    | [] -> forget@@const_int s Z.(init mod m)
    | x :: xs ->
      let* init = coerce s x in
      KB.List.fold ~init xs ~f:(fun res x ->
          match const res, const x with
          | Some res, Some x ->
            const_int s Z.(sf res x mod m)
          | _ ->
            let* x = coerce s x in
            df !!res !!x) |>
      forget

  let is_one x = CT.(inv@@is_zero x)

  let (&&&) x y = match const x, const y with
    | Some x, Some y ->
      if Bitvec.(equal x zero || equal y zero)
      then false_
      else true_
    | _ -> CT.and_ !!x !!y

  let rec is_ordered sf df = function
    | [] | [_] -> true_
    | x :: (y :: _ as rest) ->
      bitv x >>= fun x ->
      bitv y >>= fun y ->
      match const x, const y with
      | Some x, Some y ->
        const_bool@@sf x y >>= fun r ->
        is_ordered sf df rest >>= fun r' ->
        r &&& r'
      | _ ->
        df !!x !!y >>= fun r ->
        is_ordered sf df rest >>= fun r' ->
        CT.and_ !!r !!r'

  let order sf df xs = forget@@is_ordered sf df xs

  let all s cast sf df xs =
    true_ >>= fun init ->
    with_nbitv s cast xs @@ fun s xs ->
    let m = Z.modulus (size s) in
    KB.List.fold ~init xs ~f:(fun r x ->
        let r' = match const x with
          | Some x -> const_bool Z.(sf x m)
          | None -> df !!x in
        r' >>= fun r' ->
        r &&& r') |>
    forget


  let full eff res =
    res >>= fun res ->
    eff >>| fun eff ->
    KB.Value.put Theory.Semantics.value eff res

  let pure res = full (seq []) res

  let ctrl eff =
    CT.blk null (seq []) eff

  let data eff =
    CT.blk null eff (seq [])

  let memory eff res =
    full CT.(blk null (perform eff) skip) res

  let nop () =
    CT.perform Theory.Effect.Sort.bot

  let loads = memory Theory.Effect.Sort.rmem
  let stores = memory Theory.Effect.Sort.wmem
  let loads = pure


  let d_is_negative x = CT.msb x
  let d_is_positive x =
    CT.(and_ (non_zero x) (inv (d_is_negative x)))

  let s_is_negative x m = Z.(msb x mod m)
  let s_is_positive x m =
    not (Z.(s_is_negative x m) && Z.equal x Z.zero)
  let s_is_zero x _ =
    Z.equal x Z.zero

  let word_width s xs =
    nbitv xs >>= fun xs ->
    List.max_elt xs ~compare:(fun x y ->
        Int.compare (bits x) (bits y)) |>
    Option.value_map ~f:(fun x ->
        int s (bits x))
      ~default:(int s (size s)) |>
    forget

  let exec_addr xs =
    unary xs >>= bitv >>= fun dst ->
    match const dst with
    | None -> CT.jmp !!dst
    | Some bitv ->
      Theory.Label.for_addr bitv >>= fun dst ->
      CT.goto dst

  let goto_subinstruction lbl xs =
    let open Bap.Std in
    unary xs >>= bitv >>= fun dst ->
    match const dst with
    | None -> CT.jmp !!dst
    | Some dst ->
      KB.collect Insn.Seqnum.slot lbl >>= function
      | None -> illformed "not a subinstruction"
      | Some pos ->
        let dst = Bitvec.to_int dst + pos in
        Bap.Std.Insn.Seqnum.label dst >>=
        CT.goto

  let load_byte t xs =
    let mem = CT.var @@ Theory.Target.data t in
    unary xs >>= bitv >>= fun addr -> forget@@CT.(load mem !!addr)


  let is_big_endian t =
    if Theory.Endianness.equal (Theory.Target.endianness t)
        Theory.Endianness.eb then CT.b1 else CT.b0

  let to_sort x =
    to_int x >>= function
    | None -> illformed "the sort specification must be static and small"
    | Some x -> KB.return (Theory.Bitv.define x)

  let word_loader f t xs =
    let mem = CT.var @@ Theory.Target.data t in
    let s = Theory.Bitv.define (f (Theory.Target.bits t)) in
    let b = is_big_endian t in
    unary xs >>= bitv >>= fun addr ->
    forget@@CT.(loadw s b mem !!addr)

  let load_word = word_loader ident
  let load_half = word_loader (fun s -> s / 2)
  let load_double = word_loader @@ ( * ) 2
  let load_quad = word_loader @@ ( * ) 4

  let load_bits t xs =
    let mem = CT.var @@ Theory.Target.data t in
    binary xs @@ fun sz x ->
    to_sort sz >>= fun s ->
    bitv x >>= fun x ->
    forget@@CT.(loadw s (is_big_endian t) mem !!x)

  let store_byte t xs =
    binary xs @@ fun dst data ->
    bitv dst >>= fun dst ->
    bitv data >>= fun data ->
    let mem = Theory.Target.data t in
    let byte = Theory.Mem.vals (Theory.Var.sort mem) in
    let (:=) = CT.set in
    CT.(mem := store (var mem) !!dst (low byte !!data))

  let store_word t xs =
    binary xs @@ fun dst data ->
    bitv dst >>= fun dst ->
    bitv data >>= fun data ->
    let mem = Theory.Target.data t in
    let (:=) = CT.set in
    let b = is_big_endian t in
    CT.(mem := storew b (var mem) !!dst !!data)

  let rec prefix p = function
    | [] -> []
    | x::xs -> (p,x) :: prefix p xs

  let rec combinations = function
    | [] -> []
    | x :: xs -> prefix x xs @ combinations xs


  let distinct_pair (x,y) =
    bitv x >>= fun x ->
    bitv y >>= fun y ->
    match const x, const y with
    | Some x, Some y -> const_bool Bitvec.(x <> y)
    | _ -> CT.neq !!x !!y

  let distinct = function
    | [] | [_] -> true_
    | xs ->
      true_ >>= fun init ->
      KB.List.fold (combinations xs) ~init ~f:(fun t p ->
          distinct_pair p >>= fun t' ->
          t &&& t')

  let neg x =
    bitv x >>= fun x -> match const x with
    | None -> forget@@CT.neg !!x
    | Some v -> forget@@const_int (sort x) v

  let apply_static s x =
    let m = Bitvec.modulus (size s) in
    forget@@const_int s Bitvec.(x mod m)

  let lnot x =
    bitv x >>= fun x -> match const x with
    | None -> forget@@CT.not !!x
    | Some v ->
      apply_static (sort x) (Bitvec.lnot v)

  let one_op_x sop dop x =
    bitv x >>= fun x -> match const x with
    | None -> forget@@CT.(dop (int (sort x) Bitvec.one) !!x)
    | Some v -> apply_static (sort x) (sop v Bitvec.one)

  let reciprocal = one_op_x Bitvec.div CT.div
  let sreciprocal = one_op_x Bitvec.sdiv CT.sdiv

  let get_pc s lbl =
    KB.collect Theory.Label.addr lbl >>= function
    | None -> !!(empty s)
    | Some addr -> forget@@const_int s addr

  let require_symbol v k =
    match symbol v with
    | Some name -> k name
    | None -> illformed "not a symbolic value"

  let possibly_register target v =
    require_symbol v @@ fun v ->
    match Theory.Target.var target v with
    | Some v -> !!v
    | None ->
      let s = Theory.Bitv.define (Theory.Target.bits target) in
      KB.return @@ Theory.Var.forget (Theory.Var.define s v)

  let set_symbol t v x =
    let* var = possibly_register t v in
    let x = KB.Value.refine x (Theory.Var.sort var) in
    CT.set var !!x

  let alias_base_register target v =
    let* r = possibly_register target v in
    match Theory.Target.unalias target r with
    | None ->
      illformed "the register %a is not an alias in %a"
        Theory.Var.pp r Theory.Target.pp target
    | Some origin -> match Theory.Origin.cast_sub origin with
      | None ->
        illformed "the aliased register %a is not a subregister"
          Theory.Var.pp r
      | Some origin ->
        let reg = Theory.Origin.reg origin in
        let name = Theory.Var.name reg in
        forget @@
        CT.var reg >>| fun v ->
        KB.Value.put Primus.Lisp.Semantics.symbol v (Some name)

  module Intrinsic = struct
    type param =
      | Inputs
      | Result
      | Writes
      | Stores
      | Aborts
    [@@deriving equal, compare, sexp, variants]

    type arg = unit Theory.value

    let sexp_of_arg v =
      Sexp.Atom (Format.asprintf "%a" KB.Value.pp v)

    type args = (param * arg list) list [@@deriving sexp_of]

    let params = [
      (* skip inputs as they are passed directly *)
      ":result", result;
      ":writes", writes;
      ":stores", stores;
      ":aborts", aborts;
    ]


    let parse_param input =
      Option.(symbol input >>=
              List.Assoc.find ~equal:String.equal params)

    let parse_args args =
      List.fold args ~init:([],[],Inputs) ~f:(fun (parsed,current,state) v ->
          match parse_param v with
          | Some state' ->
            let parsed = (state,List.rev current) :: parsed in
            (parsed,[],state')
          | None -> (parsed,v::current,state))
      |> fun (parsed,last,state) ->
      List.rev ((state, List.rev last) :: parsed)

    let get kw args =
      List.Assoc.find ~equal:equal_param args kw |> function
      | None -> []
      | Some args -> args

    let mk_var d i s =
      Theory.Var.define s (sprintf "intrinsic:%c%d" d i)

    let ivar = mk_var 'x'
    let ovar = mk_var 'y'

    let assign_inputs args =
      seq@@List.mapi (get inputs args) ~f:(fun i x ->
          let s = Theory.Value.sort x in
          CT.set (ivar i s) !!x)

    let invoke_symbol name =
      let name = KB.Name.(unqualified@@read name) in
      let* dst = Theory.Label.for_name (sprintf "intrinsic:%s" name) in
      KB.provide Theory.Label.is_subroutine dst (Some true) >>= fun () ->
      CT.goto dst

    (* starts a new group on each symbol *)
    let group_by_symbols =
      List.group ~break:(fun _ v -> Option.is_some (symbol v))

    let write_single t i v =
      require_symbol v @@ fun v ->
      match Theory.Target.var t v with
      | None -> illformed ":writes argument is not a register"
      | Some v ->
        let s = Theory.Var.sort v in
        CT.set (ovar i s) (CT.var v)

    let write_typed t i v =
      require_symbol v @@ fun v ->
      let* t = static t in
      let s = Theory.Value.Sort.forget@@Theory.Bitv.define t in
      let v = Theory.Var.define s v in
      CT.set (ovar i s) (CT.var v)

    let group what args = group_by_symbols@@get what args
    let result = get result
    let writes = group writes
    let stores = group stores

    let assign_writes t xs =
      let base = List.length (result xs) in
      seq@@List.mapi (writes xs) ~f:(fun i -> function
          | [reg] -> write_single t (base+i) reg
          | [reg; typ] -> write_typed typ (base+i) reg
          | _ -> illformed "incorrect :writes parameters")

    let mk_store size t i ptr =
      let s = Theory.Value.Sort.forget@@Theory.Bitv.define size in
      let* v = CT.var (ovar i s) in
      store_word t [ptr; v]

    let store_word t =
      mk_store (Theory.Target.data_addr_size t) t

    let store t i ptr typ =
      let* typ = static typ in
      mk_store typ t i ptr

    let assign_stores t xs =
      let base = List.length (result xs) + List.length (writes xs) in
      seq@@List.mapi (stores xs) ~f:(fun i -> function
          | [ptr] -> store_word t (base+i) ptr
          | [ptr; typ] -> store t (base+i) ptr typ
          | _ -> illformed "incorrect :stores parameters")

    let make_result size =
      let* size = static size in
      let s = Theory.Value.Sort.forget@@Theory.Bitv.define size in
      CT.var@@ovar 0 s


    let call t name args =
      require_symbol name @@ fun name ->
      let args = parse_args args in
      let eff = seq [
          data@@assign_inputs args;
          ctrl@@invoke_symbol name;
          data@@assign_writes t args;
          data@@assign_stores t args;
        ] in
      match result args with
      | [] -> eff
      | [t] -> full eff (make_result t)
      | _ -> illformed ":result may occur once and with a single argument"
  end


  let make_symbol s name =
    intern name >>= const_int s >>| set_sym name |> forget

  let symbol_concat s syms =
    List.map syms ~f:symbol |> Option.all |> function
    | None -> illformed "require all symbols"
    | Some syms ->
      let syms,sep =
        List.split_while syms ~f:(Fn.non@@String.equal ":sep") in
      let* sep = match sep with
        | [] -> KB.return ""
        | [_; sep] -> KB.return sep
        | _ -> illformed ":sep must be last and followed by an argument" in
      make_symbol s (String.concat ~sep syms)

  let symbol s v =
    match symbol v with
    | Some name -> make_symbol s name
    | None ->
      illformed "symbol requires a symbolic value"


  let is_symbol v =
    forget@@match KB.Value.get Primus.Lisp.Semantics.symbol v with
    | Some _ -> true_
    | _ -> false_

  let is_keyword = String.is_prefix ~prefix:":"

  let special dst =
    require_symbol dst @@ fun dst ->
    if is_keyword dst then
      let* dst = Theory.Label.for_name ("special"^dst) in
      CT.goto dst
    else illformed "special requires a keyword as the tag, e.g., :hlt"

  let invoke_subroutine dst =
    require_symbol dst @@ fun dst ->
    let* dst = Theory.Label.for_name dst in
    CT.goto dst

  let mk_cast t cast xs =
    binary xs @@ fun sz x ->
    to_sort sz >>= fun s ->
    bitv x >>= fun x -> match const x with
    | None -> forget@@cast s !!x
    | Some v ->
      let r = size s in
      let w = size @@ Theory.Value.sort x in
      forget@@const_int s@@match t with
      | `hi -> Bitvec.extract ~hi:(w-1) ~lo:(w-r) v
      | `lo -> Bitvec.extract ~hi:r ~lo:0 v
      | `se ->
        let signed = Bitvec.(msb v mod modulus w) in
        if signed && w < r then
          let open Bitvec.Make(struct
              let modulus = Bitvec.modulus r
            end) in
          (ones lsl int w) lor v
        else Bitvec.extract ~hi:r ~lo:0 v

  let signed = mk_cast `se CT.signed
  let unsigned = mk_cast `lo CT.unsigned
  let low = mk_cast `lo CT.low
  let high = mk_cast `hi CT.high

  let extract xs =
    ternary xs @@ fun hi lo x ->
    to_int hi >>= fun hi ->
    to_int lo >>= fun lo ->
    bitv x >>= fun x ->
    match hi,lo with
    | None,_|_,None ->
      illformed "extract: bit numbers must be statically known"
    | Some hi, Some lo ->
      if hi < lo
      then illformed "extract: high must be no les the low"
      else
        let s = Theory.Bitv.define (hi-lo+1) in
        forget@@match const x with
        | None ->
          CT.extract s (int s hi) (int s lo) !!x
        | Some x ->
          const_int s @@ Bitvec.extract ~hi ~lo x

  let concat xs = nbitv xs >>= function
    | [] -> forget@@int (Theory.Bitv.define 1) 0
    | x :: xs ->
      forget @@
      KB.List.fold xs ~init:x ~f:(fun x y ->
          let sx = Theory.(Bitv.size@@Value.sort x)
          and sy = Theory.(Bitv.size@@Value.sort y) in
          let sz = Theory.Bitv.define (sx + sy) in
          match const x, const y with
          | Some x, Some y ->
            const_int sz @@ Bitvec.append sx sy x y
          | _ -> CT.append sz !!x !!y)

  let select s xs = match List.rev xs with
    | [] -> forget@@int s 0
    | x::bs ->
      bitv x >>= fun x ->
      KB.List.map (List.rev bs) ~f:static >>= fun bs ->
      let rs = Theory.Bitv.define (List.length bs) in
      forget@@match const x with
      | Some x -> const_int rs @@ Bitvec.select bs x
      | None -> match bs with
        | [] -> assert false
        | b :: bs ->
          let b1 = Theory.Bitv.define 1 in
          CT.extract b1 (int s b) (int s b) !!x >>= fun init ->
          KB.List.fold bs ~init ~f:(fun r b ->
              let n = Theory.(Bitv.size @@ Value.sort r) in
              let s = Theory.Bitv.define (n+1) in
              CT.append s !!r
                (CT.extract b1 (int s b) (int s b) !!x))

  let bits = Theory.Target.bits target

  let s = Theory.Bitv.define bits

  module SBitvec = struct
    let compare x y =
      let module Bitv = Bitvec.Make(struct
          let modulus = Bitvec.modulus (Theory.Bitv.size s)
        end) in
      let x_is_neg = Bitv.msb x and y_is_neg = Bitv.msb y in
      match x_is_neg, y_is_neg with
      | true,false -> -1
      | false,true -> 1
      | _ -> Bitvec.compare x y

    let (<) x y = compare x y < 0
    let (>) x y = compare x y > 0
    let (=) x y = compare x y = 0
    let (<=) x y = compare x y <= 0
    let (>=) x y = compare x y >= 0
  end

  let dispatch lbl name args =
    let t = target in
    match name,args with
    | "+",_-> pure@@monoid s join Z.add CT.add (Z.int 0) args
    | "-",[x]|"neg",[x] -> pure@@neg x
    | "-",_-> pure@@monoid s join Z.sub CT.sub (Z.int 0) args
    | "*",_-> pure@@monoid s join Z.mul CT.mul (Z.int 1) args
    | "/",[x]-> pure@@reciprocal x
    | "/",_-> pure@@monoid s join Z.div CT.div (Z.int 1) args
    | "s/",[x]-> pure@@sreciprocal x
    | "s/",_-> pure@@monoid s join Z.sdiv CT.sdiv (Z.int 1) args
    | "mod",_-> pure@@monoid s join Z.rem CT.modulo (Z.int 1) args
    | "lnot",[x] -> pure@@lnot x
    | "signed-mod",_-> pure@@monoid s join Z.srem CT.smodulo (Z.int 1) args
    | "lshift",_-> pure@@monoid s first Z.lshift CT.lshift (Z.int 1) args
    | "rshift",_-> pure@@monoid s first Z.rshift CT.rshift (Z.int 1) args
    | "arshift",_-> pure@@monoid s first Z.arshift CT.arshift (Z.int 1) args
    | "logand",_-> pure@@monoid s join Z.logand CT.logand (Z.int 1) args
    | "logor",_-> pure@@monoid s join Z.logor CT.logor (Z.int 0) args
    | "logxor",_-> pure@@monoid s join Z.logxor CT.logxor (Z.int 0) args
    | "=",_-> pure@@order Bitvec.(=) CT.eq args
    | "<",_-> pure@@order Bitvec.(<) CT.ult args
    | "s<",_ -> pure@@order SBitvec.(<) CT.slt args
    | ">",_-> pure@@order Bitvec.(>) CT.ugt args
    | "s>",_ -> pure@@order SBitvec.(>) CT.sgt args
    | "<=",_-> pure@@order Bitvec.(<=) CT.ule args
    | ">=",_-> pure@@order Bitvec.(>=) CT.uge args
    | "s<=",_-> pure@@order SBitvec.(<=) CT.ule args
    | "s>=",_-> pure@@order SBitvec.(>=) CT.uge args
    | "/=",_| "distinct",_-> pure@@forget@@distinct args
    | "is-zero",_| "not",_-> pure@@all s join s_is_zero CT.is_zero args
    | "is-positive",_-> pure@@all s join s_is_positive d_is_positive args
    | "is-negative",_-> pure@@all s join s_is_negative d_is_negative args
    | "word-width",_-> pure@@word_width s args
    | "exec-addr",_-> ctrl@@exec_addr args
    | "goto-subinstruction",_ -> ctrl@@goto_subinstruction lbl args
    | ("load-byte"|"memory-read"),_-> pure@@load_byte t args
    | "load-word",_-> pure@@load_word t args
    | "load-hword",_-> pure@@load_half t args
    | "load-qword",_-> pure@@load_quad t args
    | "load-dword",_-> pure@@load_double t args
    | "load-bits",_-> pure@@load_bits t args
    | ("store-byte"|"memory-write"),_-> data@@store_byte t args
    | "store-word",_-> data@@store_word t args
    | "get-program-counter",[]
    | "get-current-program-counter",[] -> pure@@get_pc s lbl
    | "set-symbol-value",[sym;x] -> data@@set_symbol t sym x
    | "symbol-concat",syms -> pure@@symbol_concat s syms
    | "symbol",[x] ->  pure@@symbol s x
    | "is-symbol", [x] -> pure@@is_symbol x
    | "alias-base-register", [x] -> pure@@alias_base_register t x
    | "cast-low",xs -> pure@@low xs
    | "cast-high",xs -> pure@@high xs
    | "cast-signed",xs -> pure@@signed xs
    | "cast-unsigned",xs -> pure@@unsigned xs
    | "extract",xs -> pure@@extract xs
    | "concat", xs -> pure@@concat xs
    | ("select"|"nth"),xs -> pure@@select s xs
    | "empty",[] -> nop ()
    | "intrinsic",(dst::args) -> Intrinsic.call t dst args
    | "special",[dst] -> ctrl@@special dst
    | "invoke-subroutine",[dst] -> ctrl@@invoke_subroutine dst
    | _ -> !!nothing
end

module Lisp = Primus.Lisp.Semantics

let provide () =
  List.iter export ~f:(fun (name,types,docs) ->
      Primus.Lisp.Semantics.declare ~types ~docs ~package:"core" name
        ~body:(fun target ->
            Theory.instance () >>= Theory.require >>= fun (module CT) ->
            let module Target = struct let target = target end in
            let module P = Primitives(CT)(Target) in
            KB.return @@ fun obj args ->
            KB.catch (P.dispatch obj name args) @@ function
            | Illformed err ->
              let name = KB.Name.create ~package:"core" name in
              KB.fail (Failed_primitive (name,args,err))
            | other -> KB.fail other))
