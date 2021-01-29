open Core_kernel
open Bap_core_theory
open Bap_primus.Std
open KB.Syntax
open KB.Let

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
    "(neg X) returns the negation of X. Same as (- X).";

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

    "is-zero", all any @-> any,
    "(is-zero X Y ... Z) returns one if all numbers are zero.";

    "not", all any @-> any,
    "(not X Y ... Z) returns one if all numbers are not \
     true. Equivalent to (is-zero X Y Z)";

    "is-positive", all any @-> any,
    "(is-zero X Y ... Z) returns one if all numbers are positive.";

    "is-negative", all any @-> any,
    "(is-zero X Y ... Z) returns one if all numbers are negative.";

    "word-width", all any @-> any,
    "(word-width X Y ... Z) returns the maximum width of its \
     arguments. If no arguments provided returns the size of the \
     machine word.";

    "exec-addr", one int @-> any,
    "(exec-addr ADDR) transfers control flow to ADDR.";

    "load-byte", one int @-> byte,
    "(load-byte PTR) loads one byte from the address PTR";

    "load-word", one int @-> int,
    "(load-word PTR) loads one word from the address PTR";

    "load-bits", tuple [any; int] @-> int,
    "(load-word SIZE PTR) loads a SIZE-bit long word from the address PTR";

    "load-half", one int @-> int,
    "(load-half PTR) loads half-word from the address PTR";

    "store-byte", tuple[int; any] @-> any,
    "(store-byte POS VAL) stores byte VAL at the memory position POS";

    "store-word", tuple[int; any] @-> any,
    "(store-word POS VAL) stores VAL at the memory position POS";

    "memory-write", tuple [int; byte] @-> int,
    "(memory-write PTR X) stores X at PTR.";

    "get-program-counter", unit @-> int,
    "(get-program-counter) returns the address of the current instruction";

    "get-current-program-counter", unit @-> int,
    "(get-current-program-counter) is an alias to (get-program-counter)";

    "set-symbol-value", tuple [sym; a] @-> a,
    "(set-symbol-value S X) sets the value of the symbol S to X.
         Returns X";

    "cast-low", tuple [int; a] @-> b,
    "(cast-low S X) extracts low S bits from X.";

    "cast-high", tuple [int; a] @-> b,
    "(cast-high S X) extracts high S bits from X.";

    "cast-signed", tuple [int; a] @-> b,
    "(cast-signed S X) performs signed extension of X to the size of S bits";

    "cast-unsigned", tuple [int; a] @-> b,
    "(cast-unsigned S X) performs unsigned extension of X to the size of S bits";
  ]

type KB.conflict += Illformed of string

let illformed fmt =
  Format.kasprintf (fun msg ->
      KB.fail (Illformed msg)) fmt

let () = KB.Conflict.register_printer (function
    | Illformed msg-> Some ("illformed lisp program " ^ msg)
    | _ -> None)


let domain = KB.Domain.optional "cst"
    ~equal:Sexp.equal
    ~inspect:ident

let eslot = KB.Class.property Theory.Semantics.cls "eff"
    ~package:"core"
    ~public:true
    domain

let pslot = KB.Class.property Theory.Value.cls "val"
    ~package:"core"
    ~public:true
    domain

let nothing = KB.Value.empty Theory.Semantics.cls
let size = Theory.Bitv.size
let forget x = x >>| Theory.Value.forget
let empty s = Theory.Value.(forget @@ empty s)
let fresh = KB.Object.create Theory.Program.cls
let sort = Theory.Value.sort
let bits x = size @@ sort x

module Primitives(CT : Theory.Core) = struct

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
  let set_const v x =
    KB.Value.put Primus.Lisp.Semantics.static v (Some x)
  let const_int s x = CT.int s x >>| fun v -> set_const v x
  let int s x = const_int s @@ Bitvec.(int x mod modulus (size s))
  let true_ = CT.b1 >>| fun v -> set_const v Bitvec.one
  let false_ = CT.b0 >>| fun v -> set_const v Bitvec.zero
  let const_bool x = if x then true_ else false_

  let bitv x =
    match Theory.Value.resort Theory.Bitv.refine x with
    | Some x -> !!x
    | None -> match Theory.Value.resort Theory.Bool.refine x with
      | None -> illformed "defined for bits or bools"
      | Some b ->
        let s = Theory.Bitv.define 1 in
        let b1 = const_int s Bitvec.M1.one
        and b0 = const_int s Bitvec.M1.zero in
        match const b with
        | Some r ->
          if Bitvec.(equal r zero)
          then b0
          else b1
        | None -> CT.ite !!b b1 b0


  let nbitv = KB.List.map ~f:bitv

  type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

  let monoid s sf df init xs =
    nbitv xs >>= function
    | [] -> forget@@const_int s init
    | x :: xs ->
      KB.List.fold ~init:x xs ~f:(fun res x ->
          match const res, const x with
          | Some res, Some x ->
            const_int s@@sf res x
          | _ ->
            CT.cast s CT.b0 !!x >>= fun x ->
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

  let all sf df xs =
    true_ >>= fun init ->
    KB.List.fold ~init xs ~f:(fun r x ->
        bitv x >>= fun x ->
        let r' = match const x with
          | Some x -> const_bool (sf x)
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
    let* lbl = fresh in
    CT.blk lbl (seq []) eff

  let data eff =
    let* lbl = fresh in
    CT.blk lbl eff (seq [])

  let memory eff res =
    let* lbl = fresh in
    full CT.(blk lbl (perform eff) skip) res

  let loads = memory Theory.Effect.Sort.rmem
  let stores = memory Theory.Effect.Sort.wmem
  let loads = pure

  let is_negative x = CT.msb x
  let is_positive x =
    CT.(and_ (non_zero x) (inv (is_negative x)))

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

  let load_byte t xs =
    let mem = CT.var @@ Theory.Target.data t in
    unary xs >>= bitv >>= fun addr -> forget@@CT.(load mem !!addr)


  let is_big_endian t =
    if Theory.Endianness.equal (Theory.Target.endianness t)
        Theory.Endianness.eb then CT.b1 else CT.b0

  let to_sort x =
    bitv x >>| const >>= function
    | None -> illformed "the sort specification must be static"
    | Some sz ->
      if Bitvec.fits_int sz then
        !!(Theory.Bitv.define (Bitvec.to_int sz))
      else illformed "cast size must fit into it"

  let load_word t xs =
    let mem = CT.var @@ Theory.Target.data t in
    let s = Theory.Bitv.define (Theory.Target.bits t) in
    let b = is_big_endian t in
    unary xs >>= bitv >>= fun addr ->
    forget@@CT.(loadw s b mem !!addr)

  let loadn t xs =
    let mem = CT.var @@ Theory.Target.data t in
    binary xs @@ fun sz x ->
    to_sort sz >>= fun s ->
    bitv x >>= fun x ->
    forget@@CT.(loadw s (is_big_endian t) mem !!x)

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
    let (:=) = CT.set in
    CT.(mem := store (var mem) !!dst !!data)

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

  let one_op_x op x =
    bitv x >>= fun x -> match const x with
    | None -> forget@@CT.(op (int (sort x) Bitvec.one) !!x)
    | Some v -> forget@@const_int (sort x) v

  let reciprocal = one_op_x CT.div
  let sreciprocal = one_op_x CT.sdiv

  let get_pc s lbl =
    KB.collect Primus.Lisp.Semantics.definition lbl >>= function
    | None -> !!(empty s)
    | Some lbl -> KB.collect Theory.Label.addr lbl >>= function
      | None -> !!(empty s)
      | Some addr -> forget@@const_int s addr

  let get_symbol sym =
    match KB.Value.get Primus.Lisp.Semantics.symbol sym with
    | Some sym -> sym
    | None -> match KB.Value.get pslot sym with
      | Some (Atom name) -> name ^ ":symbol"
      | _ -> "#unknown:symbol"


  let set_symbol sym x =
    let sym = get_symbol sym in
    bitv x >>= fun x ->
    let s = sort x in
    let var = Theory.Var.define s sym in
    CT.set var !!x

  let mk_cast cast xs =
    binary xs @@ fun sz x ->
    to_sort sz >>= fun s ->
    bitv x >>= fun x ->
    forget@@cast s !!x

  let signed = mk_cast CT.signed
  let unsigned = mk_cast CT.unsigned
  let low = mk_cast CT.low
  let high = mk_cast CT.high

  let dispatch lbl name args =
    Theory.Label.target lbl >>= fun t ->
    let bits = Theory.Target.bits t in
    let module Z = struct
      include Bitvec.Make(struct
          let modulus = Bitvec.modulus bits
        end)
      let is_zero = Bitvec.equal zero
      let is_negative = msb
      let is_positive x =
        not (is_negative x) && not (is_zero x)
    end in
    let s = Theory.Bitv.define bits in
    match name,args with
    | "+",_-> pure@@monoid s Z.add CT.add Z.zero args
    | "-",[x]|"neg",[x] -> pure@@neg x
    | "-",_-> pure@@monoid s Z.sub CT.sub Z.zero args
    | "*",_-> pure@@monoid s Z.mul CT.mul Z.one args
    | "/",[x]-> pure@@reciprocal x
    | "/",_-> pure@@monoid s Z.div CT.div Z.one args
    | "s/",[x]-> pure@@sreciprocal x
    | "s/",_-> pure@@monoid s Z.sdiv CT.sdiv Z.one args
    | "mod",_-> pure@@monoid s Z.rem CT.modulo Z.one args
    | "signed-mod",_-> pure@@monoid s Z.srem CT.smodulo Z.one args
    | "lshift",_-> pure@@monoid s Z.lshift CT.lshift Z.one args
    | "rshift",_-> pure@@monoid s Z.rshift CT.rshift Z.one args
    | "arshift",_-> pure@@monoid s Z.arshift CT.arshift Z.one args
    | "logand",_-> pure@@monoid s Z.logand CT.logand Z.ones args
    | "logor",_-> pure@@monoid s Z.logor CT.logor Z.zero args
    | "logxor",_-> pure@@monoid s Z.logxor CT.logxor Z.zero args
    | "=",_-> pure@@order Bitvec.(=) CT.eq args
    | "<",_-> pure@@order Bitvec.(<) CT.ult args
    | ">",_-> pure@@order Bitvec.(>) CT.ugt args
    | "<=",_-> pure@@order Bitvec.(<=) CT.ule args
    | ">=",_-> pure@@order Bitvec.(>=) CT.uge args
    | "/=",_| "distinct",_-> pure@@forget@@distinct args
    | "is-zero",_| "not",_-> pure@@all Bitvec.(equal zero) CT.is_zero args
    | "is-positive",_-> pure@@all Z.is_positive is_positive args
    | "is-negative",_-> pure@@all Z.is_negative is_negative args
    | "word-width",_-> pure@@word_width s args
    | "exec-addr",_-> ctrl@@exec_addr args
    | ("load-byte"|"memory-read"),_-> pure@@load_byte t args
    | "load-word",_-> pure@@load_word t args
    | "load-bits",_-> pure@@load_bits t args
    | ("store-byte"|"memory-write"),_-> data@@store_byte t args
    | "store-word",_-> data@@store_word t args
    | "get-program-counter",[]
    | "get-current-program-counter",[] -> pure@@get_pc s lbl
    | "set-symbol-value",[sym;x] -> data@@set_symbol sym x
    | "cast-low",xs -> pure@@low xs
    | "cast-high",xs -> pure@@high xs
    | "cast-signed",xs -> pure@@signed xs
    | "cast-unsigned",xs -> pure@@unsigned xs
    | _ -> !!nothing
end


module CST : Theory.Core = struct
  type t = Sexp.t


  let pure s cst = KB.Value.put pslot (Theory.Value.empty s) (Some cst)
  let eff s cst = KB.Value.put eslot (Theory.Effect.empty s) (Some cst)
  let data = eff Theory.Effect.Sort.bot
  let ctrl = eff Theory.Effect.Sort.bot
  let ret = KB.return
  let atom s = Sexp.Atom s

  let list = function
    | [Sexp.Atom op;
       List ((Atom opx) :: xs);
       List ((Atom opy) :: ys)]
      when String.(op = opx && op = opy) ->
      Sexp.List (Atom op :: List.append xs ys)
    | [Sexp.Atom op; List ((Atom opx) :: xs); y]
      when String.(op = opx) ->
      Sexp.List (Sexp.Atom op :: xs@[y])
    | [Sexp.Atom op; x; List ((Atom opy) :: ys)]
      when String.(op = opy) ->
      Sexp.List (Sexp.Atom op :: x :: ys)
    | xs -> Sexp.List xs

  let app x xs = list (atom x :: xs)

  let psort = Theory.Value.sort
  let esort = Theory.Effect.sort

  let (>>->) x f =
    x >>= fun v ->
    let s = psort v in
    f s (KB.Value.get pslot v)

  let (>>|>) x f = x >>-> fun s v -> ret (f s v)

  let (>>->?) x f =
    x >>= fun v ->
    let s = psort v in
    match KB.Value.get pslot v with
    | None -> ret (Theory.Value.empty s)
    | Some v -> f s v


  let (>>|>?) x f = x >>->? fun s v -> ret (f s v)

  let (>>=>) x f =
    x >>= fun v ->
    let s = esort v in
    f s (KB.Value.get eslot v)

  let (>>=>?) x f =
    x >>= fun v ->
    let s = esort v in
    match KB.Value.get eslot v with
    | None -> ret (Theory.Effect.empty s)
    | Some x -> f s x


  let empty = Theory.Value.empty

  let unary_s s op x = x >>|> fun _ -> function
    | None -> empty s
    | Some v -> pure s @@ app op [v]

  let unary op x = x >>|>? fun s v -> pure s @@ app op [v]

  let monoid_s s op x y =
    x >>-> fun _ x ->
    y >>|> fun _ y ->
    match x, y with
    | Some x, Some y -> pure s @@ app op [x; y]
    | _ -> empty s

  let monoid op x y =
    x >>->? fun s x ->
    y >>|>? fun _ y ->
    pure s @@ app op [x; y]

  module Minimal = struct
    let b0 = ret@@pure Theory.Bool.t (atom "0")
    let b1 = ret@@pure Theory.Bool.t (atom "1")

    let unk s = ret@@empty s

    let var v =
      let s = Theory.Var.sort v in
      ret@@pure s@@atom (Theory.Var.name v)

    let let_ v x y =
      x >>-> fun _ x ->
      y >>|> fun s y ->
      let name = Theory.Var.name v in
      match x,y with
      | Some x, Some y ->
        pure s@@app "let" [atom name; x; y]
      | _ -> empty s

    let ite c x y =
      c >>-> fun _ c ->
      x >>->? fun s x ->
      y >>|>? fun _ y -> match c with
      | None -> empty s
      | Some c -> pure s@@app "ite" [c; x; y]

    let inv = unary "not"
    let and_ = monoid "logand"
    let or_ = monoid "logor"
    let int s x = ret@@pure s@@atom (Bitvec.to_string x)
    let msb x = unary_s Theory.Bool.t "msb" x
    let lsb x = unary_s Theory.Bool.t "lsb" x
    let neg x = unary "-" x
    let not x = unary "not" x
    let add x = monoid "+" x
    let sub x = monoid "-" x
    let mul x = monoid "*" x
    let div x = monoid "/" x
    let sdiv x = monoid "s/" x
    let modulo x = monoid "mod" x
    let smodulo x = monoid "signed-mod" x
    let logand x = monoid "logand" x
    let logor x = monoid "logor" x
    let logxor x = monoid "logxor" x

    let genshift name fill x off =
      fill >>-> fun _ fill ->
      x >>-> fun s x ->
      off >>|> fun _ off ->
      match fill, x, off with
      | Some fill, Some x, Some off ->
        pure s @@ app name [fill; x; off]
      | _ -> empty s
    let shiftr x = genshift "shiftr" x
    let shiftl x = genshift "shiftl" x
    let sle x = monoid_s Theory.Bool.t "s<=" x
    let ule x = monoid_s Theory.Bool.t "<" x


    let cast s fill exp =
      fill >>-> fun _ fill ->
      exp >>|> fun s' x ->
      let ct = sprintf "%d" @@ Theory.Bitv.size s in
      match fill, x  with
      | Some fill, Some x ->
        if Theory.Value.Sort.same s s'
        then pure s x
        else
          pure s@@list [
            atom "cast";
            atom ct;
            fill;
            x
          ]
      | _ -> empty s

    let concat s xs =
      List.map xs ~f:(fun x -> x >>|> fun _ -> ident) |>
      KB.List.all >>| Option.all >>| function
      | None -> empty s
      | Some xs -> pure s @@ app "concat" xs

    let append s x y = monoid_s s "append" x y

    let load m x =
      m >>-> fun s m ->
      x >>|> fun _ x ->
      let s = Theory.Mem.vals s in
      match m, x with
      | Some m, Some x -> pure s @@ app "load" [m; x]
      | _ -> empty s

    let store m p x =
      m >>-> fun s m ->
      p >>-> fun _ p ->
      x >>|> fun _ x ->
      match m, p, x with
      | Some m, Some p, Some x ->
        pure s @@ app "store" [m; p; x]
      | _ -> empty s

    let nil = Theory.Effect.empty Theory.Effect.Sort.bot

    let perform eff = ret (Theory.Effect.empty eff)

    let set v x = x >>|> fun _ x ->
      match x with
      | None -> nil
      | Some x -> data@@app "set" [
          atom (Theory.Var.name v);
          x
        ]

    let jmp x = x >>|> fun _ x -> match x with
      | None -> nil
      | Some x -> ctrl@@app "goto" [x]

    let goto dst =
      KB.collect Theory.Label.addr dst >>= function
      | Some dst ->
        ret@@ctrl@@app "goto" [atom (Bitvec.to_string dst)]
      | None ->
        KB.Object.repr Theory.Program.cls dst >>= fun dst ->
        ret@@ctrl@@app "goto" [atom dst]

    let both s xs ys =
      match xs,ys with
      | None,None -> ret nil
      | Some r,None
      | None, Some r -> ret@@eff s r
      | Some xs, Some ys ->
        ret@@eff s@@list [xs; ys]

    let seq xs ys =
      xs >>=> fun s xs ->
      ys >>=> fun _ ys ->
      both s xs ys

    let blk _ xs ys =
      xs >>=> fun _ xs ->
      ys >>=> fun _ ys ->
      both Theory.Effect.Sort.top xs ys

    let repeat cnd body =
      cnd >>-> fun _ cnd ->
      body >>=>? fun s body ->
      match cnd with
      | None -> ret@@nil
      | Some cnd ->
        ret@@eff s@@app "while" [cnd; body]

    let branch cnd yes nay =
      cnd >>-> fun _ cnd ->
      yes >>=>? fun s yes ->
      nay >>=>? fun _ nay ->
      match cnd with
      | None -> ret@@nil
      | Some cnd ->
        ret@@eff s@@app "if" [cnd; yes; nay]
  end
  include Theory.Basic.Make(Minimal)

  let mk_cast name s x =
    x >>|> fun s' x -> match x with
    | None -> empty s
    | Some x ->
      if Theory.Value.Sort.same s s'
      then pure s x
      else pure s@@app name [
          atom@@sprintf "%d" (Theory.Bitv.size s);
          x
        ]

  let high s = mk_cast "high" s
  let low s = mk_cast "low" s
  let signed s = mk_cast "signed" s
  let unsigned s = mk_cast "unsigned" s

  let extract s lo hi x =
    lo >>-> fun _ lo ->
    hi >>-> fun _ hi ->
    x >>|> fun _ x ->
    match lo,hi,x with
    | Some lo, Some hi, Some x ->
      pure s@@app "extract" [lo; hi; x]
    | _ -> empty s

  let loadw s dir mem ptr =
    dir >>-> fun _ dir ->
    mem >>-> fun _ mem ->
    ptr >>|> fun _ ptr ->
    match dir,mem,ptr with
    | Some dir, Some mem, Some ptr ->
      pure s@@app "loadw" [
        atom@@sprintf "%d" (Theory.Bitv.size s);
        dir;
        mem;
        ptr
      ]
    | _ -> empty s

  let storew dir mem ptr exp =
    dir >>-> fun _ dir ->
    mem >>-> fun s mem ->
    ptr >>-> fun _ ptr ->
    exp >>|> fun _ exp ->
    match Option.all [dir; mem; ptr; exp] with
    | Some args -> pure s@@app "storew" args
    | _ -> empty s

  let mk_shift name x m =
    x >>->? fun s x ->
    m >>|> fun _ -> function
    | None -> empty s
    | Some m -> pure s@@app name [x; m]

  let arshift x = mk_shift "arshift" x
  let rshift x = mk_shift "rshift" x
  let lshift x = mk_shift "lshift" x
  let eq x = monoid_s Theory.Bool.t "=" x
  let neq x = monoid_s Theory.Bool.t "/=" x
  let slt x = monoid_s Theory.Bool.t "s<" x
  let ult x = monoid_s Theory.Bool.t "<" x
  let sgt x = monoid_s Theory.Bool.t "s>" x
  let ugt x = monoid_s Theory.Bool.t ">" x
  let sge x = monoid_s Theory.Bool.t "s>=" x
  let uge x = monoid_s Theory.Bool.t ">=" x

  let asort s =
    atom@@Format.asprintf "%a" Theory.Value.Sort.pp (Theory.Value.Sort.forget s)


  let wellknown = Theory.IEEE754.[
      binary16,  ":b16";
      binary32,  ":b32";
      binary64,  ":b64";
      binary80,  ":b80";
      binary128, ":b128";
      decimal32, ":d32";
      decimal64, ":d64";
      decimal128, ":d128";
    ]

  let format s =
    let s = Theory.Value.Sort.forget s in
    match Theory.Float.(refine s) with
    | None -> asort s
    | Some s ->
      List.find_map wellknown ~f:(fun (par,name) ->
          let s' = Theory.IEEE754.Sort.define par in
          if Theory.Value.Sort.same s s'
          then Some (atom name)
          else None) |> function
      | Some s -> s
      | None -> asort s



  let float s x =
    x >>|> fun _ x -> match x with
    | None -> empty s
    | Some x -> pure s@@app "float" [format s; x]

  let fbits x =
    x >>|> fun s x ->
    let s = Theory.Float.bits s in
    match x with
    | None -> empty s
    | Some x -> pure s@@app "fbits" [x]

  let is_finite x = unary_s Theory.Bool.t "is-finite" x
  let is_nan x = unary_s Theory.Bool.t "is-nan" x
  let is_inf x = unary_s Theory.Bool.t "is-inf" x
  let is_fzero x = unary_s Theory.Bool.t "is-fzero" x
  let is_fpos x = unary_s Theory.Bool.t "is-fpos" x
  let is_fneg x = unary_s Theory.Bool.t "is-fneg" x

  let rmode s = ret@@pure Theory.Rmode.t @@ atom s
  let rne = rmode ":rne"
  let rna = rmode ":rna"
  let rtp = rmode ":rtp"
  let rtn = rmode ":rtn"
  let rtz = rmode ":rtz"
  let requal = eq

  let mk_fcast name s m x =
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app name [m;x]
    | _ -> empty s

  let cast_float s = mk_fcast "cast-float" s
  let cast_sfloat s = mk_fcast "cast-sfloat" s
  let cast_int s = mk_fcast "cast-int" s
  let cast_sint s = mk_fcast "cast-sint" s

  let fneg x = unary "fneg" x
  let fabs x = unary "fabs" x

  let monoid_f name m x y =
    x >>->? fun s x ->
    y >>->? fun _ y ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x; y]

  let unary_f name m x =
    x >>->? fun s x ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x]

  let ternary_f name m x y z =
    x >>->? fun s x ->
    y >>->? fun _ y ->
    z >>->? fun _ z ->
    m >>|> fun _ m ->
    match m with
    | None -> empty s
    | Some m -> pure s@@app name [m; x; y; z]

  let binary_fi name m f x =
    f >>->? fun s f ->
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app name [m; f; x]
    | _ -> empty s


  let fadd m = monoid_f "+." m
  let fsub m = monoid_f "-." m
  let fmul m = monoid_f "*." m
  let fdiv m = monoid_f "/." m
  let fmodulo m = monoid_f "mod." m
  let fsqrt m = unary_f "fsqrt" m
  let fround m = unary_f "fround" m
  let fmad m = ternary_f "fmad" m
  let fsucc x = unary "fsucc" x
  let fpred x = unary "fpred" x
  let forder x = monoid_s Theory.Bool.t "forder" x

  let fconvert s m x =
    m >>-> fun _ m ->
    x >>|> fun _ x ->
    match m,x with
    | Some m, Some x -> pure s@@app "fconvert" [
        format s; m; x
      ]
    | _ -> empty s

  let pow m = monoid_f "pow" m
  let hypot m = monoid_f "hypot" m

  let rsqrt m = unary_f "rsqrt" m
  let exp m = unary_f "exp" m
  let expm1 m = unary_f "expm1" m
  let exp2 m = unary_f "exp2" m
  let exp2m1 m = unary_f "exp2m1" m
  let exp10 m = unary_f "exp10" m
  let exp10m1 m = unary_f "exp10m1" m
  let log m = unary_f "log" m
  let log2 m = unary_f "log2" m
  let log10 m = unary_f "log10" m
  let logp1 m = unary_f "logp1" m
  let log2p1 m = unary_f "log2p1" m
  let log10p1 m = unary_f "log10p1" m
  let sin m = unary_f "sin" m
  let cos m = unary_f "cos" m
  let tan m = unary_f "tan" m
  let sinpi m = unary_f "sinpi" m
  let cospi m = unary_f "cospi" m
  let atanpi m = unary_f "atanpi" m
  let atan2pi m = monoid_f "atan2pi" m
  let asin m = unary_f "asin" m
  let acos m = unary_f "acos" m
  let atan m = unary_f "atan" m
  let atan2 m = monoid_f "atan2" m
  let sinh m = unary_f "sinh" m
  let cosh m = unary_f "cosh" m
  let tanh m = unary_f "tanh" m
  let asinh m = unary_f "asinh" m
  let acosh m = unary_f "acosh" m
  let atanh m = unary_f "atanh" m

  let compound m = binary_fi "compound" m
  let rootn m = binary_fi "rootn" m
  let pown m = binary_fi "pown" m
end

module Lisp = Primus.Lisp.Semantics

let provide () =
  KB.Rule.(begin
      declare "primus-lisp-core-primitives" |>
      require Lisp.name |>
      require Lisp.args |>
      provide Theory.Semantics.slot |>
      comment "implements semantics for the core primitives"
    end);
  List.iter export ~f:(fun (name,types,docs) ->
      Primus.Lisp.Semantics.declare ~types ~docs name);
  let (let*?) x f = x >>= function
    | None -> !!nothing
    | Some x -> f x in
  KB.promise Theory.Semantics.slot @@ fun obj ->
  let*? name = KB.collect Lisp.name obj in
  let*? args = KB.collect Lisp.args obj in
  Theory.instance () >>= Theory.require >>= fun (module CT) ->
  let module P = Primitives(CT) in
  P.dispatch obj name args

let enable_extraction () =
  Theory.declare ~provides:["extraction"; "primus-lisp"; "lisp"]
    ~package:"bap"
    ~name:"primus-lisp" (KB.return (module CST : Theory.Core))
