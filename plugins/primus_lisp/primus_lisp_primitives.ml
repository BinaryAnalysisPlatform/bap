open Core_kernel
open Bap.Std
open Bap_primus.Std

let registers = Primus.Machine.State.declare
    ~uuid:"3dd2b905-c019-4466-800f-95b13d7da85e"
    ~name:"backend-registers" @@ fun proj ->
  Project.disasm proj |> Disasm.insns |>
  Seq.fold ~init:Int.Map.empty ~f:(fun regs (_,insn) ->
      Insn.ops insn |>
      Array.fold ~init:regs ~f:(fun regs -> function
          | Op.Reg r ->
            Map.set regs (Reg.code r) (Reg.name r)
          | _ -> regs))

module Closure(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Closure = Primus.Lisp.Closure.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)

  type value = Value.t
  type 'a m = 'a Machine.t

  open Machine.Syntax

  let failf = Lisp.failf

  let addr_width =
    Machine.arch >>| Arch.addr_size >>| Size.in_bits
  let endian =
    Machine.arch >>| Arch.endian
  let null = addr_width >>= Value.zero
  let negone = null >>= Value.lnot
  let false_ = Value.zero 1
  let true_  = Value.one 1
  let reduce null op = function
    | [] -> null
    | x :: xs -> Machine.List.fold xs ~init:x ~f:op

  let msb x =
    let hi = Value.bitwidth x - 1 in
    Value.extract ~hi ~lo:hi x

  let to_int e = match Word.to_int (Value.to_word e) with
    | Ok x -> Machine.return x
    | Error _ -> failf "expected smallint" ()

  let rec all f = function
    | [] -> true_
    | x :: xs ->
      f x >>= fun x ->
      if Value.is_one x
      then
        all f xs >>= fun xs ->
        Eval.binop AND x xs
      else Machine.return x

  let ordered order xs =
    let rec ordered = function
      | [] | [_] -> true_
      | x :: (y :: _ as rest) ->
        order x y >>= fun r ->
        if Value.is_one r
        then ordered rest >>= fun r' ->
          Eval.binop AND r r'
        else Machine.return r in
    ordered xs

  let reg_name code =
    Machine.Local.get registers >>= fun regs ->
    let code = Word.to_int_exn @@ Primus.Value.to_word code in
    match Map.find regs code with
    | None -> failf "unresolved intruction register %#x" code ()
    | Some name -> Value.Symbol.to_value name

  let exec_addr addr =
    Linker.exec (`addr (Primus.Value.to_word addr)) >>= fun () ->
    Eval.halt >>=
    never_returns

  let exec_symbol name =
    Value.Symbol.of_value name >>= fun name ->
    Linker.exec (`symbol name) >>= fun () ->
    Eval.halt >>=
    never_returns

  let is_zero = all @@ fun x ->
    Value.zero (Value.bitwidth x) >>= fun z ->
    Eval.binop EQ x z

  let is_positive = all @@ fun x ->
    msb x >>= Eval.unop NOT

  let word_width args =
    addr_width >>= fun width ->
    match args with
    | [] -> Value.of_int ~width width
    | x :: _ -> Value.of_int ~width (Value.bitwidth x)

  let exit_with _ =
    Eval.halt >>|
    Nothing.unreachable_code >>= fun () ->
    Value.b0

  let negone = Value.one 8
  let zero = Value.zero 8

  let value_to_int x = Value.to_word x |> Word.to_int

  let make_static_generator width x = match Word.to_int x with
    | Ok x -> Ok (Some (Primus.Generator.static ~width x))
    | _ -> Or_error.errorf "memory-allocate: fill in value must fit into int"

  let is_nil x = Word.equal Word.b0 (Value.to_word x)

  let make_uniform_generator ~width ~min ~max =
    let nomin = is_nil min and nomax = is_nil max in
    if nomin && nomax then Ok None
    else match value_to_int  min, value_to_int max with
      | Ok min, Ok max ->
        let min = if nomin then None else Some min
        and max = if nomax then None else Some max in
        let g = Primus.Generator.Random.Seeded.lcg ~width ?min ?max () in
        Ok (Some g)
      | _ -> Or_error.errorf "memory-allocate: random values do not fit into int"


  let memory_allocate addr size rest =
    let module Memory = Primus.Memory.Make(Machine) in
    Machine.gets Project.arch >>= fun arch ->
    let width = Arch.addr_size arch |> Size.in_bits in
    let n = Word.to_int (Value.to_word size) in
    let gen = match rest with
      | []  -> Ok None
      | [x] -> make_static_generator width (Value.to_word x)
      | [min; max] -> make_uniform_generator width min max
      | _ -> Or_error.errorf "bad generator" in
    if Result.is_error n
    then failf "memory-allocate: bad number" () else
    if Result.is_error gen
    then failf "memory-allocate: bad generator specification" ()
    else
      let generator = Or_error.ok_exn gen in
      Memory.allocate ?generator (Value.to_word addr) (Or_error.ok_exn n) >>=
      fun () -> zero

  let memory_read addr = Eval.load addr BigEndian `r8
  let memory_write a x =
    Eval.store a x BigEndian `r8 >>= fun () ->
    Value.succ a

  let extract hi lo x =
    to_int hi >>= fun hi ->
    to_int lo >>= fun lo ->
    Eval.extract ~hi ~lo x

  let symbol_concat syms =
    Machine.List.map syms ~f:Value.Symbol.of_value >>= fun strs ->
    Value.Symbol.to_value (String.concat strs)

  let set_value reg x =
    Value.Symbol.of_value reg >>= fun reg ->
    let typ = Type.imm (Word.bitwidth (Value.to_word x)) in
    let var = Var.create reg typ in
    Eval.set var x >>| fun () -> x

  let run args =
    Closure.name >>= fun name -> match name, args with
    | "reg-name",[arg] -> reg_name arg
    | "exec-addr", [addr] -> exec_addr addr
    | "exec-symbol", [dst] -> exec_symbol dst
    | "is-zero", args -> is_zero args
    | "is-positive", args -> is_positive args
    | "is-negative", args -> all msb args
    | "word-width", args -> word_width args
    | "exit-with", [arg] -> exit_with arg
    | "memory-allocate", x :: y :: rest -> memory_allocate x y rest
    | "memory-read", [x] -> memory_read x
    | "memory-write", [a;x] -> memory_write a x
    | "get-current-program-counter",[] -> Eval.pc >>= Value.of_word
    | "+",args -> reduce null (Eval.binop Bil.PLUS) args
    | "-",args -> reduce null (Eval.binop Bil.MINUS) args
    | "/",args -> reduce null (Eval.binop Bil.DIVIDE) args
    | "s/",args -> reduce null (Eval.binop Bil.SDIVIDE) args
    | "*",args -> reduce null (Eval.binop Bil.TIMES) args
    | "mod",args -> reduce null (Eval.binop Bil.MOD) args
    | "signed-mod",args -> reduce null (Eval.binop Bil.SMOD) args
    | "lshift", [x;y] -> Eval.binop Bil.lshift x y
    | "rshift", [x;y] -> Eval.binop Bil.rshift x y
    | "arshift", [x;y] -> Eval.binop Bil.arshift x y
    | "=",args -> ordered (Eval.binop EQ) args
    | "/=",args -> ordered (Eval.binop NEQ) args
    | "logand",args -> reduce negone (Eval.binop Bil.AND) args
    | "logor",args -> reduce negone (Eval.binop Bil.OR) args
    | "logxor",args -> reduce negone (Eval.binop Bil.XOR) args
    | "concat", args -> reduce false_ Eval.concat args
    | "extract", [hi; lo; x] -> extract hi lo x
    | "not", [x] -> is_zero [x]
    | "lnot", [x] -> Eval.unop Bil.NOT x
    | "neg", [x] -> Eval.unop Bil.NEG x
    | "<", args -> ordered (Eval.binop LT) args
    | ">", args -> ordered (fun x y -> Eval.binop LT y x) args
    | "<=", args -> ordered (Eval.binop LE) args
    | ">=", args -> ordered (fun x y -> Eval.binop LE y x) args
    | "symbol-concat",args -> symbol_concat args
    | "set-symbol-value", [reg; x] -> set_value reg x
    | name,_ -> Lisp.failf "%s: invalid number of arguments" name ()
end

module Primitives(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Lisp = Primus.Lisp.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)

  let init () =
    let open Primus.Lisp.Type.Spec in
    let def name types docs =
      Lisp.define ~types ~docs name (module Closure)  in
    Machine.sequence [
      def "exec-addr" (one int @-> any)
        "(exec-addr D) passes the control flow to D and never returns";
      def "exec-symbol" (one sym @-> any)
        "(exec-symbol D) passes the control flow to D and never returns";
      def "is-zero" (all any @-> bool)
        "(is-zero X Y ...) returns true if all arguments are zeros";
      def "is-positive" (all any @-> bool)
        "(is-positive X Y ...) returns true if all arguments are positive";
      def "is-negative" (all any @-> bool)
        "(is-negative X Y ...) returns true if all arguments are negative";
      def "word-width" (unit @-> int)
        "(word-width) returns machine word width in bits";
      def "exit-with" (one int @-> any)
        "(exit-with N) terminates program with the exit codeN";
      def "memory-read" (one int @-> byte)
        "(memory-read A) loads one byte from the address A";
      def "memory-write" (tuple [int; byte] @-> int)
        "(memory-write A X) stores by X to A";
      def "memory-allocate" (tuple [int; int] // all byte @-> byte)
        "(memory-allocate P N V?) maps memory region [P,P+N), if V is
         provided, then fills the newly mapped region with the value V";
      def "get-current-program-counter" (unit @-> int)
        "(get-current-program-counter) returns current program cunnter";
      def "+" (all a @-> a)
        "(+ X Y ...) returns the sum of arguments, or 0 if there are
         no arguments,";
      def "-" (all a @-> a)
        "(- X Y Z ...) returns X - Y - Z - ..., or 0 if there are no
         arguments.";
      def "*" (all a @-> a)
        "(* X Y Z ...) returns the product of arguments or 0 if the list
        of arguments is empty";
      def "/" (all a @-> a)
        "(/ X Y Z ...) returns X / Y / Z / ... or 0 if the list of
         arguments is empty";
      def "s/" (all a @-> a)
        "(s/ X Y Z ...) returns X s/ Y s/ Z s/ ... or 0 if the list of
         arguments is empty, where s/ is the signed division operation";
      def "mod" (all a @-> a)
        "(mod X Y Z ...) returns X % Y % Z % ... or 0 if the list of
         arguments is empty, where % is the modulo operation";
      def "signed-mod" (all a @-> a)
        "(signed-mod X Y Z ...) returns X % Y % Z % ... or 0 if the list of
         arguments is empty, where % is the signed modulo operation";
      def "lshift" (tuple [a; b] @-> a)
        "(lshift X N) logically shifts X left by N bits";
      def "rshift" (tuple [a; b] @-> a)
        "(rshift X N) logically shifts X right by N bits";
      def "arshift" (tuple [a; b] @-> a)
        "(arshift X N) arithmetically shifts X right by N bits";
      def "=" (all a @-> bool)
        "(= X Y Z ...) returns true if all arguments are equal. True
        if the list of arguments is empty";
      def "/=" (all a @-> bool)
        "(/= X Y Z ...) returns true if at least one argument is not
         equal to another argument. Returns false if the list of
         arguments is empty";
      def "logand" (all a @-> a)
        "(logand X Y Z ...) returns X & Y & Z & ... or 0 if the list of
         arguments is empty, where & is the bitwise AND
         operation. Returns ~0 if the list of arguments is empty";
      def "logor" (all a @-> a)
        "(logor X Y Z ...) returns X | Y | Z | ... or 0 if the list of
         arguments is empty, where | is the bitwise OR operation";
      def "logxor" (all a @-> a)
        "(logxor X Y Z ...) returns X ^ Y ^ Z ^ ... or 0 if the list of
         arguments is empty, where ^ is the bitwise XOR operation";
      def "concat" (all any @-> any)
        "(concat X Y Z ...) concatenates words X, Y, Z, ... into one
         big word";
      def "extract" (tuple [any; any; any] @-> any)
        "(extract HI LO X) extracts bits from HI to LO (including
           both) from the word X ";
      def "lnot" (one a @-> a)
        "(lnot X) returns the one complement of X";
      def "not" (one a @-> a)
        "(not X) returns true if X is zero";
      def "neg" (one a @-> a)
        "(neg X) returns the two complement of X";
      def "<" (all a @-> bool)
        "(< X Y Z ...) is true if the list of arguments is an
         strict ascending chain or if it is empty";
      def ">" (all a @-> bool)
        "(< X Y Z ...) is true if the list of arguments is a
         strict descending chain or if it is empty";
      def "<=" (all a @-> bool)
        "(< X Y Z ...) is true if the list of arguments is an
         ascending chain or if it is empty";
      def ">=" (all a @-> bool)
        "(< X Y Z ...) is true if the list of arguments is a
         descending chain or if it is empty";
      def "symbol-concat" (all sym @-> sym)
        "(symbol-concat X Y Z ...) returns a new symbol that is a
        concatenation of symbols X,Y,Z,... ";
      def "set-symbol-value" (tuple [sym; a] @-> a)
        "(set-symbol-value S X) sets the value of the symbol S to X.
         Returns X";
      def "reg-name" (one int @-> sym)
        "(reg-name N) returns the name of the register with the index N";
    ]
end

let init () =
  Primus.Machine.add_component (module Primitives) [@warning "-D"];
  Primus.Components.register_generic "lisp-primitives" (module Primitives)
    ~package:"bap"
    ~desc:"Provides the core set of Primus Lisp primitives.";
  Primus_lisp_ieee754.init ()
