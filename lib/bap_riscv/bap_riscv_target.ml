open Core_kernel
open Bap_core_theory

let package = "bap"

type r64 and r32 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r64 : r64 bitv = Theory.Bitv.define 64
let r32 : r32 bitv = Theory.Bitv.define 32
let r8  : r8  bitv = Theory.Bitv.define 8

let reg t n = Theory.Var.define t n
let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys


let regs = [
  "zero";
  "ra";
  "sp";
  "gp";
  "tp";
  "t0"; "t1"; "t2";
  "s0"; "s1";
  "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7";
  "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11";
  "t3"; "t4"; "t5"; "t6"
]

let array t = List.map regs ~f:(reg t)
let vars p t = List.init 32 ~f:(fun i -> reg t (sprintf "%c%d" p  i))

let parent = Theory.Target.declare ~package "riscv"
    ~endianness:Theory.Endianness.le

let select xs ~mask =
  let mask = Set.of_list (module Int) mask in
  List.filteri xs ~f:(fun i _ -> Set.mem mask i)

let (--) x y = List.range x (y+1)

let riscv t =
  let mems = Theory.Mem.define t r8 in
  let mem = reg mems "mem" in
  let pc = reg t "PC" in
  let ints = untyped@@vars 'X' t in
  let flts = untyped@@vars 'F' t in
  let vars = ints @< flts @< [pc] @< [mem] in
  let bits = Theory.Bitv.size t in
  let name = sprintf "riscv%d" bits in
  let x i = select ints ~mask:i in
  let f i = select flts ~mask:i in
  Theory.Target.declare ~package name ~parent
    ~bits
    ~code:mem
    ~data:mem
    ~vars
    ~regs:Theory.Role.Register.[
        [general; integer], ints;
        [general; floating], flts;
        [constant; zero; pseudo], x[0];
        [pseudo], untyped@@[pc];
        [function_argument], x(10--17) @ f(10--17);
        [function_return], x(10--12) @ f(10--12);
        [link], x[1];
        [stack_pointer], x[2];
        [thread], x[3];
        [caller_saved], x[1] @ x(5--7) @ x(10--17) @ x(28--31) @
                        f(0--7) @ f(10--17) @ f(28--31);
        [callee_saved], x[2] @ x(8--9) @ x(18--27) @
                        f(8--9) @ f(18--27);

      ]

let riscv64 = riscv r64
let riscv32 = riscv r32

let llvm64 = Theory.Language.declare ~package "llvm-riscv64"
let llvm32 = Theory.Language.declare ~package "llvm-riscv32"
