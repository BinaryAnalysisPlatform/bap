open Core_kernel[@@warning "-D"]
open Bap_core_theory

let package = "bap"

type r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

let r16 : r16 bitv = Theory.Bitv.define 16
let r8  : r8  bitv = Theory.Bitv.define 8
let bool = Theory.Bool.t

let reg t n = Theory.Var.define t n
let untyped = List.map ~f:Theory.Var.forget
let (@<) xs ys = untyped xs @ untyped ys


let regs = [
   "r0";  "r1";  "r2";  "r3";  "r4";  "r5"; "r6"; "r7"; "r8"; "r9"; 
  "r10"; "r11"; "r12"; "r13"; "r14"; "r15"
]

let cf = reg bool "C"
let zf = reg bool "Z"
let nf = reg bool "N"
let vf = reg bool "V"

let status_reg = [nf; zf; cf; vf]

let array t = List.map regs ~f:(reg t)
let vars p t = List.init 16 ~f:(fun i -> reg t (sprintf "%c%d" p  i))

(*
let parent = Theory.Target.declare ~package "msp430"
    ~endianness:Theory.Endianness.le *)

let select xs ~mask =
  let mask = Set.of_list (module Int) mask in
  List.filteri xs ~f:(fun i _ -> Set.mem mask i)

let msp430 =
  let mems = Theory.Mem.define r16 r8 in
  let mem = reg mems "mem" in
  (*let pc = reg r16 "PC" in *PC is at r0 *)
  let ints = untyped@@vars 'R' r16 in
  let vars = ints (*@< flts @< [pc]*) @< status_reg @< [mem] in
  let bits = Theory.Bitv.size r16 in
  let r i = select ints ~mask:i in
  Theory.Target.declare ~package "msp430" (*~parent*)
    ~endianness:Theory.Endianness.le
    ~bits
    ~code:mem
    ~data:mem
    ~vars
    ~regs:Theory.Role.Register.[
        [general; integer], ints;
        (*[general; floating], flts;*)
        (*[constant; zero; pseudo], x[0];*)
        (*[pseudo], untyped@@[pc];*)
        (*[function_argument], x(10--17) @ f(10--17);
        [function_return], x(10--12) @ f(10--12);
        [link], x[1];*)
        [stack_pointer], r[1];
        (*[thread], x[3];*)
        (*[caller_saved], x[1] @ x(5--7) @ x(10--17) @ x(28--31) @
                        f(0--7) @ f(10--17) @ f(28--31);
        [callee_saved], x[2] @ x(8--9) @ x(18--27) @
                        f(8--9) @ f(18--27);*)
        [status; integer], untyped status_reg;
        [carry_flag], untyped [cf];
        [sign_flag], untyped [nf];
        [zero_flag], untyped [zf];
        [overflow_flag], untyped [vf];
      ]

let llvm16 = Theory.Language.declare ~package "llvm-msp430"
