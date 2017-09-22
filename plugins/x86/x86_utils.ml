open Core_kernel.Std
open Bap.Std

open X86_types
open X86_env

let tmp ?(name="v") ty =
  Var.create ~fresh:true ~is_virtual:true name ty

let index_ofq elt lst =
  match List.findi lst ~f:(fun _ e -> e = elt) with
  | None -> raise Not_found
  | Some (i,_) -> i

let concat_explist elist =
  List.reduce_exn
    ~f:Bil.(^) elist

(*FIXME: This is conversion from typ to nat1. It's used in cast expressions*)
let (!!) = function Type.Imm v -> v | _ -> failwith "internal error"

let bytes_of_width t =
  let b = !!t in
  if not ((b mod 8) = 0) then invalid_arg "bytes_of_width";
  b / 8

let exp_false = Bil.int BV.b0
let exp_true  = Bil.int BV.b1
let exp_not = Bil.lnot

(* exp from int *)
let int_exp n width = BV.of_int n ~width |> Bil.int

(* the 2 with_width functions are versions of functions that
 * already exist that don't throw away existing width information
 * so that we can avoid calling Typecheck.infer_ast *)
let extract_element_symbolic_with_width t e n et =
  let t = !!t in
  Bil.(cast low t (e lsr (n * (int_exp t et))))

let extract_byte_symbolic_with_width e n et =
  extract_element_symbolic_with_width (Type.imm 8) e n et

(* the following functions were used in Big_int_Z stuff
 * and have temporarily been put here, since Bitvector functionality is
 * not up to speed yet. *)
let extract_element t e n =
  let nbits = t in
  Bil.extract (n*nbits+(nbits-1)) (n*nbits) e

let extract_byte e n = extract_element 8 e n

let reverse_bytes e t =
  let bytes = bytes_of_width t in
  let get_byte n = extract_byte e n in
  List.reduce_exn
    ~f:(fun bige e -> Bil.(bige ^ e))
    (List.map ~f:get_byte (List.init ~f:(fun x -> x) bytes))

let min_symbolic ~is_signed e1 e2 =
  let open Bil in
  let cond = match is_signed with
    | true -> e1 <$ e2
    | false -> e1 < e2 in
  ite cond e1 e2

let max_symbolic ~is_signed e1 e2 =
  let open Bil in
  let cond = match is_signed with
    | true -> e1 <$ e2
    | false -> e1 < e2 in
  ite (lnot cond) e1 e2

module Cpu_exceptions = struct
  let general_protection = Bil.cpuexn 0xd
  let divide_by_zero = Bil.cpuexn 0x0
end


let compute_segment_bases = ref false

(* Note: In general, the function g is the get memory function.  The
   variable na refers to the next address or next instruction.

   To help understand this file, please refer to the Intel Instruction
   Set Reference. For consistency, any section numbers here are wrt
   Order Number: 253666-035US June 2010 and 253667-035US.


   The x86 instruction format is as follows:
   Instruction Prefixes: 0-4bytes (1 byte per prefix)
   Optional Rex Prefix: 1 byte
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unsupported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

let type_of_mode = function
  | X86 -> Type.imm 32
  | X8664 -> Type.imm 64

let width_of_mode = function
  | X86 -> 32
  | X8664 -> 64

let sig_to_mask =
  let open Pcmpstr in
  function
  | LSB -> Bitmask
  | MSB -> Bytemask

exception Arch_exception of Arch.x86 * string [@@deriving sexp]

(** disfailwith is a non-fatal disassembly exception. *)
let disfailwith m s =
  let a = match m with
    | X86   -> `x86
    | X8664 -> `x86_64 in
  raise (Arch_exception (a, s))

let unimplemented a s  = disfailwith a ("disasm x86: unimplemented feature: "^s)

(* eflags *)
let df_to_offset mode e =
  match type_of_mode mode with
  | Type.Mem _ -> failwith "type_of_mode shouldn't be returning this"
  | Type.Imm t ->
    let open Exp in
    Bil.(ite (e = exp_false) (int_exp 1 t) (int_exp (-1) t))

let bap_to_rflags =
  let undefined d = Bil.unknown (Printf.sprintf "Undefined RFLAGS bit %d" d) bool_t in
  let unmodeled s = Bil.unknown ("Unmodeled RFLAGS bit " ^ s) bool_t in
  (List.map ~f:undefined (List.range ~stride:(-1) ~stop:`inclusive 63 32))
  @  undefined 31                  (* 31 *)
     :: undefined 30               (* 30 *)
     :: undefined 29               (* 29 *)
     :: undefined 28               (* 28 *)
     :: undefined 27               (* 27 *)
     :: undefined 26               (* 26 *)
     :: undefined 25               (* 25 *)
     :: undefined 24               (* 24 *)
     :: undefined 23               (* 23 *)
     :: undefined 22               (* 22 *)
     :: unmodeled "ID"             (* 21 *)
     :: unmodeled "VIP"            (* 20 *)
     :: unmodeled "VIF"            (* 19 *)
     :: unmodeled "AC"             (* 18 *)
     :: unmodeled "VM"             (* 17 *)
     :: unmodeled "RF"             (* 16 *)
     :: undefined 15               (* 15 *)
     :: unmodeled "NT"             (* 14 *)
     :: unmodeled "IOPL1"          (* 13 *)
     :: unmodeled "IOPL2"          (* 12 *)
     :: Bil.var oF                 (* 11 *)
     :: Bil.var df                 (* 10 *)
     :: unmodeled "IF"             (*  9 *)
     :: unmodeled "TF"             (*  8 *)
     :: Bil.var sf                 (*  7 *)
     :: Bil.var zf                 (*  6 *)
     :: undefined 5                (*  5 *)
     :: Bil.var af                 (*  4 *)
     :: undefined 3                (*  3 *)
     :: Bil.var pf                 (*  2 *)
     :: undefined 1                (*  1 *)
     :: Bil.var cf                 (*  0 *)
     :: []

let bap_to_eflags = List.drop bap_to_rflags 32
let bap_to_flags = List.drop bap_to_eflags 16
let bap_to_lflags = List.drop bap_to_flags 8

let rflags_e = List.reduce_exn ~f:Bil.(^) bap_to_rflags
let eflags_e = List.reduce_exn ~f:Bil.(^) bap_to_eflags
let flags_e = List.reduce_exn ~f:Bil.(^) bap_to_flags
let lflags_e = List.reduce_exn ~f:Bil.(^) bap_to_lflags

let rflags_to_bap =
  let assn v = Some (v, (fun x -> x)) in
  (List.map ~f:(fun _ -> None) (List.range ~stride:(-1) ~stop:`inclusive 63 32))
  @  None                       (* 31 *)
     :: None                       (* 30 *)
     :: None                       (* 29 *)
     :: None                       (* 28 *)
     :: None                       (* 27 *)
     :: None                       (* 26 *)
     :: None                       (* 25 *)
     :: None                       (* 24 *)
     :: None                       (* 23 *)
     :: None                       (* 22 *)
     :: None                       (* 21 *)
     :: None                       (* 20 *)
     :: None                       (* 19 *)
     :: None                       (* 18 *)
     :: None                       (* 17 *)
     :: None                       (* 16 *)
     :: None                       (* 15 *)
     :: None                       (* 14 *)
     :: None                       (* 13 *)
     :: None                       (* 12 *)
     :: assn oF                    (* 11 *)
     :: assn df                    (* 10 *)
     :: None                       (* 09 *)
     :: None                       (* 08 *)
     :: assn sf                    (* 07 *)
     :: assn zf                    (* 06 *)
     :: None                       (* 05 *)
     :: assn af                    (* 04 *)
     :: None                       (* 03 *)
     :: assn pf                    (* 02 *)
     :: None                       (* 01 *)
     :: assn cf                    (* 00 *)
     :: []

let eflags_to_bap = List.drop rflags_to_bap 32
let flags_to_bap = List.drop eflags_to_bap 16
let lflags_to_bap = List.drop flags_to_bap 8

(* A list of functions for assigning each bit in rflags *)
let assns_rflags_to_bap =
  List.map
    ~f:(function
        | None -> (fun _ -> [])
        | Some (v,f) -> (fun e -> [Bil.move v (f e)]))
    rflags_to_bap
let assns_eflags_to_bap = List.drop assns_rflags_to_bap 32
let assns_flags_to_bap = List.drop assns_eflags_to_bap 16
let assns_lflags_to_bap = List.drop assns_flags_to_bap 8

(* exp helpers *)

let load_s mode s t a =
  let mem = match mode with
    | X86 -> R32.mem
    | X8664 -> R64.mem in
  let mem_e = Bil.var mem in
  match s with
  | None -> Bil.load mem_e a LittleEndian t
  | Some v -> Bil.(load mem_e (var v + a) LittleEndian t)

let resize_word v width = Word.extract_exn ~hi:(width - 1) v

(* exp from big int *)
let bt n width = resize_word n width |> Bil.int
let b64 i = bt i 64
let b32 i = bt i 32
let b16 i = bt i 16

let int64_of_mode m i = match m with
  | X86 -> BV.of_int64 i ~width:32
  | X8664 -> BV.of_int64 i ~width:64

let int_of_mode m i = match m with
  | X86 ->  BV.of_int i ~width:32
  | X8664 -> BV.of_int i ~width:64

let big_int_of_mode m i =
  let module W = Word in
  match m with
  | X86 when W.bitwidth i = 32 -> i
  | X86 when W.bitwidth i < 32 -> resize_word i 32
  | X8664 when W.bitwidth i = 64 -> i
  | X8664 when W.bitwidth i < 64 -> resize_word i 64
  | _ -> failwith "big_int_of_mode failure"

(* Get elemt from low opcode bits *)
let lowbits2elemt b =
  match b land 3 with
  | 0 -> Type.imm 8
  | 1 -> Type.imm 16
  | 2 -> Type.imm 32
  | 3 -> Type.imm 64
  | _ -> disfailwith X86 "invalid"

(* converts a register number to the corresponding register variable *)
let bits2genreg mode =
  let module R = (val (vars_of_mode mode)) in
  let open R in function
    | 0 -> rax
    | 1 -> rcx
    | 2 -> rdx
    | 3 -> rbx
    | 4 -> rsp
    | 5 -> rbp
    | 6 -> rsi
    | 7 -> rdi
    | i when i >= 8 && i <= 15 -> nums.(i-8)
    | _ -> failwith "bits2genreg takes 4 bits"

let reg2bits mode x =
  let module R = (val (vars_of_mode mode)) in
  let open R in
  index_ofq x [rax; rcx; rdx; rbx; rsp; rbp; rsi; rdi]

let bits2segreg = function
  | 0 -> es
  | 1 -> cs
  | 2 -> ss
  | 3 -> ds
  | 4 -> fs
  | 5 -> gs
  | 6 | 7 -> disfailwith X86 "bits2segreg: reserved"
  | _ -> failwith "bits2regseg: invalid"

let bits2segrege b = bits2segreg b |> Bil.var

let bits2ymm b = ymms.(b)

let bits2ymme b = bits2ymm b |> Bil.var

let bits2ymm128e b =
  bits2ymme b |> Bil.(cast low (!!reg128_t))

let bits2ymm64e b =
  bits2ymme b |> Bil.(cast low (!!reg64_t))

let bits2ymm32e b =
  bits2ymme b |> Bil.(cast low (!!reg32_t))

let bits2xmm = bits2ymm128e

let bits2xmm64e = bits2ymm64e

let bits2xmm32e = bits2ymm32e

let bits2reg64e mode b =
  Bil.var (bits2genreg mode b)

let bits2reg32e mode b =
  bits2genreg mode b |> Bil.var |> Bil.(cast low (!!reg32_t))

let bits2reg16e mode b =
  bits2reg32e mode b |> Bil.(cast low (!!reg16_t))

let bits2reg8e mode ?(has_rex=false) b =
  if b < 4 || has_rex then
    bits2reg32e mode b |> Bil.(cast low (!!reg8_t))
  else
    b land 3 |> bits2reg32e mode |>
    Bil.(cast low (!!reg16_t)) |>  Bil.(cast high (!!reg8_t))

let reg2xmm mode r = reg2bits mode r |> bits2xmm

(* effective addresses for 16-bit addressing *)
let eaddr16 mode =
  let open Bil in
  let module R = (val (vars_of_mode mode)) in
  let open R in
  let e v = Bil.var v |> Bil.(cast low (!!reg16_t)) in
  function
  (* R/M byte *)
  | 0 -> e rbx + e rsi
  | 1 -> e rbx + e rdi
  | 2 -> e rbp + e rsi
  | 3 -> e rbp + e rdi
  | 4 -> e rsi
  | 5 -> e rdi
  | 6 -> e rbp
  | 7 -> e rbx
  | _ -> disfailwith X86 "eaddr16 takes only 0-7"

let eaddr16e mode b = eaddr16 mode b

let ah_e mode = bits2reg8e mode 4
let ch_e mode = bits2reg8e mode 5
let dh_e mode = bits2reg8e mode 6
let bh_e mode = bits2reg8e mode 7


let pp_insn ppf (mem,insn) =
  Format.fprintf ppf "%a: %s"
    Addr.pp_hex (Memory.min_addr mem)
    (Disasm_expert.Basic.Insn.asm insn)
