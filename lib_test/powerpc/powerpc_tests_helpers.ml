open Core_kernel.Std
open Bap.Std
open OUnit2

[@@@warning "-D"]

module Dis = Disasm_expert.Basic

module type Bitwidth = sig
  val gpr_bitwidth : int
  val addr_size : addr_size
end

let range32 = List.range 0 32
let make_name prefix i = sprintf "%s%d" prefix i
let make_var_i typ prefix i = Var.create (make_name prefix i) typ

let make_regs typ ?alias prefix range =
  List.fold ~init:String.Map.empty ~f:(fun regs i ->
      let var = make_var_i typ prefix i in
      let name = Var.name var in
      let regs = Map.add regs name var in
      match alias with
      | None -> regs
      | Some a ->
        let name = make_name a i in
        Map.add regs name var) range

let flag name = Var.create name (Type.imm 1)

module Make(B : Bitwidth) = struct
  open B
  let gpr = make_regs (Type.imm gpr_bitwidth) "R" ~alias:"X" range32
  let ctr = Var.create "CTR" (Type.imm gpr_bitwidth)
  let lr = Var.create "LR" (Type.imm gpr_bitwidth)
  let tar = Var.create "TAR" (Type.imm gpr_bitwidth)

  (** condition register bits  *)
  let cr0  = flag "CR7UN"
  let cr1  = flag "CR7EQ"
  let cr2  = flag "CR7GT"
  let cr3  = flag "CR7LT"
  let cr4  = flag "CR6UN"
  let cr5  = flag "CR6EQ"
  let cr6  = flag "CR6GT"
  let cr7  = flag "CR6LT"
  let cr8  = flag "CR5UN"
  let cr9  = flag "CR5EQ"
  let cr10 = flag "CR5GT"
  let cr11 = flag "CR5LT"
  let cr12 = flag "CR4UN"
  let cr13 = flag "CR4EQ"
  let cr14 = flag "CR4GT"
  let cr15 = flag "CR4LT"
  let cr16 = flag "CR3UN"
  let cr17 = flag "CR3EQ"
  let cr18 = flag "CR3GT"
  let cr19 = flag "CR3LT"
  let cr20 = flag "CR2UN"
  let cr21 = flag "CR2EQ"
  let cr22 = flag "CR2GT"
  let cr23 = flag "CR2LT"
  let cr24 = flag "CR1UN"
  let cr25 = flag "CR1EQ"
  let cr26 = flag "CR1GT"
  let cr27 = flag "CR1LT"
  let cr28 = flag "CR0UN"
  let cr29 = flag "CR0EQ"
  let cr30 = flag "CR0GT"
  let cr31 = flag "CR0LT"

  let cr_bits = [
    cr0;  cr1;  cr2;  cr3;  cr4;  cr5;  cr6;  cr7;
    cr8;  cr9;  cr10; cr11; cr12; cr13; cr14; cr15;
    cr16; cr17; cr18; cr19; cr20; cr21; cr22; cr23;
    cr24; cr25; cr26; cr27; cr28; cr29; cr30; cr31;
  ]

  let cri =
    let _, bits =
      List.fold (List.rev cr_bits) ~init:(0,Int.Map.empty)
        ~f:(fun (num, bits) bit ->
            num + 1, Map.add bits ~key:num ~data:bit) in
    bits

  let crn =
    Int.Map.fold cri ~init:String.Map.empty
      ~f:(fun ~key:_ ~data:var acc ->
          Map.add acc (Var.name var) var)

  let fields = [
    "CR0", 0, (cr28, cr29, cr30, cr31);
    "CR1", 1, (cr24, cr25, cr26, cr27);
    "CR2", 2, (cr20, cr21, cr22, cr23);
    "CR3", 3, (cr16, cr17, cr18, cr19);
    "CR4", 4, (cr12, cr13, cr14, cr15);
    "CR5", 5, (cr8,  cr9,  cr10, cr11);
    "CR6", 6, (cr4,  cr5,  cr6,  cr7);
    "CR7", 7, (cr0,  cr1,  cr2,  cr3);
  ]

  let cr_fields =
    List.fold fields ~init:String.Map.empty ~f:(fun fs (name, _, fd) ->
        Map.add fs name fd)

  let cri_fields =
    List.fold fields ~init:Int.Map.empty ~f:(fun fs (_, index, fd) ->
        Map.add fs index fd)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)
  let mem = Var.create "mem" (Type.mem addr_size `r8)
end


module P32 = Make(struct
    let gpr_bitwidth = 32
    let addr_size = `r32
  end)

module P64 = Make(struct
    let gpr_bitwidth = 64
    let addr_size = `r64
  end)

module Any_ppc = P32

let raise_arch () =
  failwith "powerpc arch is the only expected"

let cr_bit n =
  Int.Map.find_exn Any_ppc.cri n

let nf = cr_bit 0
let pf = cr_bit 1
let zf = cr_bit 2
let ca = Any_ppc.ca
let ca32 = Any_ppc.ca32

let lr = function
  | `ppc -> P32.lr
  | `ppc64 -> P64.lr
  | _ -> raise_arch ()

let ctr = function
  | `ppc -> P32.ctr
  | `ppc64 -> P64.ctr
  | _ -> raise_arch ()

let tar = function
  | `ppc -> P32.tar
  | `ppc64 -> P64.tar
  | _ -> raise_arch ()

let cri = Any_ppc.cri

let mem = function
  | `ppc -> P32.mem
  | `ppc64 -> P64.mem
  | _ -> raise_arch ()

module E = struct

  let cr =
    match List.rev P32.cr_bits with
    | [] -> assert false
    | v :: vars ->
      List.fold vars ~init:(Bil.var v) ~f:(fun e v -> Bil.(e ^ var v))

  let cri = Map.map cri ~f:(fun v -> Bil.var v)
end


let create_dis arch =
  Dis.create ~backend:"llvm" (Arch.to_string arch) |>
  Or_error.ok_exn |>
  Dis.store_kinds |>
  Dis.store_asm

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error _ -> failwith "can't create memory"

let to_bil arch mem insn =
  let module T = (val (target_of_arch arch)) in
  T.lift mem insn

let get_insn ?addr arch bytes =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = match addr with
    | None -> Addr.of_int ~width 0
    | Some a -> a in
  let mem = create_memory arch bytes addr in
  let dis = create_dis arch in
  match Dis.insn_of_mem dis mem with
  | Ok (mem, Some insn, _) ->
    let insn_name = Insn.(name @@ of_basic insn) in
    mem, insn, insn_name
  | _ -> failwith "disasm failed"

let lookup_var c var = match c#lookup var with
  | None -> None
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Some word
    | Bil.Bot | Bil.Mem _ -> None

let find_gpr arch name =
  try
    match arch with
    | `ppc -> String.Map.find_exn P32.gpr name
    | _ -> String.Map.find_exn P64.gpr name
  with _ ->
    sprintf "gpr %s not" name |> failwith

let get_bil ?addr arch bytes =
  let mem,insn,_ = get_insn ?addr arch bytes in
  to_bil arch mem insn

let check_bil bil =
  match Type.check bil with
  | Error te ->
    let err =
      sprintf "The lifted code is not well-typed: %s"
        (Type.Error.to_string te) in
    failwith err
  | Ok () -> ()

(** [check_gpr ?addr init_bil bytes var expected arch ctxt] -
    tests if a result bound to the [var] is equal to
    [exptected]. Evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
let check_gpr ?addr init bytes var expected arch _ctxt =
  let mem,insn,insn_name = get_insn ?addr arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  check_bil (init @ bil);
  let c = Stmt.eval (init @ bil) (new Bili.context) in
  match lookup_var c var with
  | None -> assert_bool "var not found OR it's result not Imm" false
  | Some w ->
    if not (Word.equal w expected) ||
        (Word.bitwidth w <> Word.bitwidth expected) then
      printf "\n%s: check failed for %s: expected %s <> %s\n"
        insn_name
        (Var.name var)
        (Word.to_string expected)
        (Word.to_string w);
    assert_equal ~cmp:Word.equal w expected

(** [eval ?addr init_bil bytes arch] - evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
let eval ?addr init bytes arch =
  let mem,insn,_ = get_insn ?addr arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  check_bil (init @ bil);
  Stmt.eval (init @ bil) (new Bili.context)

let load_word ctxt mem addr endian size =
  let bits = Size.in_bits size in
  let tmp = Var.create ~fresh:true "tmp" (Type.imm bits) in
  let bil = Bil.[
      tmp := load ~mem:(var mem) ~addr:(int addr) endian size;
    ] in
  check_bil bil;
  let ctxt = Stmt.eval bil ctxt in
  lookup_var ctxt tmp

let check_mem init bytes mem ~addr ~size expected ?(endian=BigEndian) arch _ctxt =
  let memory,insn,insn_name = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch memory insn in
  check_bil (init @ bil);
  let c = Stmt.eval (init @ bil) (new Bili.context) in
   match load_word c mem addr endian size with
  | None -> assert_bool "word not found OR it's result not Imm" false
  | Some w ->
    if not (Word.equal w expected) then
      printf "\n%s: check failed for %s: expected %s <> %s\n"
        insn_name
        (Addr.to_string addr)
        (Word.to_string expected)
        (Word.to_string w);
    assert_equal ~cmp:Word.equal w expected

let concat_words ws = match ws with
  | [] -> failwith "words list is empty!"
  | w :: ws ->
    List.fold ~init:w ~f:(fun ws w -> Word.concat ws w) ws

(** [make_bytes ws] - returns a string representation of concated words [ws] *)
let make_bytes ws =
  let bytes = concat_words ws in
  let bytes = Seq.to_list @@ Word.enum_chars bytes BigEndian in
  String.of_char_list bytes

let is_equal_words w = function
  | None -> false
  | Some w' -> Word.equal w w'

let string_of_bytes bytes =
  String.fold ~init:"" ~f:(fun acc b ->
      sprintf "%s%02X " acc (Char.to_int b)) bytes

let arch_width a = Arch.addr_size a |> Size.in_bits

type form = [
  | `B
  | `D
  | `I
  | `M
  | `MD
  | `MDS
  | `VA
  | `X
  | `XL
  | `XFX
  | `XO
  | `XS
] [@@deriving sexp]

let make_insn ?name ?(arch=`ppc) form fields =
  let b0 = Word.b0 in
  let word ~width n = Word.of_int ~width n in
  let bytes =
    match form, fields with
    | `B, [opcode; bo; bi; bd; aa; lk] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 bo;
        word ~width:5 bi;
        word ~width:14 bd;
        word ~width:1 aa;
        word ~width:1 lk;
      ]
    | `D, [opcode; rt; ra; d] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:16 d;
      ]
    | `I, [opcode; imm; aa; lk]  ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:24 imm;
        word ~width:1 aa;
        word ~width:1 lk;
      ]
    | `M, [opcode; rs; ra; sh; mb; me; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh;
        word ~width:5 mb;
        word ~width:5 me;
        word ~width:1 rc;
      ]
    | `MD, [opcode; rs; ra; sh1; me; opt; sh2; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh1;
        word ~width:6 me;
        word ~width:3 opt;
        word ~width:1 sh2;
        word ~width:1 rc;
      ]
    | `MDS, [opcode; rs; ra; rb; mb; opt; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:6 mb;
        word ~width:4 opt;
        word ~width:1 rc;
      ]
    | `VA, [opcode; rt; ra; rb; rc; opt_opcode;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:5 rc;
        word ~width:6 opt_opcode;
      ]
    | `X, [opcode; rt; ra; rb; opt_opcode; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:10 opt_opcode;
        word ~width:1 rc;
      ]
    | `X, [opcode; rt; ra; rb; opt_opcode;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:10 opt_opcode;
        b0;
      ]
    | `XL, [opcode; bf; x; bfa; y; z; opt_opcode; w;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:3 bf;
        word ~width:2 x;
        word ~width:3 bfa;
        word ~width:2 y;
        word ~width:5 z;
        word ~width:10 opt_opcode;
        word ~width:1 w;
      ]
    | `XL, [opcode; bt; ba; bb; opt_opcode; x] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 bt;
        word ~width:5 ba;
        word ~width:5 bb;
        word ~width:10 opt_opcode;
        word ~width:1 x;
      ]
    | `XL, [opcode; bo; bi; empty; bh; opt_opcode; lk;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 bo;
        word ~width:5 bi;
        word ~width:3 empty;
        word ~width:2 bh;
        word ~width:10 opt_opcode;
        word ~width:1 lk;
      ]
    | `XFX, [opcode; rs; x; data; y; opt_opcode; z] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:1 x;
        word ~width:8 data;
        word ~width:1 x;
        word ~width:10 opt_opcode;
        word ~width:1 z;
      ]
    | `XFX, [opcode; rs; data; opt_opcode; x] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:10 data;
        word ~width:10 opt_opcode;
        word ~width:1 x;
      ]
    | `XO, [opcode; rt; ra; rb; oe; opt_opcode; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:1 oe;
        word ~width:9 opt_opcode;
        word ~width:1 rc;
      ]
    | `XS, [opcode; rs; ra; sh1; opt; sh2; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh1;
        word ~width:9 opt;
        word ~width:1 sh2;
        word ~width:1 rc;
      ]
    | _ -> failwith "unexpected argument set for given insn form" in
  let () = match name with
    | None -> ()
    | Some name ->
      let _,_insn,insn_name = get_insn arch bytes in
      if not (String.equal name insn_name) then
        let err =
          sprintf "error: failed to construct %s insn, got a %s"
            name insn_name in
        failwith err in
  bytes
