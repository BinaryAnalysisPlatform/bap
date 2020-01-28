(** Native lifter of x86 instructions to the BAP IL *)
open Core_kernel
open Bap.Std

open X86_types
open X86_utils
open X86_env

(* extract the condition to jump on from the opcode bits
   for 70 to 7f and 0f 80 to 8f *)
let cc_to_exp i =
  let cc = match i land 0xe with
    | 0x0 -> Bil.var oF
    | 0x2 -> Bil.var cf
    | 0x4 -> Bil.var zf
    | 0x6 -> Bil.(var cf lor var zf)
    | 0x8 -> Bil.var sf
    | 0xa -> Bil.var pf
    | 0xc -> Bil.(var sf lxor var oF)
    | 0xe -> Bil.(var zf lor (var sf lxor var oF))
    | _ -> disfailwith X86 "impossible condition code"
  in
  if (i land 1) = 0 then cc else exp_not cc

let parse_instr mode mem addr =
  let add_to_addr addr value =
    let value = Word.extract_exn ~hi:(Addr.bitwidth addr - 1) value in
    Addr.(addr + value) in
  let g a =
    let open Or_error.Monad_infix in
    Memory.get ~addr:a mem >>= Word.to_int |>
    ok_exn |> Char.of_int_exn in
  let module R = (val (vars_of_mode mode)) in
  let s a = Addr.memref ~index:1 a in
  let bm = big_int_of_mode mode in
  let im = int_of_mode mode in
  let tm = type_of_mode mode in
  let get_prefix c =
    let i = Char.to_int c in
    match i with
    | 0xf0 | 0xf2 | 0xf3 | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65
    | 0x66 | 0x67 -> Some i
    | _ -> None
  in
  let get_prefixes a =
    let rex a = match mode with
      | X86 -> None, a
      | X8664 ->
        let i = Char.to_int (g a) in
        if i >= 0x40 && i <= 0x4f
        then Some i, s a
        else None, a
    in
    let rec f l a =
      if Addr.(a > Memory.max_addr mem) then (l, a)
      else match get_prefix (g a) with
        | Some p -> f (p::l) (s a)
        | None -> (l, a)
    in
    (* Legacy prefixes *)
    let leg, a = f [] a in
    (* Add rex *)
    let rex, a = rex a in
    rex, leg, a
  in
  let parse_rex i =
    {
      rex_w = i land 0x8 = 0x8;
      rex_r = i land 0x4 = 0x4;
      rex_x = i land 0x2 = 0x2;
      rex_b = i land 0x1 = 0x1;
    }
  in
  let get_vex a =
    if [%compare.equal: mode] mode X86 then None, a else
      match Char.to_int (g a) with
      (* 3-byte prefix *)
      | 0xc4 | 0x8f ->
        let a = (s a) in
        let b1, a = Char.to_int (g a), (s a) in
        let b2, a = Char.to_int (g a), (s a) in
        Some {
          vex_nr = b1 land 0x80 = 0x80;
          vex_nx = b1 land 0x40 = 0x40;
          vex_nb = b1 land 0x20 = 0x20;
          vex_map_select = b1 land 0x1f;
          vex_we = b2 land 0x80 = 0x80;
          vex_v = (b2 land 0x78) lsr 3;
          vex_l = b2 land 0x4 = 0x4;
          vex_pp = b2 land 0x3;
        }, a
      (* 2-byte prefix *)
      | 0xc5 ->
        let a = (s a) in
        let b1, a = Char.to_int (g a), (s a) in
        Some {
          vex_nr = b1 land 0x80 = 0x80;
          vex_nx = true;
          vex_nb = true;
          vex_map_select = 1;
          vex_we = false;
          vex_v = (b1 land 0x78) lsr 3;
          vex_l = b1 land 0x4 = 0x4;
          vex_pp = b1 land 0x3;
        }, a
      | _ -> None, a
  in

  (*  let int2prefix ?(jmp=false) = function
      | 0xf0 -> Some Lock
      | 0xf2 -> Some Repnz
      | 0xf3 -> Some Repz
      | 0x2e when jmp-> Some Hint_bnt
      | 0x3e when jmp-> Some Hint_bt
      | 0x2e -> Some(Override CS)
      | 0x36 -> Some(Override SS)
      | 0x3e -> Some(Override DS)
      | 0x26 -> Some(Override ES)
      | 0x64 -> Some(Override FS)
      | 0x65 -> Some(Override GS)
      | 0x66 -> Some Op_size
      | 0x0f -> Some Mandatory_0f
      | 0x67 -> Some Address_size
      | _ -> None
      in*)
  let parse_int scale a =
    Memory.get ~scale ~addr:a mem |> Or_error.ok_exn,
    Addr.memref ~index:1 ~scale a in
  let parse_int8 = parse_int `r8 in
  let parse_int16 = parse_int `r16 in
  let parse_int32 = parse_int `r32 in
  let parse_int64 = parse_int `r64 in
  let parse_sint scale a =
    let i, na = parse_int scale a in
    Word.signed i, na
  in
  let parse_sint8 = parse_sint `r8 in
  let parse_sint16 = parse_sint `r16 in
  let parse_sint32 = parse_sint `r32 in
  let parse_sint64 = parse_sint `r64 in
  let parse_disp8 = parse_sint8 in
  let parse_disp16 = parse_sint16 in
  let parse_disp32 = parse_sint32 in
  let parse_disp64 = parse_sint64 in
  let disfailwith = disfailwith mode in
  let unimplemented = unimplemented mode in
  let parse_disp =
    let open Type in
    function
    | Imm 8 ->  parse_disp8
    | Imm 16 -> parse_disp16
    | Imm 32 -> parse_disp32
    | Imm 64 -> parse_disp64
    | _ -> disfailwith "unsupported displacement size"
  in
  let parse_imm8cb b =
    let open Pcmpstr in
    let is_zero n = Word.(extract_exn ~hi:n ~lo:n b |> is_zero) in
    let is_one n = Word.(extract_exn ~hi:n ~lo:n b |> is_one) in
    let ssize = if is_zero 0 then Bytes else Words in
    let ssign = if is_zero 1 then Unsigned else Signed in
    let agg =
      let open Or_error in
      let code = Word.extract ~hi:3 ~lo:2 b >>= Word.to_int |> ok_exn in
      match  code with
      | 0 -> EqualAny
      | 1 -> Ranges
      | 2 -> EqualEach
      | 3 -> EqualOrdered
      | _ -> failwith "impossible"
    in
    let negintres1 = is_one 4 in
    let maskintres1 = is_one 5 in
    let outselectsig = if is_zero 6 then LSB else MSB in
    let outselectmask = sig_to_mask outselectsig in
    (* XXX commented out because we currently have no debugging module *)
    (*if (b & 128L) <> 0L then wprintf "Most significant bit of Imm8 control byte should be set to 0";*)

    {ssize=ssize; ssign=ssign; agg=agg; negintres1=negintres1; maskintres1=maskintres1; outselectsig=outselectsig; outselectmask=outselectmask}

  in
  let parse_sib rex modb a =
    (* ISR 2.1.5 Table 2-3 *)
    let e b = (if b then 1 else 0) lsl 3 in (* tuareg >> *)
    let rex_b, rex_x = match rex with
      | Some {rex_b; rex_x; _} -> rex_b, rex_x
      | None -> false, false
    in
    let b = Char.to_int (g a) in
    let bits2rege = match mode with
      | X86 -> bits2reg32e
      | X8664 -> bits2reg64e
    in
    let ss = b lsr 6 and idx = ((b lsr 3) land 7) lor (e rex_x) in
    let base, na =
      match (b land 7, modb land 7) with (* base register, MOD *)
      | 5, 0 -> let (i,na) = parse_disp32 (s a) in (bm i |> Bil.int, na)
      | _, 0 | _, 1 | _, 2 -> (bits2rege mode ((b land 7) lor (e rex_b)), s a)
      | _ -> disfailwith (Printf.sprintf "impossible opcode: sib b=%02x" b)
    in
    if idx = 4 then (base, na) else
      let idx = bits2rege mode idx in
      if ss = 0 then Bil.(base + idx, na)
      else Bil.(base + (idx lsl Int (im ss)), na)
  in
  (* Parse mod/rm bits helper function *)
  let parse_modrmbits a =
    let b = Char.to_int (g a)
    and na = s a in
    let r = (b lsr 3) land 7
    and m = b lsr 6
    and rm = b land 7 in
    (b, r, m, rm, na)
  in
  (* Parse mod/rm bits, but also apply bits from REX prefix *)
  let parse_modrmbits64 rex a =
    let e b = (if b then 1 else 0) lsl 3 in
    let b, r, modb, rm, na = parse_modrmbits a in
    let rex_r, _rex_x, rex_b = match rex with
      | Some {rex_r; rex_x; rex_b; _} -> rex_r, rex_x, rex_b
      | None -> false, false, false
    in
    let r = e rex_r lor r in
    let rm = e rex_b lor rm in
    b, r, modb, rm, na
  in
  (* Parse mod/rm operands for 16-bit addressing mode *)
  let parse_modrm16int _a _b r modb rm na =
    (* ISR 2.1.5 Table 2-1 *)
    match modb land 7 with (* MOD *)
    | 0 -> (match rm land 7 with
        | 6 -> let (disp, na) = parse_disp16 na in (r, Oaddr(Bil.Int (bm disp)), na)
        | n when n < 8 -> (r, Oaddr (Bil.(cast unsigned !!tm (eaddr16 mode rm))), na)
        | _ -> disfailwith "Impossible"
      )
    | 1 | 2 ->
      let (base, na) = eaddr16 mode rm, na in
      let (disp, na) =
        if (modb land 7) = 1 then parse_disp8 na else (*2*) parse_disp16 na
      in
      (r, Oaddr (Bil.(cast unsigned !!tm (base + b16 disp))), na)
    | 3 -> (r, Oreg rm, na)
    | _ -> disfailwith "Impossible"
  in
  (* External interface to get mod/rm operands for 16-bit addressing
     mode *)
  let parse_modrm16ext rex _iaoffset a =
    let b, r, modb, rm, na = parse_modrmbits64 rex a in
    parse_modrm16int a b r modb rm na
  in
  (* Same as parse_modrm16ext, but casts r as a register operand. *)
  let parse_modrm16 rex _iaoffset a =
    let (r, rm, na) = parse_modrm16ext rex _iaoffset a in
    (Oreg r, rm, na)
  in
  (* Same as parse_modrm16ext, but casts r as a segment selector
     operand. *)
  let parse_modrm16seg rex _iaoffset a =
    let (r, rm, na) = parse_modrm16ext rex _iaoffset a in
    (Oseg r, rm, na)
  in
  (* parse_modrm3264int uses the rex prefix to extended registers, but
     vex prefixes can also specify these bits.  This function converts
     from the vex prefix to the canonical rex form used by
     parse_modrm3264int. *)
  let convert_vex rex vex = match rex, vex with
    | Some _, Some _ -> failwith "both rex and vex specified"
    | Some _, None -> rex (* already in rex form *)
    | None, Some {vex_nr; vex_nx; vex_nb; vex_we; _} ->
      Some {
        rex_r=not vex_nr;
        rex_x=not vex_nx;
        rex_b=not vex_nb;
        rex_w=vex_we;
      }
    | None, None -> None
  in
  (* Parse mod/rm operands for 32 and 64-bit addressing modes. *)
  let parse_modrm3264int rex at _ia _a _b r modb rm naoffset na =
    (* ISR 2.1.5 Table 2-2 *)
    let bm = big_int_of_mode mode in
    let bits2rege =
      let open Type in
      match at with
      | Imm 32 -> fun b -> Bil.(cast unsigned !!(type_of_mode mode) (bits2reg32e mode b))
      | Imm 64 -> bits2reg64e mode
      | _ -> failwith "parse_modrm3264int: invalid address type"
    in
    match modb land 7 with (* MOD *)
    | 0 -> (match rm land 7 with (* rm & 7 ignores the extended rex bit *)
        | 4 -> let (sib, na) = parse_sib rex modb na in (r, Oaddr sib, na)
        | 5 ->
          (* See Table 2-7. RIP-Relative Addressing.

             In 32-bit mode, this is a displacement.  In 64-bit mode,
             this is RIP + displacement
          *)
          (match mode with
           | X86 ->
             let (disp, na) = parse_disp32 na in (r, Oaddr (b32 disp), na)
           | X8664 ->
             let immoff = match naoffset with
               | Some (Type.Imm nbits) -> int_exp (nbits / 8) 64
               | _ -> int_exp 0 64
             in
             let (disp, na) = parse_disp32 na in (r, Oaddr Bil.(b64 disp + b64 na + immoff), na))
        | _ -> (r, Oaddr(bits2rege rm), na)
      )
    | 1 | 2 ->
      let (base, na) =
        (* rm & 7 ignores the extended rex bit *)
        if 4 = (rm land 7) then parse_sib rex modb na else (bits2rege rm, na) in
      let (disp, na) =
        if modb = 1 then parse_disp8 na else (*2*) parse_disp32 na in
      (r, Oaddr Bil.(base + int (bm disp)), na)
    | 3 -> (r, Oreg rm, na)
    | _ -> disfailwith "Impossible"
  in
  (* External interface to get mod/rm operands for 32 and 64-bit
     addressing modes. *)
  let parse_modrm3264ext rex vex at ia immoff a =
    let rex = convert_vex rex vex in
    let b, r, modb, rm, na = parse_modrmbits64 rex a in
    parse_modrm3264int rex at ia a b r modb rm immoff na
  in
  (* Same as parse_modrm3264ext, but casts r as a register. *)
  let parse_modrm3264 rex vex at ia immoff a =
    let r, rm, na = parse_modrm3264ext rex vex at ia immoff a in
    (Oreg r, rm, na)
  in
  (* Same as parse_modrm3264ext, but casts r as a segment selector. *)
  let parse_modrm3264seg rex vex at ia iaoffset a =
    let (r, rm, na) = parse_modrm3264ext rex vex at ia iaoffset a in
    (* Ignore bits from REX prefix for segment selectors *)
    (Oseg (r land 7), rm, na)
  in
  (* Get the extra vvvv operand from the VEX prefix *)
  let get_vex_opr vex = match vex with
    | None -> None
    | Some {vex_v; _} -> Some (Ovec ((lnot vex_v) land 0xf))
  in
  (* Force an operand to be a vector register if it's a register operand. *)
  let tovec op = match op with
    | Oreg r -> Ovec r
    | _ -> op
  in
  let toreg op = match op with
    | Ovec r -> Oreg r
    | _ -> op
  in
  (* Same as parse_modrm3264ext, but casts r, rm, and the optional vex
     operand as vector registers. *)
  let parse_modrm3264_vec rex vex at ia iaoffset a =
    let r, rm, na = parse_modrm3264 rex vex at ia iaoffset a in
    (tovec r, tovec rm, get_vex_opr vex, na)
  in
  (* Parse 8-bits as unsigned integer *)
  let parse_imm8 a = (* not sign extended *)
    let (i, na) = parse_int8 a in
    (Oimm i, na) in
  let parse_simm8 a = (* sign extended *)
    let (i, na) = parse_sint8 a in
    (Oimm i, na) in
  let parse_imm16 a =
    let (i, na) = parse_int16 a in
    (Oimm i, na) in
  let parse_simm16 a =
    let (i, na) = parse_sint16 a in
    (Oimm i, na) in
  let parse_imm32 a =
    let (i, na) = parse_int32 a in
    (Oimm i, na) in
  let parse_simm32 a =
    let (i, na) = parse_sint32 a in
    (Oimm i, na) in
  let parse_imm64 a =
    let (i, na) = parse_int64 a in
    (Oimm i, na) in
  let parse_simm64 a =
    let (i, na) = parse_sint64 a in
    (Oimm i, na)
  in
  let parse_immz t a =
    let open Type in
    match t with
    | Imm 8 -> parse_imm8 a
    | Imm 16 -> parse_imm16 a
    | Imm 32 -> parse_imm32 a
    | Imm 64 -> parse_imm64 a
    | Imm n -> disfailwith ("parse_immz unsupported size: "^(string_of_int n))
    | _ -> disfailwith "parse_immz unsupported size"
  in
  let parse_simm t a =
    let open Type in
    match t with
    | Imm 8 -> parse_simm8 a
    | Imm 16 -> parse_simm16 a
    | Imm 32 -> parse_simm32 a
    | Imm 64 -> parse_simm64 a
    | Imm n -> disfailwith ("parse_simm unsupported size: "^(string_of_int n))
    | _ -> disfailwith "parse_simm unsupported size"
  in
  let parse_immv = parse_immz in
  let parse_immb = parse_imm8 in
  let parse_immw = parse_imm16 in
  (* let parse_immd = parse_imm32 in *)
  let _parse_simmb = parse_simm8 in
  let _parse_simmw = parse_simm16 in
  let _parse_simmd = parse_simm32 in
  (* width changing oimm size *)
  let oimm_resize op size = match op with
    | Oimm d -> Oimm (Word.extract_exn ~hi:(!!size - 1) (Word.signed d))
    | _ -> disfailwith "oimm_resize only handles Oimm"
  in
  (* This is for opcodes like e9 *)
  let expanded_jump_type opsize = match mode with
    | X86 -> opsize
    | X8664 -> reg32_t
  in
  let get_opcode _pref ({rex; vex; rm_extend; addrsize; _} as prefix) a =
    let parse_disp_addr, parse_modrm_addr, parse_modrmseg_addr,
        parse_modrmext_addr =
      let open Type in
      match addrsize with
      | Imm 16 -> parse_disp16, parse_modrm16 rex, parse_modrm16seg rex, parse_modrm16ext rex
      | Imm 32 -> parse_disp32, parse_modrm3264 rex vex addrsize a, parse_modrm3264seg rex vex addrsize a, parse_modrm3264ext rex vex addrsize a
      | Imm 64 -> parse_disp64, parse_modrm3264 rex vex addrsize a, parse_modrm3264seg rex vex addrsize a, parse_modrm3264ext rex vex addrsize a
      | _ -> failwith "Bad address type"
    in
    let parse_modrm_vec = parse_modrm3264_vec rex vex addrsize a in
    let mi = int_of_mode mode in
    let _mi64 = int64_of_mode mode in
    let mbi = big_int_of_mode mode in
    let mt = type_of_mode mode in
    (* A VEX prefix always implies the first byte of 0x0f *)
    let b1, na = if Option.is_some vex then 0x0f, a else Char.to_int (g a), s a in
    match b1 with (* Table A-2 *)
    (*** 00 to 3d are near the end ***)
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 ->
      (Inc(prefix.opsize, Oreg(rm_extend lor (b1 land 7))), na)
    | 0x48 | 0x49 | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
      (Dec(prefix.opsize, Oreg(rm_extend lor (b1 land 7))), na)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
      (Push(prefix.bopsize, Oreg(rm_extend lor (b1 land 7))), na)
    | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f ->
      (Pop(prefix.bopsize, Oreg(rm_extend lor (b1 land 7))), na)
    | 0x63 when [%compare.equal: mode] mode X8664 ->
      let (r, rm, na) = parse_modrm_addr None na in
      (Movsx(prefix.opsize, r, reg32_t, rm), na)
    | 0x68 | 0x6a  ->
      let (o, na) =
        (* SWXXX Sign extend these? *)
        if b1=0x68 then parse_immz prefix.opsize na else parse_immb na
      in
      let size = match mode with
        | X86 -> prefix.opsize
        | X8664 -> reg64_t
      in
      (Push(size, o), na)
    | 0x69 | 0x6b ->
      let it =
        if b1 = 0x6b
        then reg8_t
        else if Type.equal prefix.opsize reg16_t then reg16_t
        else reg32_t
      in
      let (r, rm, na) = parse_modrm_addr (Some it) na in
      let (o, na) = parse_simm it na in
      (Imul(prefix.opsize, (false,r), rm, (oimm_resize o prefix.opsize)), na)
    | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79
    | 0x7a | 0x7b | 0x7c | 0x7d | 0x7e | 0x7f ->
      let (i,na) = parse_disp8 na in
      (Jcc(Jabs(Oimm(add_to_addr na i)), cc_to_exp b1), na)
    | 0x80 | 0x81 | 0x82 | 0x83 ->
      let it = match b1 with
        | 0x81 -> if Type.equal prefix.opsize reg64_t then reg32_t else prefix.opsize
        | _ -> reg8_t
      in
      let (r, rm, na) = parse_modrmext_addr (Some it) na in
      let (o, na) = parse_immz it na in
      let (o2, na) = ((oimm_resize o prefix.opsize), na) in
      let opsize = if b1 land 1 = 0 then reg8_t else prefix.opsize in
      (match r with (* Grp 1 *)
       | 0 -> (Add(opsize, rm, o2), na)
       | 1 -> (Or(opsize, rm, o2), na)
       | 2 -> (Adc(opsize, rm, o2), na)
       | 3 -> (Sbb(opsize, rm, o2), na)
       | 4 -> (And(opsize, rm, o2), na)
       | 5 -> (Sub(opsize, rm, o2), na)
       | 6 -> (Xor(opsize, rm, o2), na)
       | 7 -> (Cmp(opsize, rm, o2), na)
       | _ -> disfailwith
                (Printf.sprintf "impossible Grp 1 opcode: %02x/%d" b1 r)
      )
    | 0x84
    | 0x85 -> let (r, rm, na) = parse_modrm_addr None na in
      let o = if b1 = 0x84 then reg8_t else prefix.opsize in
      (Test(o, rm, r), na)
    | 0x87 -> let (r, rm, na) = parse_modrm_addr None na in
      (Xchg(prefix.opsize, r, rm), na)
    | 0x88 -> let (r, rm, na) = parse_modrm_addr None na in
      (Mov(reg8_t, rm, r, None), na)
    | 0x89 -> let (r, rm, na) = parse_modrm_addr None na in
      (Mov(prefix.opsize, rm, r, None), na)
    | 0x8a -> let (r, rm, na) = parse_modrm_addr None na in
      (Mov(reg8_t, r, rm, None), na)
    | 0x8b -> let (r, rm, na) = parse_modrm_addr None na in
      (Mov(prefix.opsize, r, rm, None), na)
    | 0x8c -> let (r, rm, na) = parse_modrmseg_addr None na in
      let extend = if Type.equal prefix.opsize reg64_t then reg64_t else reg16_t in
      (Mov(extend, rm, r, None), na)
    | 0x8d -> let (r, rm, na) = parse_modrm_addr None na in
      (match rm with
       | Oaddr a -> (Lea(prefix.opsize, r, a), na)
       | _ -> disfailwith "invalid lea (must be address)")
    | 0x8e -> let (r, rm, na) = parse_modrmseg_addr None na in
      (Mov(reg16_t, r, rm, None), na)
    | 0x90 -> (Nop, na)
    | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 ->
      let reg = Oreg (rm_extend lor (b1 land 7)) in
      (Xchg(prefix.opsize, o_rax, reg), na)
    | 0x98 -> let srct =
                let open Type in
                match prefix.opsize with
                | Imm 16 -> reg8_t
                | Imm 32 -> reg16_t
                | Imm 64 -> reg32_t
                | _ -> disfailwith "invalid opsize for CBW/CWDE/CWQE"
      in
      (Movsx(prefix.opsize, o_rax, srct, o_rax), na)
    | 0x9c -> (Pushf(prefix.bopsize), na)
    (* Intel says that popfq needs to have a REX.W prefix, but gas
       and clang both insist that no prefix is needed! *)
    | 0x9d -> (Popf(prefix.bopsize), na)
    | 0x9e -> (Sahf, na)
    | 0x9f -> (Lahf, na)
    | 0xa0 | 0xa1 ->
      let t = if b1 = 0xa0 then Type.imm 8 else prefix.opsize in
      let (addr, na) = parse_disp_addr na in
      (Mov(t, o_rax, Oaddr (mbi addr |> Bil.int), None), na)
    | 0xa2 | 0xa3 ->
      let t = if b1 = 0xa2 then reg8_t else prefix.opsize in
      let (addr, na) = parse_disp_addr na in
      (Mov(t, Oaddr (mbi addr |> Bil.int), o_rax, None), na)
    | 0xa4 -> (Movs reg8_t, na)
    | 0xa5 -> (Movs prefix.opsize, na)
    | 0xa6 -> (Cmps reg8_t, na)
    | 0xa7 -> (Cmps prefix.opsize, na)
    | 0xae -> (Scas reg8_t, na)
    | 0xaf -> (Scas prefix.opsize, na)
    | 0xa8 -> let (i, na) = parse_imm8 na in
      (Test(reg8_t, o_rax, i), na)
    | 0xa9 -> let it = if Type.equal prefix.opsize reg64_t then reg32_t else prefix.opsize in
      let (i,na) = parse_immz it na in
      (Test(prefix.opsize, o_rax, oimm_resize i prefix.opsize), na)
    | 0xaa -> (Stos reg8_t, na)
    | 0xab -> (Stos prefix.opsize, na)
    | 0xb0 | 0xb1 | 0xb2 | 0xb3 | 0xb4 | 0xb5 | 0xb6
    | 0xb7 -> let (i, na) = parse_imm8 na in
      (Mov(reg8_t, Oreg(rm_extend lor (b1 land 7)), i, None), na)
    | 0xb8 | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe
    | 0xbf -> let (i, na) = parse_immv prefix.opsize na in
      (Mov(prefix.opsize, Oreg(rm_extend lor (b1 land 7)), i, None), na)
    | 0xc2 | 0xc3 (* Near ret *)
    | 0xca | 0xcb (* Far ret *) ->
      let far_ret = if (b1 = 0xc2 || b1 = 0xc3) then false else true in
      if (b1 = 0xc3 || b1 = 0xcb) then (Retn(None, far_ret), na)
      else let (imm,na) = parse_immw na in
        (Retn(Some(mt, imm), far_ret), na)
    | 0xc6
    | 0xc7 -> let t = if b1 = 0xc6 then reg8_t else prefix.opsize in
      let it = match b1 with
        | 0xc6 -> reg8_t
        | 0xc7 when prefix.opsize_override -> reg16_t
        | 0xc7 -> reg32_t
        | _ -> failwith "impossible"
      in
      let (e, rm, na) = parse_modrmext_addr (Some it) na in
      let (i,na) = parse_immz it na in
      (match e with (* Grp 11 *)
       | 0 -> (Mov(t, rm, oimm_resize i t, None), na)
       | _ -> disfailwith (Printf.sprintf "Invalid opcode: %02x/%d" b1 e)
      )
    | 0xc9 -> (Leave (type_of_mode mode), na)
    | 0xcc -> (Interrupt3, na)
    | 0xcd -> let (i,na) = parse_imm8 na in
      (Interrupt(i), na)

    (* 0xd8-0xdf can be followed by a secondary opcode, OR a modrm
       byte. But the secondary opcode is only used when the modrm
       byte does not specify a memory address. *)
    | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf ->
      let b2, _ = parse_int8 na in
      let (r, rm, na) = parse_modrmext_addr None na in
      (match r, rm with
       | 2, Oaddr _ ->
         (match b1 with
          | 0xd9 | 0xdd -> (Fst(rm, false), na)
          | _ ->
            unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
         )
       | 3, Oaddr _ ->
         (match b1 with
          | 0xd9 | 0xdd -> (Fst(rm, true), na)
          | _ ->
            unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
         )
       | 5, Oaddr _ ->
         (match b1 with
          | 0xd9 -> (Fldcw rm, na)
          | 0xdb -> (Fld rm, na)
          | _ ->
            unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
         )
       | 7, Oaddr _ ->
         (match b1 with
          | 0xd9 -> (Fnstcw rm, na)
          | 0xdb -> (Fst(rm, true), na)
          | _ ->
            unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
         )
       | _, Oaddr _ ->
         unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
       | _, _ ->
         unimplemented (Printf.sprintf "unsupported opcode: %02x %s" b1 (Word.string_of_value b2))
      )

    | 0xe8 -> let t = expanded_jump_type prefix.opsize in
      let (i,na) = parse_disp t na in
      (* I suppose the width of the return address should be addrsize *)
      (Call (Oimm (add_to_addr na i), resize_word na !!addrsize), na)
    | 0xe9 -> let t = expanded_jump_type prefix.opsize in
      let (i,na) = parse_disp t na in
      (Jump (Jabs (Oimm (add_to_addr na i))), na)
    | 0xeb -> let (i,na) = parse_disp8 na in
      (Jump (Jabs (Oimm (add_to_addr na i))), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let immoff = if (b1 land 0xfe) = 0xc0 then Some reg8_t else None in
      let (r, rm, na) = parse_modrmext_addr immoff na in
      let opsize = if (b1 land 1) = 0 then reg8_t else prefix.opsize in
      let (amt, na) = match b1 land 0xfe with
        | 0xc0 -> parse_imm8 na
        | 0xd0 -> (Oimm Addr.b1, na)
        | 0xd2 -> (o_rcx, na)
        | _ ->
          disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      in
      let open Bil in
      (match r with (* Grp 2 *)
       | 0 -> (Rotate(LSHIFT, opsize, rm, amt, false),na)
       | 1 -> (Rotate(RSHIFT, opsize, rm, amt, false),na)
       (* SWXXX Implement these *)
       | 2 -> unimplemented
                (* (Rotate(LSHIFT, opsize, rm, amt, true),na) *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
       | 3 -> unimplemented
                (* (Rotate(RSHIFT, opsize, rm, amt, true),na) *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
       | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
       | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
       | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
       | _ -> disfailwith
                (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      )
    | 0xe3 ->
      let t = !!addrsize in
      let rcx_e = Bil.var R.rcx in
      let (i,na) = parse_disp8 na in
      (* (Jcc (Jrel (BV.litz na t, BV.litz i t), Bop.(rcx_e = Int (mi 0))), na) *)
      (Jcc (Jrel (resize_word na t, resize_word i t), Bil.(rcx_e = (mi 0 |> int))), na)
    | 0xf4 -> (Hlt, na)
    | 0xf6
    | 0xf7 -> let t = if b1 = 0xf6 then reg8_t else prefix.opsize in
      let it = if Type.equal t reg64_t then reg32_t else t in
      let (r, rm, na) = parse_modrmext_addr (Some it) na in
      (match r with (* Grp 3 *)
       | 0 ->
         let (imm, na) = parse_immz it na in
         (Test(t, rm, oimm_resize imm t), na)
       | 2 -> (Not(t, rm), na)
       | 3 -> (Neg(t, rm), na)
       | 4 ->
         (match b1 with
          | 0xf6 -> (Mul(t, rm), na)
          | 0xf7 -> (Mul(t, rm), na)
          | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
         )
       | 5 ->
         (match b1 with
          | 0xf6 -> (Imul(t, (true,o_rax), o_rax, rm), na)
          | 0xf7 -> (Imul(t, (true,o_rdx), o_rax, rm), na)
          | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
         )
       | 6 ->
         (match b1 with
          | 0xf6 -> (Div(reg8_t, rm) , na)
          | 0xf7 -> (Div(t, rm), na)
          | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
         )
       | 7 ->
         (match b1 with
          | 0xf6 -> (Idiv(reg8_t, rm) , na)
          | 0xf7 -> (Idiv(t, rm), na)
          | _ -> disfailwith
                   (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
         )
       | _ ->
         disfailwith (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      )
    | 0xfc -> (Cld, na)
    | 0xfe -> let (r, rm, na) = parse_modrmext_addr None na in
      (match r with (* Grp 4 *)
       | 0 -> (Inc (reg8_t, rm), na)
       | 1 -> (Dec (reg8_t, rm), na)
       | _ -> disfailwith
                (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      )
    | 0xff -> let (r, rm, na) = parse_modrmext_addr None na in
      let t = !!addrsize in
      (match r with (* Grp 5 *)
       | 0 -> (Inc (prefix.opsize, rm), na)
       | 1 -> (Dec (prefix.opsize, rm), na)
       | 2 -> (Call (rm, resize_word na t), na)
       | 3 -> unimplemented (* callf *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
       | 4 -> (Jump (Jabs rm), na)
       | 5 -> unimplemented (* jmpf *)
                (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
       | 6 -> let size = match mode with
           | X86 -> prefix.opsize
           | X8664 -> reg64_t
         in
         (Push(size, rm), na)
       | _ -> disfailwith
                (Printf.sprintf "impossible opcode: %02x/%d" b1 r)
      )
    (*** 00 to 3e ***)
    | b1 when b1 < 0x3e && (b1 land 7) < 6 ->
      (
        let ins a = match b1 lsr 3 with
          | 0 -> Add a
          | 1 -> Or a
          | 2 -> Adc a
          | 3 -> Sbb a
          | 4 -> And a
          | 5 -> Sub a
          | 6 -> Xor a
          | 7 -> Cmp a
          | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
        in
        let t = if (b1 land 1) = 0  then reg8_t else prefix.opsize in
        (* handle sign extended immediate cases *)
        let it = if Type.equal t reg64_t then reg32_t else t in
        let (o1, o2, na) = match b1 land 7 with
          | 0 | 1 -> let r, rm, na = parse_modrm_addr None na in
            (rm, r, na)
          | 2 | 3 -> let r, rm, na = parse_modrm_addr None na in
            (r, rm, na)
          | 4 -> let i, na = parse_immb na in
            (o_rax, i, na)
          | 5 -> let i, na = parse_immz it na in
            (o_rax, oimm_resize i t, na)
          | _ -> disfailwith (Printf.sprintf "impossible opcode: %02x" b1)
        in
        (ins(t, o1, o2), na)
      )
    (* Two byte opcodes *)
    | 0x0f -> (
        (* Add in the second implied vex prefix *)
        let b2, na = match vex with
          | Some {vex_map_select=2; _} -> 0x38, na
          | Some {vex_map_select=3; _} -> 0x3a, na
          | Some {vex_map_select=1; _} | None -> Char.to_int (g na), s na
          | Some {vex_map_select; _} -> disfailwith (Printf.sprintf "reserved mmmmmm vex value: %d" vex_map_select)
        in
        match b2 with (* Table A-3 *)
        | 0x01 ->
          let b3, nna = Char.to_int (g na), (s na) in
          (match b3 with
           | 0xd0 -> (Xgetbv, nna)
           | _ -> disfailwith (Printf.sprintf "unsupported opcode %02x %02x %02x" b1 b2 b3))
        | 0x05 when [%compare.equal: mode] mode X8664 -> (Syscall, na)
        | 0x1f ->
          (* Even though we don't use the operand to nop, we need to
             parse it to get the next address *)
          let _, _, na = parse_modrm_addr None na in
          (Nop, na)
        | 0x10 | 0x11 when (prefix.repeat || prefix.nrepeat) -> (* MOVSS, MOVSD *)
          let r, rm, rv, na = parse_modrm_vec None na in
          let t = if prefix.repeat then reg32_t else reg64_t in
          let d, s, td = if b2 = 0x10 then r, rm, reg128_t else rm, r, t in
          (match rm, rv with
           | Ovec _, Some rv ->
             let nt = !!t in
             (Movoffset((reg128_t, d),
                        {offlen=Type.imm (128 - nt); offtyp=reg128_t; offop=rv; offsrcoffset=nt; offdstoffset=nt}
                        :: {offlen=t; offtyp=reg128_t; offop=s; offsrcoffset=0; offdstoffset=0} :: []), na)
           | Ovec _, None ->
             (Movdq(t, s, t, d, false), na)
           | Oaddr _, _ ->
             (Movdq(t, s, td, d, false), na)
           | _ -> disfailwith "impossible")
        | 0x12 | 0x13 | 0x16 | 0x17 -> (* MOVLPS, MOVLPD, MOVHPS, MOVHPD, MOVHLPS, MOVHLPD, MOVLHPS, MOVLHPD *)
          let r, rm, rv, na = parse_modrm_vec None na in
          let tdst, dst, telt, tsrc1, src1, off_src1, off_dst1, src2 =
            match b2 with
            | (0x12 | 0x16) when Option.is_some rv ->
              let offs1, offs2, offd1, offd2 = match b2, rm with
                | 0x12, Ovec _ -> 64, 64, 64, 0
                | 0x12, _ -> 64, 0, 64, 0
                | 0x16, _ -> 0, 0, 0, 64
                | _ -> disfailwith "impossible"
              in
              let rv = match rv with
                | Some r -> r
                | None -> disfailwith "impossible"
              in
              let src2 = [{offlen=reg64_t; offtyp=reg128_t; offop=rm; offsrcoffset=offs2; offdstoffset=offd2}] in
              reg128_t, r, reg64_t, reg128_t, rv, offs1, offd1, src2
            | 0x12 | 0x13 | 0x16 | 0x17 ->
              let offset = match b2 with
                | 0x12 | 0x13 -> 0
                | 0x16 | 0x17 -> 64
                | _ -> disfailwith "impossible"
              in
              let s, d, offs, offd = match b2, rm with
                | 0x12, Ovec _ -> rm, r, 64, 0
                | (0x12 | 0x16), _ -> rm, r, 0, offset
                | (0x13 | 0x17), _ -> r, rm, offset, 0
                | _ -> disfailwith "impossible"
              in
              let ts = match s with Ovec _ -> reg128_t | _ -> reg64_t in
              reg128_t, d, reg64_t, ts, s, offs, offd, []
            | _ -> disfailwith "impossible"
          in
          (Movoffset((tdst, dst),
                     [{offlen=telt; offtyp=tsrc1; offop=src1; offsrcoffset=off_src1; offdstoffset=off_dst1}]@src2), na)
        | 0x10 | 0x11 | 0x28 | 0x29 | 0x6e | 0x7e | 0x6f | 0x7f | 0xd6 ->
          (* REGULAR MOVDQ *)
          let r, rm, _, na = parse_modrm_vec None na in
          let src, dst, tsrc, tdst, align = match b2 with
            | 0x10 | 0x11 | 0x28 | 0x29 -> (* MOVUPS, MOVUPD, MOVAPS, MOVAPD *)
              let s, d = match b2 with
                | 0x10 | 0x28 -> rm, r
                | 0x11 | 0x29 -> r, rm
                | _ -> disfailwith "impossible"
              in
              let align = match b2 with
                | 0x10 | 0x11 -> false
                | 0x28 | 0x29 -> true
                | _ -> disfailwith "impossible"
              in
              let t = if Type.equal prefix.mopsize reg256_t then reg256_t else reg128_t in
              s, d, t, t, align
            | 0x6e | 0x7e -> (* MOVD, MOVQ *)
              let t = if Type.equal prefix.opsize reg64_t then reg64_t else reg32_t in
              let s, d, ts, td = match b2 with
                | 0x6e -> toreg rm, r, t, reg128_t
                | 0x7e when prefix.repeat -> rm, r, reg64_t, reg128_t
                | 0x7e -> r, toreg rm, t, t
                | _ -> disfailwith "impossible"
              in
              s, d, ts, td, false
            | 0x6f | 0x7f -> (* MOVQ, MOVDQA, MOVDQU *)
              let s, d = match b2 with
                | 0x6f -> rm, r
                | 0x7f -> r, rm
                | _ -> disfailwith "impossible"
              in
              let size = if prefix.repeat && Option.is_none prefix.vex then reg128_t else prefix.mopsize in
              let align = if prefix.opsize_override then true else false in
              s, d, size, size, align
            | 0xd6 -> (* MOVQ *)
              r, rm, reg64_t, reg64_t, false
            | _ -> unimplemented
                     (Printf.sprintf "mov opcode case missing: %02x" b2)
          in
          (Movdq(tsrc, src, tdst, dst, align), na)
        | 0x31 -> (Rdtsc, na)
        | 0x34 -> (Sysenter, na)
        | 0x38 ->
          (* Three byte opcodes *)
          let b3 = Char.to_int (g na) and na = s na in
          (match b3 with
           | 0x00 ->
             let d, s, rv, na = parse_modrm_vec None na in
             (Pshufb(prefix.mopsize, d, s, rv), na)
           | 0x17 when prefix.opsize_override ->
             let d, s, _, na = parse_modrm_vec None na in
             (Ptest(prefix.mopsize, d, s), na)
           | 0x29 when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             (Pcmp(prefix.mopsize, Type.imm 64, Bil.EQ, "pcmpeq", r, rm, rv), na)
           | 0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x25
           | 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 when prefix.opsize_override ->
             (* pmovsx and pmovzx *)
             let r, rm, _, na = parse_modrm_vec None na in
             (* determine sign/zero extension *)
             let ext, name = match (b3 land 0xf0) with
               | 0x20 -> Bil.signed, "pmovsx"
               | 0x30 -> Bil.unsigned, "pmovzx"
               | _ -> disfailwith "impossible"
             in
             (* determine dest/src element size *)
             let dstet, srcet, fullname = match (b3 land 0x0f) with
               | 0x00 -> reg16_t, reg8_t, name ^ "bw"
               | 0x01 -> reg32_t, reg8_t, name ^ "bd"
               | 0x02 -> reg64_t, reg8_t, name ^ "bq"
               | 0x03 -> reg32_t, reg16_t, name ^ "wd"
               | 0x04 -> reg64_t, reg16_t, name ^ "wq"
               | 0x05 -> reg64_t, reg32_t, name ^ "dq"
               | _ -> disfailwith "impossible"
             in
             (Pmov(prefix.mopsize, dstet, srcet, r, rm, ext, fullname), na)
           | 0x37 when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             (Pcmp(prefix.mopsize, Type.imm 64, Bil.SLT, "pcmpgt", r, rm, rv), na)
           | 0x38 when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 8, min_symbolic ~is_signed:true, "pminsb", r, rm, rv), na
           | 0x39 when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 32, min_symbolic ~is_signed:true, "pminsd", r, rm, rv), na
           | 0x3a when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 16, min_symbolic ~is_signed:false, "pminuw", r, rm, rv), na
           | 0x3b when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 32, min_symbolic ~is_signed:false, "pminud", r, rm, rv), na
           | 0x3c when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 8, max_symbolic ~is_signed:true, "pmaxsb", r, rm, rv), na
           | 0x3d when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 32, max_symbolic ~is_signed:true, "pmaxsd", r, rm, rv), na
           | 0x3e when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 16, max_symbolic ~is_signed:false, "pmaxuw", r, rm, rv), na
           | 0x3f when prefix.opsize_override ->
             let r, rm, rv, na = parse_modrm_vec None na in
             Ppackedbinop(prefix.mopsize, Type.imm 32, max_symbolic ~is_signed:false, "pmaxud", r, rm, rv), na
           | _ -> disfailwith (Printf.sprintf "opcode unsupported: 0f 38 %02x" b3))
        | 0x3a ->
          let b3 = Char.to_int (g na) and na = s na in
          (match b3 with
           | 0x0f ->
             let r, rm, rv, na = parse_modrm_vec (Some reg8_t) na in
             let i, na = parse_imm8 na in
             (Palignr(prefix.mopsize, r, rm, rv, i), na)
           | 0x60 | 0x61 | 0x62 | 0x63 ->
             let r, rm, _, na = parse_modrm_vec (Some reg8_t) na in
             let i, na = parse_imm8 na in
             (match i with
              | Oimm imm ->
                let open Pcmpstr in
                let imm8cb = parse_imm8cb imm in
                let pcmp = {out=if b3 land 0x1 = 0x1 then Index else Mask;
                            len=if b3 land 0x2 = 0x2 then Implicit else Explicit} in
                (Pcmpstr(prefix.mopsize, r, rm, i, imm8cb, pcmp), na)
              | _ ->  unimplemented "unsupported non-imm op for pcmpistri")
           | _ ->  unimplemented
                     (Printf.sprintf "unsupported opcode %02x %02x %02x" b1 b2 b3)
          )
        (* conditional moves *)
        | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49
        | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
          let (r, rm, na) = parse_modrm_addr None na in
          (Mov(prefix.opsize, r, rm, Some(cc_to_exp b2)), na)
        | 0x57 ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let t = if Type.equal prefix.mopsize reg256_t then reg256_t else reg128_t in
          (Ppackedbinop(t, prefix.opsize, Bil.(lxor), "xorp", r, rm, rv), na)
        | 0x60 | 0x61 | 0x62 | 0x68 | 0x69 | 0x6a ->
          let order = match b2 with
            | 0x60 | 0x61 | 0x62 -> Low
            | 0x68 | 0x69 | 0x6a -> High
            | _ -> disfailwith "impossible"
          in
          let elemt = match b2 with
            | 0x60 | 0x68 -> Type.imm 8
            | 0x61 | 0x69 -> Type.imm 16
            | 0x62 | 0x6a -> Type.imm 32
            | _ -> disfailwith "impossible"
          in
          let r, rm, rv, na = parse_modrm_vec None na in
          (Punpck(prefix.mopsize, elemt, order, r, rm, rv), na)
        | 0x6c | 0x6d when prefix.opsize_override ->
          let order = match b2 with
            | 0x6c -> Low
            | 0x6d -> High
            | _ -> disfailwith "impossible"
          in
          let elemt = Type.imm 64 in
          let r, rm, rv, na = parse_modrm_vec None na in
          (Punpck(prefix.mopsize, elemt, order, r, rm, rv), na)
        | 0x64 | 0x65 | 0x66 | 0x74 | 0x75 | 0x76  as o ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let elet = match o land 0x6 with | 0x4 -> reg8_t | 0x5 -> reg16_t | 0x6 -> reg32_t | _ ->
            disfailwith "impossible" in
          let bop, bstr = match o land 0x70 with
            | 0x70 -> Bil.EQ, "pcmpeq"
            | 0x60 -> Bil.SLT, "pcmpgt"
            | _ -> disfailwith "impossible" in
          (Pcmp(prefix.mopsize, elet, bop, bstr, r, rm, rv), na)
        | 0x70 when Type.equal prefix.opsize reg16_t ->
          let r, rm, rv, na = parse_modrm_vec (Some reg8_t) na in
          let i, na = parse_imm8 na in
          (Pshufd(prefix.mopsize, r, rm, rv, i), na)
        | 0x71 | 0x72 | 0x73 ->
          let t = prefix.mopsize in
          let r, rm, rv, na = parse_modrm_vec (Some reg8_t) na in
          let i, na = parse_imm8 na in
          let bi8 = Word.of_int ~width:8 0x8 in
          let fbop, str, et, i =
            let open Bil in
            match b2, r, i with
            | _, Ovec 2, _ -> Bil.binop RSHIFT, "psrl", lowbits2elemt b2, i
            | _, Ovec 6, _ -> Bil.binop LSHIFT, "psll", lowbits2elemt b2, i
            | _, Ovec 4, _ -> Bil.binop ARSHIFT, "psra", lowbits2elemt b2, i
            (* The shift amount of next two elements are multiplied by eight *)
            | 0x73, Ovec 3, Oimm i when prefix.opsize_override -> Bil.binop RSHIFT, "psrldq", t, Oimm (Addr.(i * bi8))
            | 0x73, Ovec 7, Oimm i when prefix.opsize_override -> Bil.binop LSHIFT, "pslldq", t, Oimm (Addr.(i * bi8))
            | _, Oreg i, _ -> disfailwith (Printf.sprintf "invalid psrl/psll encoding b2=%#x r=%#x" b2 i)
            | _ -> disfailwith "impossible"
          in
          (Ppackedbinop(t, et, fbop, str, rm, i, rv), na)
        | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89
        | 0x8a | 0x8b | 0x8c | 0x8d | 0x8e | 0x8f ->
          let t = expanded_jump_type prefix.opsize in
          let (i,na) = parse_disp t na in
          (Jcc(Jabs(Oimm(add_to_addr na i)), cc_to_exp b2), na)
        (* add other opcodes for setcc here *)
        | 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 | 0x98 | 0x99
        | 0x9a | 0x9b | 0x9c | 0x9d | 0x9e | 0x9f ->
          let _r, rm, na = parse_modrm_addr None na in
          (* unclear what happens otherwise *)
          assert (Type.equal prefix.opsize reg32_t);
          (Setcc(reg8_t, rm, cc_to_exp b2), na)
        | 0xa2 -> (Cpuid, na)
        | 0xa3 | 0xba ->
          let it = if b2 = 0xba then Some reg8_t else None in
          let (r, rm, na) = parse_modrm_addr it na in
          let r, na = if b2 = 0xba then parse_imm8 na else r, na in
          (Bt(prefix.opsize, r, rm), na)
        | 0xa4 ->
          (* shld *)
          let (r, rm, na) = parse_modrm_addr (Some reg8_t) na in
          let (i, na) = parse_imm8 na in
          (Shiftd(Bil.LSHIFT, prefix.opsize, rm, r, i), na)
        | 0xa5 ->
          (* shld *)
          let (r, rm, na) = parse_modrm_addr None na in
          (Shiftd(Bil.LSHIFT, prefix.opsize, rm, r, o_rcx), na)
        | 0xac ->
          (* shrd *)
          let (r, rm, na) = parse_modrm_addr (Some reg8_t) na in
          let (i, na) = parse_imm8 na in
          (Shiftd(Bil.RSHIFT, prefix.opsize, rm, r, i), na)
        | 0xad ->
          (* shrd *)
          let (r, rm, na) = parse_modrm_addr None na in
          (Shiftd(Bil.RSHIFT, prefix.opsize, rm, r, o_rcx), na)
        | 0xae ->
          let (r, rm, na) = parse_modrmext_addr None na in
          (match r with
           | 2 -> (Ldmxcsr rm, na) (* ldmxcsr *)
           | 3 -> (Stmxcsr rm, na) (* stmxcsr *)
           | _ -> unimplemented
                    (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
          )
        | 0xaf ->
          let (r, rm, na) = parse_modrm_addr None na in
          (Imul(prefix.opsize, (false,r), r, rm), na)
        | 0xb1 ->
          let r, rm, na = parse_modrm_addr None na in
          (Cmpxchg (prefix.opsize, r, rm), na)
        | 0xb6
        | 0xb7 -> let st = if b2 = 0xb6 then reg8_t else reg16_t in
          let r, rm, na = parse_modrm_addr None na in
          (Movzx(prefix.opsize, r, st, rm), na)
        | 0xb8 when prefix.repeat ->
          let r, rm, na = parse_modrm_addr None na in
          (Popcnt (prefix.opsize, rm, r), na)
        | 0xbc | 0xbd ->
          let dir = match b2 with | 0xbc -> Forward | 0xbd -> Backward | _ -> failwith "impossible" in
          let r, rm, na = parse_modrm_addr None na in
          (Bs (prefix.opsize, r, rm, dir), na)
        | 0xbe
        | 0xbf -> let st = if b2 = 0xbe then reg8_t else reg16_t in
          let r, rm, na = parse_modrm_addr None na in
          (Movsx(prefix.opsize, r, st, rm), na)
        | 0xc1 ->
          let r, rm, na = parse_modrm_addr None na in
          (Xadd(prefix.opsize, r, rm), na)
        | 0xc7 ->
          let r, rm, na = parse_modrmext_addr None na in
          (match r with
           | 1 -> (Cmpxchg8b(rm), na)
           | _ -> unimplemented
                    (Printf.sprintf "unsupported opcode: %02x %02x/%d" b1 b2 r)
          )
        | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf ->
          (Bswap(prefix.opsize, Oreg(rm_extend lor (b2 land 7))), na)
        | 0xd1 | 0xd2 | 0xd3 | 0xe1 | 0xe2 | 0xf1 | 0xf2 | 0xf3 ->
          let t = prefix.mopsize in
          let r, rm, rv, na = parse_modrm_vec None na in
          let et = lowbits2elemt b2 in
          let fbop, str = match b2 land 0xf0 with
            | 0xd0 -> Bil.(lsr), "psrl"
            | 0xe0 -> Bil.(asr), "psra"
            | 0xf0 -> Bil.(lsl), "psll"
            | _ -> disfailwith "invalid"
          in
          (Ppackedbinop(t, et, fbop, str, r, rm, rv), na)
        | 0xda ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Ppackedbinop(prefix.mopsize, Type.imm 8, min_symbolic ~is_signed:false, "pminub", r, rm, rv), na)
        | 0xdb ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Pbinop(prefix.mopsize, Bil.(land), "pand", r, rm, rv), na)
        | 0xd7 ->
          let r, rm, na = parse_modrm_addr None na in
          let r, rm = r, tovec rm in
          (Pmovmskb(prefix.mopsize, r, rm), na)
        | 0xde ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Ppackedbinop(prefix.mopsize, Type.imm 8, max_symbolic ~is_signed:false, "pmaxub", r, rm, rv), na)
        | 0xdf ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let andn x y = Bil.(lnot x land y) in
          (Pbinop(prefix.mopsize, andn, "pandn", r, rm, rv), na)
        | 0xe0 | 0xe3 ->
          (* pavg *)
          let r, rm, rv, na = parse_modrm_vec None na in
          (* determine whether we're using bytes or words *)
          let et = match b2 land 0x0f with
            | 0x00 -> reg8_t
            | 0x03 -> reg16_t
            | _ -> disfailwith "invalid"
          in
          let one = int_exp 1 !!et in
          let average x y = Bil.(((x + y) + one) lsr one) in
          (Ppackedbinop(prefix.mopsize, et, average, "pavg", r, rm, rv), na)
        | 0xea ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Ppackedbinop(prefix.mopsize, Type.imm 16, min_symbolic ~is_signed:true, "pminsw", r, rm, rv), na)
        | 0xeb ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Pbinop(prefix.mopsize, Bil.(lor), "por", r, rm, rv), na)
        | 0xee ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Ppackedbinop(prefix.mopsize, Type.imm 16, max_symbolic ~is_signed:true, "pmaxsw", r, rm, rv), na)
        | 0xef ->
          let r, rm, rv, na = parse_modrm_vec None na in
          (Pbinop(prefix.mopsize, Bil.(lxor), "pxor", r, rm, rv), na)
        | 0xf0 ->
          let r, rm, _, na = parse_modrm_vec None na in
          let t = if Type.equal prefix.mopsize reg256_t then reg256_t else reg128_t in
          (Movdq(t, rm, t, r, false), na)
        | 0xf8 | 0xf9 | 0xfa | 0xfb ->
          let r, rm, rv, na = parse_modrm_vec None na in
          let eltsize = match b2 land 7 with
            | 0 -> reg8_t
            | 1 -> reg16_t
            | 2 -> reg32_t
            | 3 -> reg64_t
            | _ -> disfailwith "impossible"
          in
          (* XXX I should just have put in a binop *)
          (Ppackedbinop(prefix.mopsize, eltsize, Bil.(-), "psub", r, rm, rv), na)
        | _ -> unimplemented
                 (Printf.sprintf "unuspported opcode: %02x %02x" b1 b2)
      )
    | n -> unimplemented (Printf.sprintf "unsupported single opcode: %02x" n)

  in
  let rex, pref, a = get_prefixes addr in
  let vex, a = get_vex a in
  (* Append VEX implied mandatory prefixes *)
  let pref = match vex with
    | Some {vex_pp=1; _} -> 0x66 :: pref
    | Some {vex_pp=2; _} -> 0xf3 :: pref
    | Some {vex_pp=3; _} -> 0xf2 :: pref
    | _ -> pref
  in
  let rex = match rex with
    | Some v -> Some (parse_rex v)
    | None   -> None in
  (* Opsize for regular instructions, MMX/SSE2 instructions

     The opsize override makes regular operands smaller, but MMX
     operands larger.  *)
  let modesize = type_of_mode mode in
  let opsize, bopsize, mopsize =
    if ints_mem pref pref_opsize
    then reg16_t,reg16_t,reg128_t else reg32_t,modesize,reg64_t
  in
  let opsize = match rex with
    | Some {rex_w=true; _} -> reg64_t (* See Table 3-4: Effective Operand-
                                         and Address-Size Attributes in 64-Bit Mode *)
    | Some {rex_w=false; _} | None -> opsize
  in
  let opsize = match vex with
    | Some {vex_we=true; _} -> reg64_t
    | _ -> opsize
  in
  let mopsize = match vex with
    | Some {vex_l=false; _} -> reg128_t
    | Some {vex_l=true; _} -> reg256_t
    | None -> mopsize
  in
  let addrsize = match mode with
    | X86 -> if ints_mem pref pref_addrsize then reg16_t else reg32_t
    | X8664 -> if ints_mem pref pref_addrsize then reg32_t else reg64_t
  in
  let r_extend, rm_extend, sib_extend =
    let e b = ((if b then 1 else 0) lsl 3) in
    match rex with
    | Some {rex_r; rex_b; rex_x; _} ->
      e rex_r, e rex_b, e rex_x
    | None ->
      (match vex with
       | Some {vex_nr; vex_nb; vex_nx; _} -> e (not vex_nr), e (not vex_nb), e (not vex_nx)
       | None -> 0, 0, 0)
  in

  let prefix =
    {
      addrsize;
      opsize;
      bopsize;
      mopsize;
      repeat = ints_mem pref repz;
      nrepeat = ints_mem pref repnz;
      addrsize_override = ints_mem pref pref_addrsize;
      opsize_override = ints_mem pref pref_opsize;
      rex;
      vex;
      r_extend;
      rm_extend;
      sib_extend;
    }
  in
  let op, a = get_opcode pref prefix a in
  (pref, prefix, op, a)

let parse_prefixes mode pref _ =
  let module R = (val (vars_of_mode mode)) in
  let open R in
  (* FIXME: how to deal with conflicting prefixes? *)
  let rec f s r = function
    | [] -> ((match s with
        | Some v -> Some v
        | None   -> None), List.rev r)
    | 0x2e::p -> f seg_cs r p
    | 0x36::p -> f seg_ss r p
    | 0x3e::p -> f seg_ds r p
    | 0x26::p -> f seg_es r p
    | 0x64::p -> f seg_fs r p
    | 0x65::p -> f seg_gs r p
    | 0xf0::p -> f s r p (* discard lock prefix *)
    | 0x66::p -> f s r p
    | p::ps -> f s (p::r) ps
  in
  f None [] pref
