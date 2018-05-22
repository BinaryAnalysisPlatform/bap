(** Native lifter of x86 instructions to the BAP IL *)

open Core_kernel.Std
open Bap.Std
open Format
open Bil.Types
open X86_types
open X86_utils
open X86_env
include Self()

module ToIR = struct

  let cf_e = Bil.var cf
  let pf_e = Bil.var pf
  let af_e = Bil.var af
  let zf_e = Bil.var zf
  let sf_e = Bil.var sf
  let of_e = Bil.var oF
  let df_e = Bil.var df

  let size_of_typ s = Size.of_int_exn !!s (** doubts here  *)

  (* stmt helpers *)

  let store_s mode s t a e =
    let mem = match mode with
      | X86 -> R32.mem
      | X8664 -> R64.mem in
    let sz = size_of_typ t in
    match s with
    | None -> Bil.Move (mem, Bil.(Store (Var mem, a, e, LittleEndian, sz)))
    | Some v -> Bil.Move (mem, Bil.(Store (Var mem, Var v + a, e,
                                           LittleEndian, sz)))

  (* copypasted from op2e_s below, but keeps the opcode width *)
  let op2e_s_keep_width mode ss has_rex t = function
    | Ovec r when t = reg256_t -> (bits2ymme mode r, t)
    | Ovec r when t = reg128_t -> (bits2ymm128e mode r, t)
    | Ovec r when t = reg64_t -> (bits2ymm64e mode r, t)
    | Ovec r when t = reg32_t -> (bits2ymm32e mode r, t)
    | Ovec _ ->
      let i = match t with
        | Type.Imm n -> ": "^(string_of_int n)
        | _ -> "" in
      disfailwith mode ("invalid SIMD register size for op2e"^i)
    | Oreg r when t = reg64_t -> (bits2reg64e mode r, t)
    | Oreg r when t = reg32_t -> (bits2reg32e mode r, t)
    | Oreg r when t = reg16_t -> (bits2reg16e mode r, t)
    | Oreg r when t = reg8_t -> (bits2reg8e mode ~has_rex r, t)
    | Oreg _ -> unimplemented mode "unknown register"
    | Oseg r when t = reg64_t -> Bil.(Cast (UNSIGNED, !!reg64_t, bits2segrege r), t)
    (* There is no 32-bit extension of segment selectors; this is not a bug *)
    | Oseg r when t = reg16_t -> (bits2segrege r, t)
    | Oseg _ -> (disfailwith mode "Segment register when t is not r16", t)
    | Oaddr e -> (load_s mode ss (size_of_typ t) e, t)
    | Oimm i -> Bil.(Int (resize_word i !!t), t)

  let op2e_s mode ss has_rex t = function
    | Ovec r when t = reg256_t -> bits2ymme mode r
    | Ovec r when t = reg128_t -> bits2ymm128e mode r
    | Ovec r when t = reg64_t -> bits2ymm64e mode r
    | Ovec r when t = reg32_t -> bits2ymm32e mode r
    | Ovec _ ->
      let i = match t with
        | Type.Imm n -> ": "^(string_of_int n)
        | _ -> "" in
      disfailwith mode ("invalid SIMD register size for op2e"^i)
    | Oreg r when t = reg64_t -> bits2reg64e mode r
    | Oreg r when t = reg32_t -> bits2reg32e mode r
    | Oreg r when t = reg16_t -> bits2reg16e mode r
    | Oreg r when t = reg8_t -> bits2reg8e mode ~has_rex r
    | Oreg _ -> unimplemented mode "unknown register"
    | Oseg r when t = reg64_t -> Bil.(Cast (UNSIGNED, !!reg64_t, bits2segrege r))
    (* There is no 32-bit extension of segment selectors; this is not a bug *)
    | Oseg r when t = reg16_t -> bits2segrege r
    | Oseg _ -> disfailwith mode "Segment register when t is not r16"
    | Oaddr e -> load_s mode ss (size_of_typ t) e
    | Oimm i -> Bil.Int (resize_word i !!t)

  let assn_s mode s has_rex has_vex t v e =
    (* Assign to some bits of v, starting at bit off, while preserving the other bits *)
    let sub_assn ?(off=0) t v e =
      let concat_exps = ref [] in
      let bits = !!(Var.typ v) in
      let assnbits = !!t in

      (* Add the upper preserved bits, if any *)
      let ubh = (bits-1) and ubl = (assnbits+off) in
      if ubh > ubl then concat_exps := (Bil.(Extract (ubh, ubl, Var v)))::!concat_exps;

      (* Add e *)
      concat_exps := e::!concat_exps;

      (* Add the lower preserved bits, if any *)
      let lbh = (off-1) and lbl = 0 in
      if lbh > lbl then
        concat_exps := (Bil.(Extract (lbh, lbl, Var v)))::!concat_exps;

      let final_e = List.reduce_exn ~f:(fun big_e e -> Bil.Concat (e, big_e)) !concat_exps in
      Bil.Move (v, final_e)
    in
    let is8664 = mode = X8664 in
    match v, t with
    (* Zero-extend 128-bit assignments to 256-bit ymms. *)
    | Ovec r, Type.Imm (128|64|32) when has_vex ->
      let v = bits2ymm mode r in
      sub_assn reg256_t v Bil.(Cast (UNSIGNED, !!reg256_t, e))
    | Ovec r, Type.Imm (256|128|64|32) ->
      let v = bits2ymm mode r in
      sub_assn t v e
    | Ovec _, _ -> disfailwith mode "invalid SIMD register size for assignment"
    (* Zero-extend 32-bit assignments to 64-bit registers. *)
    | Oreg r, Type.Imm 32 when is8664 ->
      let v = bits2genreg mode r in
      sub_assn reg64_t v Bil.(Cast (UNSIGNED, !!reg64_t, e))
    | Oreg r, Type.Imm (64|32|16) ->
      let v = bits2genreg mode r in
      sub_assn t v e
    | Oreg r, Type.Imm 8 when r < 4 || (mode = X8664 && has_rex) ->
      let v = bits2genreg mode r in
      sub_assn t v e
    | Oreg r, Type.Imm 8 ->
      let v = bits2genreg mode (r land 3) in
      sub_assn ~off:8 t v e
    | Oreg _, _ -> unimplemented mode "assignment to sub registers"
    | Oseg r, _ when t = reg16_t ->
      let v = bits2segreg r in
      Bil.Move (v, e)
    | Oseg _, _ -> disfailwith mode "Can't assign to non 16 bit segment register"
    | Oaddr a, _ -> store_s mode s t a e
    | Oimm _, _ -> disfailwith mode "disasm x86: Can't assign to an immediate value"


  (* Double width operands, as used by multiplication and division *)
  let op_dbl t =
    let open Type in
    match t with
    | Imm 8 -> [reg16_t, o_rax]
    | Imm 16 -> [reg16_t, o_rdx; reg16_t, o_rax]
    | Imm 32 -> [reg32_t, o_rdx; reg32_t, o_rax]
    | Imm 64 -> [reg64_t, o_rdx; reg64_t, o_rax]
    | _ -> disfailwith X86 "op_dbl only defined for Reg 8, 16, 32, and 64"

  (* Return an expression for a double-width operand, as used by the div
     instruction. *)
  let op2e_dbl_s mode ss has_rex t =
    let cf (ct, o) = op2e_s mode ss has_rex ct o in
    let ol = List.map ~f:cf (op_dbl t) in
    List.reduce_exn
      ~f:(fun bige little -> Bil.(bige ^ little))
      ol

  (* Double width assignments, as used by multiplication *)
  let assn_dbl_s mode s has_rex has_vex t e =
    match op_dbl t with
    | (t,o) :: [] -> [assn_s mode s has_rex has_vex t o e], op2e_s mode s has_rex t o
    | l ->
      let tmp = tmp (Type.Imm (!!t * 2)) in
      let f (stmts, off) (ct, o) =
        let newoff = off + !!ct in
        assn_s mode s has_rex has_vex ct o (Bil.Extract (newoff-1, off, Bil.Var tmp))::stmts, newoff
      in
      List.rev (fst (List.fold_left ~f:f ~init:([Bil.Move (tmp, e)], 0) (List.rev l))), Bil.Var tmp

  (* A function for computing the target of jumps. *)
  let compute_jump_target mode s has_rex =
    let t = type_of_mode mode in
    function
    | Jabs o -> op2e_s mode s has_rex t o
    | Jrel (na,offset) ->
      (*let i,t = Arithmetic.binop PLUS (na,t) (offset,t) in*)
      Bil.((Int na) + (Int offset))
  let jump_target = compute_jump_target

  let string_incr mode t v =
    let i n = Bil.Int (int_of_mode mode n) in
    if t = reg8_t then
      Bil.Move (v, Bil.(Var v + df_to_offset mode df_e))
    else
      Bil.Move (v, Bil.(Var v + (df_to_offset mode df_e * i (bytes_of_width t))))

  let rep_wrap ?check_zf ~mode ~addr ~next stmts =
    let bi = big_int_of_mode mode in
    let bi0 = Word.b0 in
    let bi1 = Word.b1 in
    let endstmt = match check_zf with
      | None -> Bil.Jmp(Bil.Int (bi addr))
      | Some x when x = repz ->
        (* a conditional jump was replaced with the new If statement here *)
        Bil.If (zf_e, [Bil.Jmp (Bil.Int (bi addr))], [])
      | Some x when x = repnz ->
        Bil.If (zf_e, [], [Bil.Jmp (Bil.Int (bi addr))])
      | _ -> failwith "invalid value for ?check_zf"
    in
    let rcx = match mode with
      | X86 -> R32.rcx
      | X8664 -> R64.rcx in
    let rcx_e = Bil.Var rcx in
    let open Stmt in
    (* a conditional jump was replaced with the new If statement here *)
    (If (Bil.(rcx_e = (Int (bi bi0))), [Jmp (Bil.Int (bi next))], []))
    :: stmts
    @ Move (rcx, Bil.(rcx_e - (Int (bi bi1))))
      :: [(If (Bil.(rcx_e = (Int (bi bi0))), [Jmp (Bil.Int (bi next))], []))]
    @ [endstmt]

  let compute_sf result = Bil.(Cast (HIGH, !!bool_t, result))

  let compute_zf t result =
    let bi0 = BV.of_int ~width:t 0 in
    Bil.((Int bi0) = result)

  let compute_pf t r =
    let acc = tmp t in
    let var_acc = Bil.Var acc in
    let t' = !!t in
    (* extra parens do not change semantics but do make it pretty
       print nicer *)
    let open Bil in
    exp_not (Cast (LOW, !!bool_t,
                   ((Let(acc, (r lsr (int_exp 4 t')) lxor r, Let(acc, (var_acc lsr (int_exp 2 t')) lxor var_acc, (var_acc lsr (int_exp 1 t')) lxor var_acc))))))

  let set_sf r = Bil.Move (sf, compute_sf r)
  let set_zf t r = Bil.Move (zf, compute_zf t r)
  let set_pf t r = Bil.Move (pf, compute_pf t r)

  let set_pszf t r =
    let t' = !!t in
    [set_pf t r;
     set_sf r;
     set_zf t' r]

  (* Adjust flag

     AF is set when there is a carry to or borrow from bit 4 (starting
     at 0), when considering unsigned operands. Let X_i denote bit i of
     value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
     where c is the carry bit from the lower four bits. Since AF = c,
     and we want to know the value of AF, we can rewrite as AF = c = r_4
     - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
     2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
  *)

  let set_apszf t s1 s2 r =
    let bit4 = int_exp (1 lsl 4) t in
    let t = Type.Imm t in
    Bil.Move (af, Bil.(bit4 = (bit4 land ((r lxor s1) lxor s2))))
    ::set_pszf t r

  (* Helper functions to set flags for adding *)
  let set_aopszf_add t s1 s2 r =
    (* Move (oF, Bop.(Cast (CAST_HIGH, r1, (s1 = s2) land (s1 lxor r)))) *)
    let s1_high = Bil.(Cast (HIGH, !!bool_t, s1)) in
    let s2_high = Bil.(Cast (HIGH, !!bool_t, s2)) in
    let r_high  = Bil.(Cast (HIGH, !!bool_t, r)) in
    Bil.Move (oF, Bil.((s1_high = s2_high) land (s1_high lxor r_high)))
    ::set_apszf t s1 s2 r

  let set_flags_add t s1 s2 r =
    Bil.Move (cf, Bil.(r < s1))
    ::set_aopszf_add t s1 s2 r

  (* Helper functions to set flags for subtracting *)
  let set_apszf_sub t s1 s2 r = set_apszf t s1 s2 r

  let set_aopszf_sub t s1 s2 r =
    Bil.Move (oF, Bil.(Cast (HIGH, !!bool_t, (s1 lxor s2) land (s1 lxor r))))
    ::set_apszf_sub t s1 s2 r

  let set_flags_sub t s1 s2 r =
    Bil.Move (cf, Bil.(s2 > s1))
    ::set_aopszf_sub t s1 s2 r

  let rec to_ir mode addr next ss pref has_rex has_vex =
    let module R = (val (vars_of_mode mode)) in
    let open R in
    let load = load_s mode ss in (* Need to change this if we want seg_ds <> None *)
    let op2e = op2e_s mode ss has_rex in
    let op2e_keep_width = op2e_s_keep_width mode ss has_rex in
    let op2e_dbl = op2e_dbl_s mode ss has_rex in
    let store = store_s mode ss in
    let assn = assn_s mode ss has_rex has_vex in
    let assn_dbl = assn_dbl_s mode ss has_rex has_vex in
    let mi = int_of_mode mode in
    let mt = type_of_mode mode in
    let rbp_e = Bil.var rbp in
    let rsp_e = Bil.var rsp in
    let rsi_e = Bil.var rsi in
    let rdi_e = Bil.var rdi in
    let rax_e = Bil.var rax in
    let rdx_e = Bil.var rdx in
    let ah_e = ah_e mode in
    let disfailwith = disfailwith mode in
    let unimplemented = unimplemented mode in
    let is_small_imm op t = match op with
      | Oimm imm -> Word.bitwidth imm < bitwidth_of_type t
      | _ -> false in
    let sign_extend_imm op t =
      let op_typ = match op with
        | Oimm imm -> Type.imm (Word.bitwidth imm)
        | _ -> disfailwith "imm operand expected" in
      Bil.(cast signed (bitwidth_of_type t) (op2e op_typ op)) in
    function
      | Nop -> []
      | Bswap(t, op) ->
        let e = match t with
          | Type.Imm 32 | Type.Imm 64 -> let (op', t') = op2e_keep_width t op in
            reverse_bytes op' t'
          | _ -> disfailwith "bswap: Expected 32 or 64 bit type"
        in
        [assn t op e]
      | Retn (op, far_ret) when pref = [] || pref = [repz]  || pref = [repnz]->
        let temp = tmp mt in
        let load_stmt = if far_ret
          then (* TODO Mess with segment selectors here *)
            unimplemented "long retn not supported"
          else Bil.Move (temp, load_s mode seg_ss (size_of_typ mt) rsp_e)
        in
        let rsp_stmts =
          Bil.Move (rsp, Bil.(rsp_e + (Int (mi (bytes_of_width mt)))))::
          (match op with
           | None -> []
           | Some(t, src) ->
             [Bil.Move (rsp, Bil.(rsp_e + (op2e t src)))]
          ) in
        load_stmt::
        rsp_stmts@
        [Bil.Jmp (Bil.Var temp)]
      | Mov(t, dst, src, condition) ->
        let c_src = (match condition with
            | None -> op2e t src
            | Some c -> Bil.Ite (c, op2e t src, op2e t dst))
        in
        (* Find base by looking at LDT or GDT *)
        let base_e e =
          (* 0 = GDT, 1 = LDT *)
          let ti = Bil.Extract (3, 3, e) in
          let base = Bil.Ite (ti, Bil.var ldt, Bil.var gdt) in
          (* Extract index into table *)
          let entry_size, entry_shift = match mode with
            | X86 -> reg64_t, 6  (* "1<<6 = 64" *)
            | X8664 -> reg128_t, 7 (* "1<<7 = 128" *)
          in
          let index = Bil.(Cast (UNSIGNED, !!mt, (Extract (15, 4, e)) lsl (Int (mi entry_shift)))) in
          (* Load the table entry *)
          let table_entry = load_s mode None (size_of_typ entry_size) (Bil.(base + index)) in
          (* Extract the base *)
          concat_explist
            ((match mode with
                | X86 -> []
                | X8664 -> (Bil.Extract (95, 64, table_entry)) :: [])
             @  (Bil.Extract (63, 56, table_entry))
                :: (Bil.Extract (39, 32, table_entry))
                :: (Bil.Extract (31, 16, table_entry))
                :: [])
        in
        let bs =
          let dst_e = op2e t dst in
          if dst = o_fs && !compute_segment_bases then [Bil.Move (fs_base, base_e dst_e)]
          else if dst = o_gs && !compute_segment_bases then [Bil.Move (gs_base, base_e dst_e)]
          else []
        in
        assn t dst c_src :: bs
      | Movs(Type.Imm _bits as t) ->
        let stmts =
          store_s mode seg_es t rdi_e (load_s mode seg_es (size_of_typ t) rsi_e)
          :: string_incr mode t rsi
          :: string_incr mode t rdi
          :: []
        in
        if pref = [] then
          stmts
        else if ints_mem pref repz || ints_mem pref repnz then
          (* movs has only rep instruction others just considered to be rep *)
          rep_wrap ~mode ~addr ~next stmts
        else
          unimplemented "unsupported prefix for movs"
      | Movzx(t, dst, ts, src) ->
        [assn t dst Bil.(Cast (UNSIGNED, !!t, op2e ts src))]
      | Movsx(t, dst, ts, src) ->
        [assn t dst Bil.(Cast (SIGNED, !!t, op2e ts src))]
      | Movdq(ts, s, td, d, align) ->
        let (s, al) = match s with
          | Ovec _ | Oreg _-> op2e ts s, []
          | Oaddr a -> op2e ts s, [a]
          | Oimm _ | Oseg _ -> disfailwith "invalid source operand for movdq"
        in
        let (d, al) = match d with
          (* Behavior is to clear the xmm bits *)
          | Ovec _ -> assn td d Bil.(Cast (UNSIGNED, !!td, s)), al
          | Oreg _ -> assn td d s, al
          | Oaddr a -> assn td d s, a::al
          | Oimm _ | Oseg _ -> disfailwith "invalid dest operand for movdq"
        in
        (* sources tell me that movdqa raises a general protection exception
         * if its operands aren't aligned on a 16-byte boundary *)
        let im i = Bil.Int (int_of_mode mode i) in
        let al =
          if align then
            List.map ~f:(fun a -> Bil.If (Bil.((a land im 15) = im 0), [], [Cpu_exceptions.general_protection])) al
          else []
        in
        d::al
      | Movoffset((tdst, dst), offsets) ->
        (* If a vex prefix is present, then extra space is filled with 0.
           Otherwise, the bits are preserved. *)
        let padding hi lo =
          if hi < lo then []
          else if has_vex then [int_exp 0 (hi - lo + 1)]
          else [Bil.Extract (hi, lo, op2e tdst dst)]
        in
        let offsets = List.sort ~cmp:(fun {offdstoffset=o1; _} {offdstoffset=o2; _} -> Int.compare o1 o2) offsets in
        let add_exp (elist,nextbit) {offlen; offtyp; offop; offsrcoffset; offdstoffset} =
          Bil.Extract ((!!offlen + offsrcoffset - 1), offsrcoffset, (op2e offtyp offop))
          :: padding (offdstoffset - 1) nextbit
          @ elist, offdstoffset + !!offlen
        in
        let elist, nextbit = List.fold_left ~f:add_exp ~init:([],0) offsets in
        let elist = padding (!!tdst - 1) nextbit @ elist in
        [assn tdst dst (concat_explist elist)]
      | Punpck(t, et, o, d, s, vs) ->
        let nelem = match t, et with
          | Type.Imm n, Type.Imm n' -> n / n'
          | _ -> disfailwith "invalid"
        in
        assert (nelem mod 2 = 0);
        let nelem_per_src = nelem / 2 in
        let halft = Type.Imm (!!t / 2) in
        let castf = match o with
          | High -> fun e -> Bil.(Cast (HIGH, !!halft, e))
          | Low -> fun e -> Bil.(Cast (LOW, !!halft, e))
        in
        let se, de = castf (op2e t s), castf (op2e t d) in
        let st, dt = tmp halft, tmp halft in
        let et' = !!et in
        let mape i =
          [extract_element et' (Bil.Var st) i; extract_element et' (Bil.Var dt) i]
        in
        let e = concat_explist (List.concat (List.map ~f:mape (List.range ~stride:(-1) ~stop:`inclusive (nelem_per_src-1) 0))) in
        let dest = match vs with
          | None -> d
          | Some vdst -> vdst
        in
        [Bil.Move (st, se);
         Bil.Move (dt, de);
         assn t dest e]
      | Ppackedbinop(t, et, fbop, _, d, s, vs) ->
        let t_width = bitwidth_of_type t  in
        let e_width = bitwidth_of_type et in
        let nops = t_width / e_width in
        let byte = 8 in
        let tmp_dst = tmp t in
        let vdst = Option.value ~default:d vs in
        let iv  = tmp (Type.imm byte) in
        let elt = tmp et in
        let zero = Word.zero t_width in
        let bits = Word.of_int ~width:byte e_width in

        let foreach_size f = List.concat @@ List.init nops ~f in
        let getelement o i =
          (* assumption: immediate operands are repeated for all vector
             elements *)
          match o with
          | Oimm _ -> op2e et o
          | _ -> extract_element !!et (op2e t o) i in

        List.concat Bil.[
            [tmp_dst := int zero];
	    foreach_size (fun i -> [
                  elt := fbop (getelement d i) (getelement s i);
                  iv := int @@ Word.of_int ~width:byte i;
                  tmp_dst :=
                    var tmp_dst lor
                    ((cast unsigned t_width (var elt)) lsl (var iv * int bits))
                ]
              );
            [assn t vdst (var tmp_dst)]
          ]
      | Pbinop(t, fbop, _s, o1, o2, vop) ->
        (match vop with
         | None -> [assn t o1 (fbop (op2e t o1) (op2e t o2))]
         | Some vop -> [assn t o1 (fbop (op2e t vop) (op2e t o2))])
      | Pcmp (t,elet,bop,_,dst,src,vsrc) ->
        let elebits = bitwidth_of_type elet in
        let t_width = bitwidth_of_type t in
        let ncmps = t_width / elebits in
        let src = match src with
          | Ovec _ -> op2e t src
          | Oaddr a -> load (size_of_typ t) a
          | Oreg _ | Oimm _ | Oseg _ -> disfailwith "invalid" in
        let vsrc = Option.value ~default:dst vsrc in
        let byte = 8 in
        let tmp_dst = tmp t in
        let iv  = tmp (Type.imm byte) in
        let elt = tmp elet in
        let elt_1 = tmp elet in
        let elt_2 = tmp elet in
        let _one = Word.of_int ~width:elebits (-1) in
        let zero = Word.zero elebits in
        let bits = Word.of_int ~width:byte elebits in
        let zero_long = Word.zero t_width in
        let foreach_size f =
          let sizes = List.init ncmps ~f:(fun i ->
              let left_bit = i * elebits in
              let right_bit = (i + 1) * elebits - 1 in
              let i = Word.of_int ~width:byte i in
              i, left_bit, right_bit) in
          List.concat @@ List.map sizes ~f in

        List.concat Bil.[
          [tmp_dst := int zero_long];
	  foreach_size (fun (i, left_bit, right_bit) -> [
                elt_1 := extract right_bit left_bit src;
                elt_2 := extract right_bit left_bit (op2e t vsrc);
                iv := int i;
                if_ (binop bop (var elt_1) (var elt_2)) [
                  elt := int _one;
                ] [
                  elt := int zero;
                ];
                tmp_dst :=
                  var tmp_dst lor
                    ((cast unsigned t_width (var elt)) lsl (var iv * int bits))
              ]);
          [assn t dst (var tmp_dst)] ]
      | Pmov (t, dstet, srcet, dst, src, ext, _) ->
        let nelem = match t, dstet with
          | Type.Imm n, Type.Imm n' -> n / n'
          | _ -> disfailwith "invalid"
        in
        let getelt op i = extract_element !!srcet (op2e t op) i in
        let extcast =
          let open Exp in
          match ext with
          | UNSIGNED | SIGNED -> fun e -> Bil.Cast (ext, !!dstet, e)
          | _ -> disfailwith "invalid"
        in
        let extend i = extcast (getelt src i) in
        let e = concat_explist (List.map ~f:extend (List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0)) in
        [assn t dst e]
      | Pmovmskb (t,dst,src) ->
        let nbytes = bytes_of_width t in
        let src = match src with
          | Ovec _ -> op2e t src
          | _ -> disfailwith "invalid operand"
        in
        let get_bit i = Bil.Extract(i*8-1, i*8-1, src) in
        let byte_indices = List.init ~f:(fun i -> i + 1) nbytes in (* list 1-nbytes *)
        let all_bits = List.rev (List.map ~f:get_bit byte_indices) in
        (* could also be done with shifts *)
        let padt = Type.Imm(32 - nbytes) in
        let or_together_bits = List.fold_left ~f:(fun acc i -> Bil.Concat(acc,i)) ~init:(int_exp 0 (!!padt)) all_bits in
        [assn reg32_t dst or_together_bits]
      | Palignr (t,dst,src,vsrc,imm) ->
        let (dst_e, t_concat) = op2e_keep_width t dst in
        let (src_e, t_concat') = op2e_keep_width t src in
        (* previously, this code called Typecheck.infer_ast.
         * We're now just preserving the widths, so here we assert
         * that our 2 "preserved widths" are the same. *)
        assert (!!t_concat = !!t_concat');
        let imm = op2e t imm in
        let conct = Bil.(dst_e ^ src_e) in
        let shift = Bil.(conct lsr (Cast (UNSIGNED, !!t_concat, imm lsl (int_exp 3 !!t)))) in
        let high, low = match t with
          | Type.Imm 256 -> 255, 0
          | Type.Imm 128 -> 127, 0
          | Type.Imm 64 -> 63, 0
          | _ -> disfailwith "impossible: used non 64/128/256-bit operand in palignr"
        in
        let result = Bil.Extract (high, low, shift) in
        let im i = Bil.Int (int_of_mode mode i) in
        let addresses = List.fold
            ~f:(fun acc -> function Oaddr a -> a::acc | _ -> acc) ~init:[] [src;dst] in
        (* Palignr seems to cause a CPU general protection exception if this fails.
         * previously this code used the ast.ml Assert statement, which is gone,
         * so it's been replaced with Bil's CpuExn *)
        List.map ~f:(fun addr -> Bil.If (Bil.((addr land im 15) = im 0),
                                         [], [Cpu_exceptions.general_protection])) addresses
        @ (match vsrc with
            | None -> [assn t dst result]
            | Some vdst -> [assn t vdst result])
      | Pcmpstr(t,xmm1,xmm2m128,_imm,imm8cb,pcmpinfo) ->
        let open Pcmpstr in
        let concat elt_width exps = match exps with
          | [] -> disfailwith "trying concat on empty list"
          | exps ->
            let f (acc, n, y) e =
              let x = tmp (Type.imm (n + elt_width)) in
              match y with
              | None -> Bil.(x := e) :: acc, n + 1, Some x
              | Some y ->
                Bil.(move x (var y ^ e)) :: acc, n + elt_width, Some x in
            let bil,_, var = List.fold ~init:([],0, None) ~f exps in
            Option.value_exn var, List.rev bil in

        (* All bytes and bits are numbered with zero being the least
           significant. This includes strings! *)
        (* NOTE: Strings are backwards, at least when they are in
           registers.  This doesn't seem to be documented in the Intel
           manual.  This means that the NULL byte comes before the
           string. *)
        let xmm1_e = op2e t xmm1 in
        let xmm2m128_e = op2e t xmm2m128 in
        let regm = type_of_mode mode in

        let nelem, _nbits, elemt = match imm8cb.ssize with
          | Bytes -> 16, 8, Type.imm 8
          | Words -> 8, 16, Type.imm 16 in

        (* Get element index in e *)
        let get_elem = extract_element !!elemt in
        let get_xmm1 = get_elem xmm1_e in
        let get_xmm2 = get_elem xmm2m128_e in

        let nelem_range =
          List.range ~stride:(-1) ~stop:`inclusive (nelem-1) 0 in

        (* Build expressions that assigns the correct values to the
           is_valid variables using implicit (NULL-based) string length. *)
        let implicit_check is_valid_xmm_i get_xmm_i =
          let f acc i =
            let previous_valid =
              if i = 0 then exp_true
              else Bil.var (is_valid_xmm_i (i - 1)) in
            let current_valid =
              Bil.(get_xmm_i i <> (int_exp 0 !!elemt)) in
            let x = is_valid_xmm_i i in
            Bil.(x := previous_valid land current_valid) :: acc in
          List.fold ~f:f ~init:[] nelem_range in

        (* Build expressions that assigns the correct values to the
           is_valid variables using explicit string length. *)
        let explicit_check is_valid_xmm_i sizee =
          (* Max size is nelem *)
          let nelem_e = Word.of_int ~width:(bitwidth_of_type regm) nelem in
          let sizev = tmp ~name:"sz" regm in
          let init = Bil.[
              if_ (int nelem_e < sizee) [
                sizev := int nelem_e;
              ] [
                sizev := sizee;
              ]
            ] in
          let f acc i =
            (* Current element is valid *)
            let current_valid = Bil.(int_exp i !!regm < var sizev) in
            let x = is_valid_xmm_i i in
            Bil.(x := current_valid) :: acc in
          List.fold_left ~f ~init nelem_range in

        (* Get var name indicating whether index in xmm num is a valid
           byte (before NULL byte). *)
        (* XXX more hashtable code *)
        let is_valid =
          let vh = Hashtbl.Poly.create () ~size:(2*nelem) in
          (fun xmmnum index -> match Hashtbl.find vh (xmmnum,index) with
             | Some v -> v
             | None ->
               let name = sprintf "is_valid_xmm%d_ele%d" xmmnum index in
               let v = tmp ~name bool_t in
               Hashtbl.add_exn vh ~key:(xmmnum,index) ~data:v;
               v) in

        let is_valid_xmm1 index = is_valid 1 index in
        let is_valid_xmm2 index = is_valid 2 index in
        let is_valid_xmm1_e index = Bil.Var(is_valid_xmm1 index) in
        let is_valid_xmm2_e index = Bil.Var(is_valid_xmm2 index) in

        let xmm1_checks, xmm2_checks =
          match pcmpinfo.len with
          | Implicit ->
            implicit_check is_valid_xmm1 get_xmm1,
            implicit_check is_valid_xmm2 get_xmm2
          | Explicit ->
            explicit_check is_valid_xmm1 rax_e,
            explicit_check is_valid_xmm2 rdx_e in

        let get_bit index =
          match imm8cb.agg with
          | EqualAny ->
            (* Is xmm2[index] at xmm1[j]? *)
            let check_char acc j =
              let is_eql = Bil.(get_xmm2 index = get_xmm1 j) in
              let is_valid = is_valid_xmm1_e j in
              Bil.(Ite ((is_eql land is_valid), exp_true, acc))
            in
            Bil.BinOp (AND, is_valid_xmm2_e index,
                       (* Is xmm2[index] included in xmm1[j] for any j? *)
                       (List.fold ~f:check_char ~init:exp_false nelem_range))
          | Ranges ->
            (* Is there an even j such that xmm1[j] <= xmm2[index] <=
               xmm1[j+1]? *)
            let check_char acc j =
              (* XXX: Should this be AND? *)
              let ind0 = 2 * j in
              let ind1 = ind0 + 1 in
              let rangevalid = Bil.(is_valid_xmm1_e ind0 land is_valid_xmm1_e ind1) in
              let (<=) = match imm8cb.ssign with
                | Unsigned -> Bil.(<=)
                | Signed -> Bil.(<=$) in
              let (land) = Bil.(land) in
              let inrange =
                (get_xmm1 ind0 <= get_xmm2 index) land (get_xmm2 index <= get_xmm1 ind1) in
              Bil.(Ite (UnOp (NOT, rangevalid), exp_false, Ite (inrange, exp_true, acc)))
            in
            Bil.(is_valid_xmm2_e index
                 (* Is xmm2[index] in the jth range pair? *)
                 land List.fold_left ~f:check_char ~init:exp_false (List.range ~stride:(-1) ~stop:`inclusive Pervasives.(nelem/2-1) 0))
          | EqualEach ->
            (* Does xmm1[index] = xmm2[index]? *)
            let xmm1_invalid = Bil.(UnOp (NOT, (is_valid_xmm1_e index))) in
            let xmm2_invalid = Bil.(UnOp (NOT, (is_valid_xmm2_e index))) in
            let bothinvalid = Bil.(xmm1_invalid land xmm2_invalid) in
            let eitherinvalid = Bil.(xmm1_invalid lor xmm2_invalid) in
            let eq = Bil.(get_xmm1 index = get_xmm2 index) in
            (* both invalid -> true
               one invalid -> false
               both valid -> check same byte *)
            Bil.Ite (bothinvalid, exp_true,
                     Bil.Ite (eitherinvalid, exp_false,
                              Bil.Ite (eq, exp_true, exp_false)))
          | EqualOrdered ->
            (* Does the substring xmm1 occur at xmm2[index]? *)
            let check_char acc j =
              let neq = Bil.(get_xmm1 j <> get_xmm2 Pervasives.(index+j)) in
              let substrended = Bil.(UnOp (NOT, (is_valid_xmm1_e j))) in
              let bigstrended = Bil.UnOp (NOT, (is_valid_xmm2_e (index+j))) in
              (* substrended => true
                 bigstrended => false
                 byte diff => false
                 byte same => keep going  *)
              Bil.Ite (substrended, exp_true,
                       Bil.Ite (bigstrended, exp_false,
                                Bil.Ite (neq, exp_false, acc)))
            in
            (* Is xmm1[j] equal to xmm2[index+j]? *)
            List.fold_left ~f:check_char ~init:exp_true (List.range
                                                           ~stride:(-1) ~stop:`inclusive (nelem-index-1) 0)
        in
        let bits = List.map ~f:get_bit nelem_range in
        let res, res_bil = concat 1 bits in
        let int_res_1 = tmp ~name:"IntRes1" reg16_t in
        let int_res_2 = tmp ~name:"IntRes2" reg16_t in

        let contains_null e =
          List.fold_left ~f:(fun acc i ->
              Bil.Ite (Bil.(get_elem e i = int_exp 0 !!elemt), exp_true, acc)) ~init:exp_false (List.init ~f:(fun x -> x) nelem)
        in

        (* For pcmpistri/pcmpestri *)
        let sb exp =
          List.fold_left ~f:(fun acc i ->
              Bil.Ite (Bil.(exp_true = Extract (i, i, exp)),
                       (int_exp i !!regm),
                       acc))
            ~init:(int_exp nelem !!regm)
            (match imm8cb.outselectsig with
             | LSB -> List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive nelem 0
             | MSB -> List.init ~f:(fun x -> x) nelem)
        in

        (* For pcmpistrm/pcmpestrm *)
        let mask e =
          match imm8cb.outselectmask with
          | Bitmask -> Bil.(cast unsigned 128 e)
          | Bytemask ->
            let get_element i =
              Bil.(cast unsigned !!elemt (extract i i e)) in
            let range = List.range ~stride:(-1)
                ~start:`exclusive ~stop:`inclusive nelem 0 in
            concat_explist (List.map ~f:get_element range) in

        let res_of_cb = match imm8cb with
          | {negintres1=false; _} -> Bil.(int_res_2 := var int_res_1)
          | {negintres1=true; maskintres1=false; _} -> (* int_res_1 is bitwise-notted *)
            Bil.(int_res_2 := unop not (var int_res_1))
          | {negintres1=true; maskintres1=true; _} ->
            (* only the valid elements in xmm2 are bitwise-notted *)
            (* XXX: Right now we duplicate the valid element computations
               when negating the valid elements.  They are also used by the
               aggregation functions.  A better way to implement this might
               be to write the valid element information out as a temporary
               bitvector.  The aggregation functions and this code would
               then extract the relevant bit to see if an element is
               valid. *)
            let validvector =
              let range = List.range ~stride:(-1)
                  ~start:`exclusive ~stop:`inclusive nelem 0 in
              let bits = List.map ~f:is_valid_xmm2_e range  in
              Bil.(cast unsigned 16 (concat_explist bits)) in
            Bil.(int_res_2 := validvector lxor var int_res_1) in

        let res_of_pcmpinfo = match pcmpinfo.out with
          | Index -> Bil.(rcx := sb (var int_res_2))
          (* FIXME: ymms should be used instead of xmms here *)
          | Mask -> Bil.(ymms.(0) := mask (var int_res_2)) in

        List.concat [
          xmm1_checks;
          xmm2_checks;
          res_bil;
          Bil.[
            int_res_1 := cast unsigned 16 (var res);
            res_of_cb;
            res_of_pcmpinfo;
            cf := var int_res_2 <> int_exp 0 16;
            zf := contains_null xmm2m128_e;
            sf := contains_null xmm1_e;
            oF := extract 0 0 (var int_res_2);
            af := int_exp 0 1;
            pf := int_exp 0 1;
          ]
        ]

      | Pshufd (t, dst, src, vsrc, imm) ->
        let src_e = op2e t src in
        let imm_e = op2e t imm in
        (* XXX: This would be more straight-forward if implemented using
           map, instead of fold *)
        let get_dword ndword =
          let high_b = 2 * (ndword mod 4) + 1 in
          let low_b = 2 * (ndword mod 4) in
          let index = Bil.(Cast (UNSIGNED, !!t, Extract (high_b, low_b, imm_e))) in
          let t' = !!t in
          (* Use the same pattern for the top half of a ymm register *)
          (* had to stop using extract_element_symbolic, since that calls
           * Typecheck.infer_ast. I believe this captures the same
           * "width logic", but this is a good place to check if things start
           * going wrong later. *)
          let (index, index_width) = if t' = 256 && ndword > 3 then
              (Bil.(index + (Int (BV.of_int ~width:(!!t) 4))), 256)
            else (index, t') in
          extract_element_symbolic_with_width (Type.imm 32) src_e index index_width
        in
        let topdword = match t with Type.Imm 128 -> 3 | _ -> 7 in
        let dwords = concat_explist (List.map ~f:get_dword (List.range ~stride:(-1) ~stop:`inclusive topdword 0)) in
        (match vsrc with
         | None -> [assn t dst dwords]
         | Some vdst -> [assn t vdst dwords])
      | Pshufb (exp_type, dst_op, src_op, vsrc) ->
        let op_size = bitwidth_of_type exp_type in
        let index_bits = match op_size with
          | 64 -> 3
          | 128 | 256 -> 4
          | _ -> disfailwith "invalid size for pshufb" in
	let foreach_byte f = List.concat @@ List.init (op_size / 8) ~f in

        let src = op2e exp_type src_op in
        let dst = op2e exp_type dst_op in
        let dst_op = Option.value ~default:dst_op vsrc in
        let zero = Bil.int (Word.zero op_size) in
        let msb_one = Bil.int (Word.of_int ~width:8 0x80) in

	let byte_t = Type.imm 8 in
	let byte = int_exp 8 8 in
        let iv = tmp ~name:"i" byte_t in
        let mask_byte_i = tmp byte_t in
        let tmp_byte = tmp exp_type in
        let tmp_dst = tmp exp_type in
        let ind = tmp byte_t in

	let check_mem_alignment = match src_op with
          | Oaddr addr when (op_size = 128) ->
            let word_size = width_of_mode mode in
            let zero = Bil.int (Word.zero word_size) in
            let oxf = Bil.int (Word.of_int ~width:word_size 0xf) in
            [ Bil.(if_ (oxf land addr <> zero) [cpuexn 13] []) ]
          | _ -> [] in

        List.concat [
	  check_mem_alignment;
	  [Bil.move tmp_dst zero];
	  foreach_byte (fun i ->
              Bil.[
                iv := int (Word.of_int ~width:8 i);
                mask_byte_i := extract 7 0 (src lsr (var iv * byte));
                if_ (msb_one land var mask_byte_i = msb_one) [
                  tmp_byte := zero;
		] (* else *) [
		  ind := cast unsigned 8 (extract index_bits 0 (var mask_byte_i));
                  tmp_byte :=
                    cast unsigned op_size (extract 7 0 (dst lsr (var ind * byte)))
                ];
                tmp_dst := Bil.(var tmp_dst lor (var tmp_byte lsl (var iv * byte)));
              ]);
          [assn exp_type dst_op (Bil.var tmp_dst)]
        ]

      | Lea(t, r, a) when pref = [] ->
        (* See Table 3-64 *)
        (* previously, it checked whether addrbits > opbits before the cast_low.
         * The conclusion we came to was that
           - if they were equal, the cast is basically a nop
           - if it was the other way round, you want to extend it anyway.
         * (I may be remembering things wrongly) *)
        [assn t r Bil.(Cast (LOW, !!t, a))]
      | Call(o1, ra) when pref = [] ->
        (* If o1 is an immediate, we should syntactically have Jump(imm)
           so that the CFG algorithm knows where the jump goes.  Otherwise
           it will point to BB_Indirect.

           Otherwise, we should evaluate the operand before decrementing esp.
           (This really only matters when esp is the base register of a memory
           lookup. *)
        let target = op2e mt o1 in
        (match o1 with
         | Oimm _ ->
           [Bil.Move (rsp, Bil.(rsp_e - (Int (mi (bytes_of_width mt)))));
            store_s mode None mt rsp_e (Bil.Int ra);
            Bil.Jmp target]
         | _ ->
           let t = tmp mt in
           [Bil.Move (t, target);
            Bil.Move (rsp, Bil.(rsp_e - (Int (mi (bytes_of_width mt)))));
            store_s mode None mt rsp_e (Bil.Int ra);
            Bil.Jmp (Bil.Var t)])
      | Jump(o) ->
        [Bil.Jmp (jump_target mode ss has_rex o)]
      | Jcc(o, c) ->
        [Bil.If (c, [Bil.Jmp (jump_target mode ss has_rex o)], [])]
      | Setcc(t, o1, c) ->
        [assn t o1 Bil.(Cast (UNSIGNED, !!t, c))]
      | Shift(st, s, dst, shift) ->
        let old = tmp ~name:"tmp" s in
        let s' = !!s in
        let size = int_exp s' s' in
        let s_f = Bil.(match st with
            | LSHIFT -> (lsl)
            | RSHIFT -> (lsr)
            | ARSHIFT -> (asr)
            | _ -> disfailwith "invalid shift type") in
        let dste = op2e s dst in
        let count_mask = Bil.(size - int_exp 1 s') in
        let count = Bil.(op2e s shift land count_mask) in
        let new_of = match st with
          | LSHIFT -> Bil.((Cast (HIGH, !!bool_t, dste)) lxor cf_e)
          | RSHIFT -> Bil.(Cast (HIGH, !!bool_t, var old))
          | ARSHIFT -> exp_false
          | _ -> disfailwith "impossible"
        in
        let new_cf =
          (* undefined for SHL and SHR instructions where the count is greater than
             or equal to the size (in bits) of the destination operand *)
          match st with
          | LSHIFT -> Bil.(Cast (LOW, !!bool_t, var old lsr (size - count)))
          | RSHIFT | ARSHIFT ->
            Bil.(Cast (HIGH, !!bool_t, var old lsl (size - count)))
          | _ -> failwith "impossible"
        in
        Bil.[
          old := dste;
          assn s dst (s_f dste count);
          if_ (count <> int_exp 0 s') [
            cf := new_cf;
            sf := compute_sf dste;
            zf := compute_zf s' dste;
            pf := compute_pf s dste;
            af := unknown "after-shift" bool_t;
            if_ (count = int_exp 1 s') [
                oF := new_of;
              ] [
                oF := unknown "after-shift" bool_t;
              ]
            ] [];
        ]
      | Shiftd(st, s, dst, fill, count) ->
        let was = tmp ~name:"tmp" s in
        let e_dst = op2e s dst in
        let e_fill = op2e s fill in
        let s' = !!s in
        (* Check for 64-bit operand *)
        let size = int_exp s' s' in
        let count_mask = Bil.(size - int_exp 1 s') in
        let e_count = Bil.(op2e s count land count_mask) in
        let new_cf =  match st with
          | LSHIFT -> Bil.(Cast (LOW, !!bool_t, var was lsr (size - e_count)))
          | RSHIFT -> Bil.(Cast (HIGH, !!bool_t, var was lsl (size - e_count)))
          | _ -> disfailwith "impossible" in
        let new_of = Bil.(Cast (HIGH, !!bool_t, (var was lxor e_dst))) in
        let unk_of =
          Bil.Unknown ("OF undefined after shiftd of more then 1 bit", bool_t) in
        let ret1 = match st with
          | LSHIFT -> Bil.(e_fill lsr (size - e_count))
          | RSHIFT -> Bil.(e_fill lsl (size - e_count))
          | _ -> disfailwith "impossible" in
        let ret2 = match st with
          | LSHIFT -> Bil.(e_dst lsl e_count)
          | RSHIFT -> Bil.(e_dst lsr e_count)
          | _ -> disfailwith "impossible" in
        let result = Bil.(ret1 lor ret2) in
        (* SWXXX If shift is greater than the operand size, dst and
           flags are undefined *)
        Bil.[
          was := e_dst;
          assn s dst result;
          if_ (e_count <> int_exp 0 s') [
            cf := new_cf;
            sf := compute_sf e_dst;
            zf := compute_zf s' e_dst;
            pf := compute_pf s e_dst;
            (* For a 1-bit shift, the OF flag is set if a sign change occurred;
               otherwise, it is cleared. For shifts greater than 1 bit, the OF flag
               is undefined. *)
            if_ (e_count = int_exp 1 s') [
                oF := new_of;
              ] [
                oF := unk_of;
              ]
          ] []
        ]
      | Rotate(shift_type, exp_type, dst_op, shift_op, use_cf) ->
        if use_cf then unimplemented "rotate use_vf";
        let word_size = bitwidth_of_type exp_type in
        let mask_size = word_size - 1 in
        let count_var = tmp exp_type in
        let count = Bil.var count_var in
        let zero = int_exp 0 word_size in
        let one  = int_exp 1 word_size in
        let dst  = op2e exp_type dst_op in
        let shift_mask = int_exp mask_size word_size in
        let size = int_exp word_size word_size in
        let shift = op2e exp_type shift_op in

        if shift_type = LSHIFT then
          Bil.[
            count_var := (shift land shift_mask) mod size;
            assn exp_type dst_op ((dst lsl count) lor (dst lsr (size - count)));
            if_ (count = zero) [
              cf := cast low 1 dst;
            ] (* else *) [
              if_ (count = one) [
                oF := cf_e lxor (cast high 1 dst);
              ]  (* else  *) [
                oF := unknown "OF undefined after rotate of more then 1 bit" bool_t;
              ]
            ]
          ]
        else
          Bil.[
            count_var := (shift land shift_mask) mod size;
            assn exp_type dst_op ((dst lsr count) lor (dst lsl (size - count)));
            if_ (count = zero) [
              cf := cast high 1 dst;
            ] (* else *) [
              if_ (count = one) [
                oF := cast high 1 dst lxor ((cast high 1 dst) lsl int_exp 1 word_size);
              ]  (* else  *) [
                oF := unknown "OF undefined after rotate of more then 1 bit" bool_t;
              ]
            ]
          ]
      | Bt(t, bitoffset, bitbase) ->
        let t' = !!t in
        let offset = op2e t bitoffset in
        let value, shift = match bitbase with
          | Oreg _ ->
            let reg = op2e t bitbase in
            let shift = Bil.(offset land int_exp Pervasives.(t' - 1) t') in
            reg, shift
          | Oaddr a ->
            let offset = Bil.(cast unsigned (width_of_mode mode) offset) in
            let byte = load (size_of_typ reg8_t) Bil.(a + offset lsr int_exp 3 t') in
            let shift = Bil.(Cast (LOW, !!reg8_t, offset) land int_exp 7 8) in
            byte, shift
          | Ovec _ | Oseg _ | Oimm _ -> disfailwith "Invalid bt operand"
        in
        [
          Bil.Move (cf, Bil.(Cast (LOW, !!bool_t, value lsr shift)));
          Bil.Move (oF, Bil.Unknown ("OF undefined after bt", bool_t));
          Bil.Move (sf, Bil.Unknown ("SF undefined after bt", bool_t));
          Bil.Move (af, Bil.Unknown ("AF undefined after bt", bool_t));
          Bil.Move (pf, Bil.Unknown ("PF undefined after bt", bool_t))
        ]
      | Bs(t, dst, src, dir) ->
        let t' = !!t in
        let source_is_zero = tmp bool_t in
        let source_is_zero_v = Bil.Var source_is_zero in
        let src_e = op2e t src in
        let bits = !!t in
        let check_bit bitindex next_value =
          Bil.(Ite (Extract (bitindex,bitindex,src_e) = int_exp 1 1, int_exp bitindex t', next_value))
        in
        let bitlist = List.init ~f:(fun x -> x) bits in
        (* We are folding from right to left *)
        let bitlist = match dir with
          | Forward -> (* least significant first *) bitlist
          | Backward -> (* most significant *) List.rev bitlist
        in
        let first_one = List.fold_right ~f:check_bit bitlist
            ~init:(Bil.Unknown("bs: destination undefined when source is zero", t)) in
        [
          Bil.Move (source_is_zero, Bil.(src_e = int_exp 0 t'));
          assn t dst first_one;
          Bil.Move (zf, Bil.Ite (source_is_zero_v, int_exp 1 1, int_exp 0 1));
        ]
        @
        let undef r =
          Bil.Move (r, Bil.Unknown (Var.name r ^ " undefined after bsf", Var.typ r)) in
        List.map ~f:undef [cf; oF; sf; af; pf]
      | Hlt -> [] (* x86 Hlt is essentially a NOP *)
      | Rdtsc ->
        let undef reg = assn reg32_t reg (Bil.Unknown ("rdtsc", reg32_t)) in
        List.map ~f:undef [o_rax; o_rdx]
      | Cpuid ->
        let undef reg = assn reg32_t reg (Bil.Unknown ("cpuid", reg32_t)) in
        List.map ~f:undef [o_rax; o_rbx; o_rcx; o_rdx]
      | Xgetbv ->
        let undef reg = assn reg32_t reg (Bil.Unknown ("xgetbv", reg32_t)) in
        List.map ~f:undef [o_rax; o_rdx]
      | Stmxcsr (dst) ->
        let dst = match dst with
          | Oaddr addr -> addr
          | _ -> disfailwith "stmxcsr argument cannot be non-memory"
        in
        [store reg32_t dst (Bil.Var mxcsr);(*(Unknown ("stmxcsr", reg32_t));*) ]
      | Ldmxcsr (src) ->
        let src = match src with
          | Oaddr addr -> addr
          | _ -> disfailwith "ldmxcsr argument cannot be non-memory"
        in
        [ Bil.Move (mxcsr, load (size_of_typ reg32_t) src); ]
      | Fnstcw (dst) ->
        let dst = match dst with
          | Oaddr addr -> addr
          | _ -> disfailwith "fnstcw argument cannot be non-memory"
        in
        [store reg16_t dst (Bil.Var fpu_ctrl); ]
      | Fldcw (src) ->
        let src = match src with
          | Oaddr addr -> addr
          | _ -> disfailwith "fldcw argument cannot be non-memory"
        in
        [ Bil.Move (fpu_ctrl, load (size_of_typ reg16_t) src); ]
      | Fld _src ->
        unimplemented "unsupported FPU register stack"
      | Fst (_dst,_pop) ->
        unimplemented "unsupported FPU flags"
      | Cmps(Type.Imm _bits as t) ->
        let t' = !!t in
        let src1   = tmp t in
        let src2   = tmp t in
        let tmpres = tmp t in
        let stmts =
          Bil.Move (src1, op2e t (Oaddr rsi_e))
          :: Bil.Move (src2, op2e_s mode seg_es has_rex t (Oaddr rdi_e))
          :: Bil.Move (tmpres, Bil.(Var src1 - Var src2))
          :: string_incr mode t rsi
          :: string_incr mode t rdi
          :: set_flags_sub t' (Bil.Var src1) (Bil.Var src2) (Bil.Var tmpres)
        in
        begin match pref with
          | [] -> stmts
          | [single] when single = repz || single = repnz ->
            rep_wrap ~mode ~check_zf:single ~addr ~next stmts
          | _ -> unimplemented "unsupported flags in cmps" end
      | Scas(Type.Imm _bits as t) ->
        let t' = !!t in
        let src1   = tmp t in
        let src2   = tmp t in
        let tmpres = tmp t in
        let stmts =
          let open Stmt in
          Move (src1, Bil.(Cast (LOW, !!t, Var rax)))
          :: Move (src2, op2e_s mode seg_es has_rex t (Oaddr rdi_e))
          :: Move (tmpres, Bil.(Var src1 - Var src2))
          :: string_incr mode t rdi
          :: set_flags_sub t' (Bil.Var src1) (Bil.Var src2) (Bil.Var tmpres)
        in
        begin match pref with
          | [] -> stmts
          | [single] when single = repz || single = repnz ->
            rep_wrap ~mode ~check_zf:single ~addr ~next stmts
          | _ -> unimplemented "unsupported flags in scas" end
      | Stos(Type.Imm _bits as t) ->
        let stmts = [store_s mode seg_es t rdi_e (op2e t o_rax);
                     string_incr mode t rdi]
        in
        begin match pref with
          | [] -> stmts
          | [single] when single = repz -> rep_wrap ~mode ~addr ~next stmts
          | _ -> unimplemented "unsupported prefix for stos" end
      | Push(t, o) ->
        let o = if is_small_imm o t then sign_extend_imm o t
          else op2e t o in
        let tmp = tmp t in (* only really needed when o involves esp *)
        Bil.Move (tmp, o)
        :: Bil.Move (rsp, Bil.(rsp_e - Int (mi (bytes_of_width t))))
        :: store_s mode seg_ss t rsp_e (Bil.var tmp) (* FIXME: can ss be overridden? *)
        :: []
      | Pop(t, o) ->
        (* From the manual:

           "The POP ESP instruction increments the stack pointer (ESP)
           before data at the old top of stack is written into the
           destination"

           So, effectively there is no incrementation.
        *)
        assn t o (load_s mode seg_ss (size_of_typ t) rsp_e)
        :: if o = o_rsp then []
        else [Bil.Move (rsp, Bil.(rsp_e + Int (mi (bytes_of_width t))))]
      | Pushf(t) ->
        (* Note that we currently treat these fields as unknowns, but the
           manual says: When copying the entire EFLAGS register to the
           stack, the VM and RF flags (bits 16 and 17) are not copied;
           instead, the values for these flags are cleared in the EFLAGS
           image stored on the stack. *)
        let flags_e = match t with
          | Type.Imm 16 -> flags_e
          | Type.Imm 32 -> eflags_e
          | Type.Imm 64 -> rflags_e
          | _ -> failwith "impossible"
        in
        Bil.Move (rsp, Bil.(rsp_e - Int (mi (bytes_of_width t))))
        :: store_s mode seg_ss t rsp_e flags_e
        :: []
      | Popf t ->
        let assnsf = match t with
          | Type.Imm 16 -> assns_flags_to_bap
          | Type.Imm 32 -> assns_eflags_to_bap
          | Type.Imm 64 -> assns_rflags_to_bap
          | _ -> failwith "impossible"
        in
        let tmp = tmp t in
        let extractlist =
          List.map
            ~f:(fun i ->
                Bil.(Extract (i, i, Var tmp)))
            (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive !!t 0)
        in
        Bil.Move (tmp, load_s mode seg_ss (size_of_typ t) rsp_e)
        :: Bil.Move (rsp, Bil.(rsp_e + Int (mi (bytes_of_width t))))
        :: List.concat (List.map2_exn ~f:(fun f e -> f e) assnsf
                          extractlist)
      | Popcnt(t, s, d) ->
        let width = !!t in
        let bits = op2e t s in
        let bitvector = Array.to_list (Array.init width ~f:(fun i -> Bil.(Ite (Extract (i, i, bits), int_exp 1 width, int_exp 0 width)))) in
        let count = List.reduce_exn ~f:Bil.(+) bitvector in
        set_zf width bits
        :: assn t d count
        :: List.map ~f:(fun r -> Bil.Move (r, int_exp 0 1)) [cf; oF; sf; af; pf]
      | Sahf ->
        let assnsf = assns_lflags_to_bap in
        let tah = tmp ~name:"AH" reg8_t in
        let extractlist =
          List.map
            ~f:(fun i ->
                Bil.(Extract (i, i, Var tah)))
            (List.range ~stride:(-1) ~stop:`inclusive 7 0)
        in
        Bil.Move (tah, ah_e)
        :: List.concat (List.map2_exn ~f:(fun f e -> f e) assnsf extractlist)
      | Lahf ->
        let o_ah = Oreg 4 in
        [assn reg8_t o_ah lflags_e]
      | Add(t, o1, o2) ->
        let tmp  = tmp t and tmp2 = tmp t in
        Bil.Move (tmp, op2e t o1)
        :: Bil.Move (tmp2, op2e t o2)
        :: assn t o1 Bil.(op2e t o1 + Var tmp2)
        :: let s1 = Bil.Var tmp in let s2 = Bil.Var tmp2 in let r = op2e t o1 in
        set_flags_add !!t s1 s2 r
      | Adc(t, o1, o2) ->
        let orig1 = tmp t in
        let orig2 = tmp t in
        let bits = !!t in
        let t' = Type.Imm (bits + 1) in
        let c e = Bil.(Cast (UNSIGNED, !!t', e)) in
        (* Literally compute the addition with an extra bit and see
           what the value is for CF *)
        let s1 = Bil.Var orig1 in let s2 = Bil.Var orig2 in let r = op2e t o1 in
        let bige = Bil.(c s1 + c s2 + c (Cast (UNSIGNED, !!t, cf_e))) in
        Bil.Move (orig1, op2e t o1)
        :: Bil.Move (orig2, op2e t o2)
        :: assn t o1 Bil.(s1 + s2 + Cast (UNSIGNED, !!t, cf_e))
        :: Bil.Move (cf, Bil.Extract (bits, bits, bige))
        :: set_aopszf_add !!t s1 s2 r
      | Inc(t, o) (* o = o + 1 *) ->
        let t' = !!t in
        let tmp = tmp t in
        Bil.Move (tmp, op2e t o)
        :: assn t o Bil.(op2e t o + int_exp 1 t')
        :: set_aopszf_add t' (Bil.Var tmp) (int_exp 1 t') (op2e t o)
      | Dec(t, o) (* o = o - 1 *) ->
        let t' = !!t in
        let tmp = tmp t in
        Bil.Move (tmp, op2e t o)
        :: assn t o Bil.(op2e t o - int_exp 1 t')
        :: set_aopszf_sub t' (Bil.Var tmp) (int_exp 1 t') (op2e t o) (* CF is maintained *)
      | Sub(t, o1, o2) (* o1 = o1 - o2 *) ->
        let oldo1 = tmp t in
        let oldo2 = tmp t in
        let op1 = op2e t o1 in
        let op2 =
          if is_small_imm o2 t then sign_extend_imm o2 t
          else op2e t o2 in
        Bil.([
            oldo1 := op1;
            oldo2 := op2;
            assn t o1 (op1 - op2);
          ] @ set_flags_sub (bitwidth_of_type t) (var oldo1) (var oldo2) op1)
      | Sbb(t, o1, o2) ->
        let tmp_s = tmp t in
        let tmp_d = tmp t in
        let orig_s = Bil.Var tmp_s in
        let orig_d = Bil.Var tmp_d in
        let sube = Bil.(orig_s + Cast (UNSIGNED, !!t, cf_e)) in
        let d = op2e t o1 in
        let s1 =
          if is_small_imm o2 t then sign_extend_imm o2 t
          else op2e t o2 in
        Bil.Move (tmp_s, s1)
        :: Bil.Move (tmp_d, d)
        :: assn t o1 Bil.(orig_d - sube)
        :: Bil.Move (oF, Bil.(Cast (HIGH, !!bool_t, (orig_s lxor orig_d) land (orig_d lxor d))))
        (* When src = 0xffffffff and cf=1, the processor sets CF=1.

           Note that we compute dest = dest - (0xffffffff + 1) = 0, so the
           subtraction does not overflow.

           So, I am guessing that CF is set if the subtraction overflows
           or the addition overflows.

           Maybe we should implement this by doing the actual computation,
           like we do for adc.
        *)
        (* sub overflow | add overflow *)
        :: Bil.Move (cf, Bil.((sube > orig_d) lor (sube < orig_s)))
        :: set_apszf !!t orig_s orig_d d
      | Cmp(t, o1, o2) ->
        let tmp = tmp t in
        Bil.Move (tmp, Bil.(op2e t o1 - op2e t o2))
        :: set_flags_sub !!t (op2e t o1) (op2e t o2) (Bil.Var tmp)
      | Cmpxchg(t, src, dst) ->
        let t' = !!t in
        let eax_e = op2e t o_rax in
        let dst_e = op2e t dst in
        let src_e = op2e t src in
        let tmp = tmp t in
        Bil.Move (tmp, Bil.(eax_e - dst_e))
        :: set_flags_sub t' eax_e dst_e (Bil.Var tmp)
        @ assn t dst (Bil.Ite (zf_e, src_e, dst_e))
          :: assn t o_rax (Bil.Ite (zf_e, eax_e, dst_e))
          :: []
      | Cmpxchg8b o -> (* only 32bit case *)
        let accumulator = Bil.Concat((op2e reg32_t o_rdx),(op2e reg32_t o_rax)) in
        let dst_e = op2e reg64_t o in
        let src_e = Bil.Concat((op2e reg32_t o_rcx),(op2e reg32_t o_rbx)) in
        let dst_low_e = Bil.Extract(63, 32, dst_e) in
        let dst_hi_e = Bil.Extract(31, 0, dst_e) in
        let eax_e = op2e reg32_t o_rax in
        let edx_e = op2e reg32_t o_rdx in
        let equal = tmp bool_t in
        let equal_v = Bil.Var equal in
        [
          Bil.Move (equal, Bil.(accumulator = dst_e));
          Bil.Move (zf, equal_v);
          assn reg64_t o (Bil.Ite (equal_v, src_e, dst_e));
          assn reg32_t o_rax (Bil.Ite (equal_v, eax_e, dst_low_e));
          assn reg32_t o_rdx (Bil.Ite (equal_v, edx_e, dst_hi_e))
        ]
      | Xadd(t, dst_op, src_op) ->
        let tmp_res = tmp t in
        let tmp_dst = tmp t in
        let tmp_src = tmp t in
        let dst = op2e t dst_op in
        let src = op2e t src_op in
        Bil.[
          tmp_src := src;
          tmp_dst := dst;
          tmp_res := src + dst;
          assn t src_op dst;
          assn t dst_op (var tmp_res);
        ] @ set_flags_add !!t (Bil.var tmp_dst) (Bil.var tmp_src) dst;
      | Xchg(t, src, dst) ->
        let tmp = tmp t in
        [ Bil.Move (tmp, op2e t src);
          assn t src (op2e t dst);
          assn t dst (Bil.Var tmp); ]
      | And(t, o1, o2) ->
        assn t o1 Bil.(op2e t o1 land op2e t o2)
        :: Bil.Move (oF, exp_false)
        :: Bil.Move (cf, exp_false)
        :: Bil.Move (af, Bil.Unknown ("AF is undefined after and", bool_t))
        :: set_pszf t (op2e t o1)
      | Or(t, o1, o2) ->
        let o2 =
          if is_small_imm o2 t then sign_extend_imm o2 t
          else op2e t o2 in
        assn t o1 Bil.(op2e t o1 lor o2)
        :: Bil.Move (oF, exp_false)
        :: Bil.Move (cf, exp_false)
        :: Bil.Move (af, Bil.Unknown ("AF is undefined after or", bool_t))
        :: set_pszf t (op2e t o1)
      | Xor(t, o1, o2) when o1 = o2 ->
        assn t o1 Bil.(Int (BV.of_int ~width:(!!t) 0))
        :: Bil.Move (af, Bil.Unknown ("AF is undefined after xor", bool_t))
        :: List.map ~f:(fun v -> Bil.Move (v, exp_true)) [zf; pf]
        @  List.map ~f:(fun v -> Bil.Move (v, exp_false)) [oF; cf; sf]
      | Xor(t, o1, o2) ->
        assn t o1 Bil.(op2e t o1 lxor op2e t o2)
        :: Bil.Move (oF, exp_false)
        :: Bil.Move (cf, exp_false)
        :: Bil.Move (af, Bil.Unknown ("AF is undefined after xor", bool_t))
        :: set_pszf t (op2e t o1)
      | Test(t, o1, o2) ->
        let o2 =
          if is_small_imm o2 t then sign_extend_imm o2 t
          else op2e t o2 in
        let tmp = tmp t in
        Bil.Move (tmp, Bil.(op2e t o1 land o2))
        :: Bil.Move (oF, exp_false)
        :: Bil.Move (cf, exp_false)
        :: Bil.Move (af, Bil.Unknown ("AF is undefined after and", bool_t))
        :: set_pszf t (Bil.Var tmp)
      | Ptest(t, o1, o2) ->
        let open Stmt in
        let t' = !!t in
        let tmp1 = tmp t and tmp2 = tmp t in
        Move (tmp1, Bil.(op2e t o2 land op2e t o1))
        :: Move (tmp2, Bil.(op2e t o2 land (exp_not (op2e t o1))))
        :: Move (af, exp_false)
        :: Move (oF, exp_false)
        :: Move (pf, exp_false)
        :: Move (sf, exp_false)
        :: Move (zf, Bil.(Var tmp1 = Int (BV.of_int ~width:t' 0)))
        :: [Move (cf, Bil.(Var tmp2 = Int (BV.of_int ~width:t' 0)))]
      | Not(t, o) ->
        [assn t o (exp_not (op2e t o))]
      | Neg(t, o) ->
        let t' = !!t in
        let tmp = tmp t in
        let min_int =
          Bil.BinOp (LSHIFT, int_exp 1 t', int_exp (t'-1) t')
        in
        Bil.Move (tmp, op2e t o)
        ::assn t o Bil.(int_exp 0 t' - op2e t o)
        ::Bil.Move (cf, Bil.(Ite (Var tmp = int_exp 0 t', int_exp 0 1, int_exp 1 1)))
        ::Bil.Move (oF, Bil.(Ite (Var tmp = min_int, int_exp 1 1, int_exp 0 1)))
        ::set_apszf_sub t' (Bil.Var tmp) (int_exp 0 t') (op2e t o)
      | Mul (t, src) ->
        (* Mul always multiplies EAX by src and stores the result in EDX:EAX
           starting from the "right hand side" based on the type t of src *)

        (* The OF and CF flags are set to 0 if the upper half of the result is 0;
           otherwise, they are set to 1 *)
        let new_t = Type.Imm (!!t * 2) in
        let assnstmts, assne = Bil.(assn_dbl t ((Cast (UNSIGNED, !!new_t, op2e t o_rax)) * (Cast (UNSIGNED, !!new_t, op2e t src))))
        in
        let flag =
          let highbit = !!new_t - 1 in
          let lowbit = !!new_t / 2 in
          Bil.((Extract (highbit, lowbit, assne)) <> int_exp 0 !!t)
        in
        assnstmts
        @
        [
          Bil.Move (oF, flag);
          Bil.Move (cf, flag);
          Bil.Move (sf, Bil.Unknown ("SF is undefined after Mul", bool_t));
          Bil.Move (zf, Bil.Unknown ("ZF is undefined after Mul", bool_t));
          Bil.Move (af, Bil.Unknown ("AF is undefined after Mul", bool_t));
          Bil.Move (pf, Bil.Unknown ("PF is undefined after Mul", bool_t))
        ]
      | Imul (t, (oneopform, dst), src1, src2) ->
        let new_t = Type.Imm (!!t * 2) in
        let mul_stmts =
          (match oneopform with
           | true ->
             (* For one operand form, use assn_double *)
             let assnstmts, assne =
               assn_dbl t Bil.((Cast (SIGNED, !!new_t, op2e t src1)) * (Cast (SIGNED, !!new_t, op2e t src2))) in
             let flag =
               (* Intel checks if EAX == EDX:EAX.  Instead of doing this, we are just
                  going to check if the upper bits are != 0 *)
               let highbit = !!new_t - 1 in
               let lowbit = !!new_t / 2 in
               Bil.((Extract (highbit, lowbit, assne)) <> int_exp 0 !!t)
             in
             assnstmts @
             [Bil.Move (oF, flag);
              Bil.Move (cf, flag)]
           | false ->
             (* Two and three operand forms *)
             let tmp = tmp new_t in
             (* Flag is set when the result is truncated *)
             let flag = Bil.(Var tmp <> Cast (SIGNED, !!new_t, op2e t dst)) in
             [(Bil.Move (tmp, Bil.((Cast (SIGNED, !!new_t, op2e t src1)) * (Cast (SIGNED, !!new_t, op2e t src2)))));
              (assn t dst Bil.(Cast (LOW, !!t, Var tmp)));
              Bil.Move (oF, flag);
              Bil.Move (cf, flag)] )
        in
        mul_stmts@[
          Bil.Move (pf, Bil.Unknown ("PF is undefined after imul", bool_t));
          Bil.Move (sf, Bil.Unknown ("SF is undefined after imul", bool_t));
          Bil.Move (zf, Bil.Unknown ("ZF is undefined after imul", bool_t));
          Bil.Move (af, Bil.Unknown ("AF is undefined after imul", bool_t));]
      | Div(t, src) ->
        let dt' = !!t * 2 in
        let dt = Type.Imm dt' in
        let dividend = op2e_dbl t in
        let divisor = Bil.(Cast (UNSIGNED, !!dt, op2e t src)) in
        let tdiv = tmp ~name:"div" dt in
        let trem = tmp ~name:"rem" dt in
        let assne = Bil.((Cast (LOW, !!t, Bil.Var trem)) ^ (Cast (LOW, !!t, Var tdiv))) in
        Bil.If (Bil.(divisor = int_exp 0 dt'), [Cpu_exceptions.divide_by_zero], [])
        :: Bil.Move (tdiv, Bil.(dividend / divisor))
        :: Bil.Move (trem, Bil.(dividend mod divisor))
        (* Overflow is indicated with the #DE (divide error) exception
           rather than with the CF flag. *)
        :: Bil.If (Bil.((Cast (HIGH, !!t, Var tdiv)) = int_exp 0 !!t), [], [Cpu_exceptions.divide_by_zero])
        :: fst (assn_dbl t assne)
        @ (let undef r =
             Bil.Move (r, Bil.Unknown ((Var.name r ^ " undefined after div"), Var.typ r))
           in
           List.map ~f:undef [cf; oF; sf; zf; af; pf])
      | Idiv(t, src) ->
        let dt' = !!t * 2 in
        let dt = Type.Imm dt' in
        let dividend = op2e_dbl t in
        let divisor = Bil.(Cast (SIGNED, !!dt, op2e t src)) in
        let tdiv = tmp ~name:"div" dt in
        let trem = tmp ~name:"rem" dt in
        let assne = Bil.((Cast (LOW, !!t, Var trem)) ^ (Cast (LOW, !!t, Var tdiv))) in
        Bil.If (Bil.(divisor = int_exp 0 dt'), [Cpu_exceptions.divide_by_zero], [])
        :: Bil.Move (tdiv, Bil.(dividend /$ divisor))
        :: Bil.Move (trem, Bil.(dividend %$ divisor))
        (* Overflow is indicated with the #DE (divide error) exception
           rather than with the CF flag. *)
        (* SWXXX For signed division make sure quotient is between smallest and
           largest values.  For type t, this would be -2^(t/2) to (2^(t/2) - 1). *)
        :: Bil.If (Bil.((Cast (HIGH, !!t, Var tdiv)) = int_exp 0 !!t), [], [Cpu_exceptions.divide_by_zero])
        :: fst (assn_dbl t assne)
        @ (let undef r =
             Bil.Move (r, Bil.Unknown (Var.name r ^ " undefined after div", Var.typ r)) in
           List.map ~f:undef [cf; oF; sf; zf; af; pf])
      | Cld ->
        [Bil.Move (df, exp_false)]
      | Leave t when pref = [] -> (* #UD if Lock prefix is used *)
        Bil.Move (rsp, rbp_e)
        ::to_ir mode addr next ss pref has_rex has_vex (Pop(t, o_rbp))
      | Interrupt3 -> [Bil.Special "int3"]
      | Interrupt(Oimm i) ->
        (** use [BV.string_of_value ~hex:true] here *)
        [Bil.Special ("int " ^ Addr.string_of_value i)]
      | Sysenter | Syscall -> [Bil.Special "syscall"]
      (* Match everything exhaustively *)
      | Leave _ ->  unimplemented "to_ir: Leave"
      | Call _ ->  unimplemented "to_ir: Call"
      | Lea _ ->  unimplemented "to_ir: Lea"
      | Movs _ ->  unimplemented "to_ir: Movs"
      | Cmps _ ->  unimplemented "to_ir: Cmps"
      | Scas _ ->  unimplemented "to_ir: Scas"
      | Stos _ ->  unimplemented "to_ir: Stos"
      | Retn _ ->  unimplemented "to_ir: Retn"
      | Interrupt _ ->  unimplemented "to_ir: Interrupt"

end (* ToIR *)

let disasm_instr mode mem addr =
  let module D = X86_disasm in
  let (pref, prefix, op, na) = D.parse_instr mode mem addr in
  let has_rex = prefix.rex <> None in
  let has_vex = prefix.vex <> None in
  let (ss, pref) = D.parse_prefixes mode pref op in
  ToIR.to_ir mode addr na ss pref has_rex has_vex op

let insn arch mem insn =
  let mode = match arch with `x86 -> X86 | `x86_64 -> X8664 in
  let addr = Memory.min_addr mem in
  Or_error.try_with (fun () -> disasm_instr mode mem addr) |> function
  | Error err ->
    warning "the legacy lifter failed at %a - %a"
      pp_insn (mem,insn) Error.pp err;
    Error err
  | Ok bil -> match Type.check bil with
    | Ok () -> Ok bil
    | Error err ->
      warning "BIL is not well-type in the legacy lifter at %a - %a"
        pp_insn (mem,insn) Type.Error.pp err;
      Error (Error.of_string "type error")

module AMD64 = struct
  module CPU = X86_cpu.AMD64
  let lift mem i = insn `x86_64 mem i
end

module IA32 = struct
  module CPU = X86_cpu.IA32
  let lift mem i = insn `x86 mem i
end
