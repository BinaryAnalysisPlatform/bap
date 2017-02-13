open Core_kernel.Std
open Bap.Std
open X86_opcode_btx

module Make (Tools : X86_tools.S) (Backend : X86_backend.S) = struct
  open Tools
  let set_flags op base off =
    let op = match op with
      | #bt -> "bt"
      | #btc -> "btc"
      | #btr -> "btr"
      | #bts -> "bts" in
    FR.set `CF Bil.(cast low 1 (base lsr off)) ::
    List.map ~f:(fun f -> FR.set_unknown f op) [`OF; `SF; `ZF; `AF; `PF]

  let offset_width = function
    | `r16 -> 4
    | `r32 -> 5
    | `r64 -> 6
    | _ -> invalid_arg "Btx: offset width"

  let local_var name width =
    Var.create ~is_virtual:true ~fresh:true name @@ Type.imm width

  let one size = Size.in_bits size |> Word.one

  let mask_imm size o () = Word.lshift (one size) o |> Bil.int

  let mask_reg size o () = Bil.(int (one size) lsl var o)

  let side_effect op data (mask:unit -> exp) (stmt: exp -> stmt) =
    match op with
    | #bt -> []
    | #btc -> [stmt Bil.(data lxor mask ())]
    | #btr -> [stmt Bil.(data land lnot (mask ()))]
    | #bts -> [stmt Bil.(data lor mask ())]

  let btx_rr (op:btx_rr) =
    X86_operands.rr ~f:(fun _mem base offset ->
        let base = RR.of_mc_exn base in
        let offset = RR.of_mc_exn offset in
        let size = RR.width base in
        let w =  offset_width size in
        let o = local_var "o" w in
        let base = RR.var base in
        let offset = RR.var offset in
        let bil =
          let off = Bil.(o := cast low w (var offset)) in
          let flag = set_flags op (Bil.var base) (Bil.var o) in
          let side = side_effect op (Bil.var base)
              (mask_reg RR.size o)
              (fun exp -> Bil.(base := exp)) in
          List.concat [off::flag; side] in
        Ok bil)

  let btx_ri (op:btx_ri) =
    X86_operands.ri ~f:(fun _mem base offset ->
        let base = RR.of_mc_exn base in
        let size = RR.width base in
        let o = Imm.to_word offset ~width:(offset_width size) |>
                Option.value_exn in
        let base = RR.var base in
        let bil =
          let flags = set_flags op (Bil.var base) (Bil.int o) in
          let side = side_effect op (Bil.var base)
              (mask_imm RR.size o)
              (fun exp -> Bil.(base := exp)) in
          List.concat [flags; side] in
        Ok bil)

  let btx_mi (op:btx_mi) =
    X86_operands.mi ~f:(fun mem ~seg ~base ~scale ~index ~disp offset ->
        let base_mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let size = match op with
          | `BT64mi8 | `BTC64mi8 | `BTR64mi8 | `BTS64mi8 -> `r64
          | `BT32mi8 | `BTC32mi8 | `BTR32mi8 | `BTS32mi8 -> `r32
          | `BT16mi8 | `BTC16mi8 | `BTR16mi8 | `BTS16mi8 -> `r16 in
        let a = local_var "a" @@ Size.in_bits MM.addr_size in
        let d = local_var "d" @@ Size.in_bits size in
        let o = Imm.to_word offset ~width:(offset_width size) |>
                Option.value_exn in
        let bil =
          let load = Bil.[
              a := MM.addr base_mem;
              d := MM.load_from ~size (var a)
            ] in
          let flag = set_flags op (Bil.var d) (Bil.int o) in
          let side = side_effect op (Bil.var d)
              (mask_imm size o)
              (fun exp -> MM.store_to Bil.(var a) ~size exp) in
          List.concat [load; flag; side] in
        Ok bil)

  let btx_mr (op:btx_mr) =
    X86_operands.mr ~f:(fun mem ~seg ~base ~scale ~index ~disp offset ->
        let base_mem = MM.of_mem mem ~seg ~base ~scale ~index ~disp in
        let offset = RR.of_mc_exn offset in
        let size = RR.width offset in
        let bsize = Size.in_bits size in
        let bsize_addr = Size.in_bits MM.addr_size in
        let a = local_var "a" bsize_addr in
        let b = local_var "b" bsize_addr in
        let d = local_var "d" bsize in
        let w = offset_width size in
        let o = local_var "o" w in
        let f = Word.of_int ~width:bsize (Size.in_bytes size) in
        let dv = Word.of_int ~width:bsize bsize in
        let bil =
          let load =
            let bexp =
              let e = Bil.(int f * (RR.get offset /$ int dv)) in
              if bsize_addr <> bsize
              then Bil.(cast signed bsize_addr e)
              else e in
            Bil.[
              a := MM.addr base_mem;
              b := bexp;
              d := MM.load_from ~size (var a + var b);
            ] in
          let flags =
            Bil.(o := cast low w (var (RR.var offset))) ::
            set_flags op (Bil.var d) (Bil.var o) in
          let side = side_effect op (Bil.var d)
              (mask_reg size o)
              (fun exp -> MM.store_to Bil.(var a + var b) ~size exp) in
          List.concat [load; flags; side] in
        Ok bil)

  let btx (op:btx) =
    let open Or_error in
    let lift = match op with
      | #btx_rr as op -> btx_rr op
      | #btx_ri as op -> btx_ri op
      | #btx_mi as op -> btx_mi op
      | #btx_mr as op -> btx_mr op in
    fun mem insn ->
      lift mem insn >>= fun bil ->
      match op with
      | #bt_crs_mem when X86_prefix.exists `xF0 mem -> Ok (PR.lock bil)
      | _ -> Ok bil

  let register what =
    let name op = sexp_of_btx (op :> btx) |> Sexp.to_string in
    List.iter (what :> btx list)
      ~f:(fun op -> Backend.register (name op) (btx op))

end

module IA32 = Make (X86_tools.IA32) (X86_backend.IA32)
module AMD64 = Make (X86_tools.AMD64) (X86_backend.AMD64)

let () =
  IA32.register all_of_btx_ia32;
  AMD64.register all_of_btx
