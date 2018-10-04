open Core_kernel.Std
open Bap.Std
open X86_tools_types


module Make (CPU : X86CPU) (RR : RR) (IM : IM) : MM = struct

  let addr_size = Arch.addr_size (CPU.arch :> arch)
  module Segment = struct
    module Base = struct
      type t =
        | GPR of RR.t
        | IP of word
        [@@ deriving variants, sexp]
      let create mem' base =
        let open Option in
        X86_asm.Reg.decode base >>=
        (function
          | #X86_asm.Reg.gpr -> RR.of_mc_exn base |> gpr |> some
          | `IP | `EIP | `RIP ->
            Memory.max_addr mem' |> Word.succ |> ip |> some
          | b -> Error.failwiths "invalid base" b X86_asm.Reg.sexp_of_t)

    end

    type t = {
      seg : RR.t option;
      base : Base.t option;
      scale : int option;
      index : RR.t option;
      disp : int;
    } [@@ deriving fields, sexp]

    let create ?seg ?base ?scale ?index ~disp mem =
      let seg =
        let base = function
          | `FS -> RR.of_asm_exn `FS_BASE |> Option.some
          | `GS -> RR.of_asm_exn `GS_BASE |> Option.some
          | `CS | `DS | `ES | `SS -> None in
        let open X86_asm in
        let seg =
          Option.value_map ~default:None ~f:X86_asm.Reg.decode seg in
        match seg with
        | None -> None
        | Some (#Reg.segment as s) -> base s
        | Some r -> Error.failwiths "invalid segment" r
                      X86_asm.Reg.sexp_of_t in
      let map = Option.value_map ~default:None in
      let base = map ~f:(Base.create mem) base in
      let scale = map ~f:Imm.to_int scale in
      let index = map ~f:RR.of_mc index in
      let disp = Imm.to_int disp |> Option.value_exn in
      Fields.create ~seg ~base ~scale ~index ~disp

    let make_value reg =
      let size = Size.in_bits addr_size in
      let open X86_asm in
      match RR.to_asm reg, CPU.arch with
      | #Reg.r8, _
      | #Reg.r16, _
      | #Reg.r32, `x86_64 -> (RR.get reg |> Bil.(cast unsigned size))
      | #Reg.r32, `x86
      | #Reg.r64, _
      | #Reg.segment_base, _ -> RR.get reg
      | (#Reg.segment | #Reg.r128 | #Reg.r256), _ ->
        Error.failwiths "invalid address register" reg RR.sexp_of_t

    let make_scale scale =
      let shift = match scale with
        | 1 -> None
        | 2 -> Some 1
        | 4 -> Some 2
        | 8 -> Some 3
        | s -> Error.failwiths "invalid memory scale" s sexp_of_int in
      Option.map ~f:(fun s -> Word.of_int ~width:2 s |> Bil.int) shift

    let disp_exp disp =
      Bil.int @@ Word.of_int ~width:(Size.in_bits addr_size) disp

    let addr {seg; base; scale; index; disp} =
      let seg = Option.map ~f:(fun seg -> make_value seg) seg in
      let base = Option.map base ~f:(function
          | Base.GPR base -> make_value base
          | Base.IP word -> Bil.int word) in

      let scale = Option.value_map
          ~default:None
          ~f:make_scale scale in
      let index = Option.map ~f:make_value index in
      let default = disp_exp disp in
      let disp = Option.some_if (disp <> 0) (disp_exp disp) in

      let ( + ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op1 + op2) |> Option.some
        | Some _ as op, None -> op
        | None, (Some _ as op) -> op
        | None, None -> None in

      let ( * ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op2 lsl op1) |> Option.some
        | None, (Some _ as op) -> op
        | _, None -> None in

      match seg + (base + scale * index + disp) with
      | None -> default
      | Some x -> x

  end


  module Relative = struct
    type t = IM.t

    let create off = IM.of_imm off

    let addr off =  IM.get off ~width:(addr_size :> size)

  end

  type t =
    | Segment of Segment.t
    | Relative of Relative.t
    [@@ deriving variants]

  let of_mem ?seg ?base ?scale ?index ~disp mem =
    segment @@ Segment.create mem ?seg ?base ?scale ?index ~disp

  let of_offset imm =
    Relative.create imm |> relative

  let addr =
    Variants.map ~segment:(fun _ -> Segment.addr)
      ~relative:(fun _ -> Relative.addr)

  let load_from addr ~size =
    let mem = Bil.var CPU.mem in
    Bil.load ~mem ~addr LittleEndian size

  let store_to addr ~size data =
    let mem = Bil.var CPU.mem in
    Bil.(CPU.mem := store ~mem ~addr data LittleEndian size)

  let load t ~size =  addr t |> load_from ~size

  let store t ~size data = addr t |> fun addr -> store_to ~size addr data
end
