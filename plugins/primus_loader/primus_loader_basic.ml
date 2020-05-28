open Core_kernel
open Bap.Std
open Bap_primus.Std
open Format

module Generator = Primus.Generator

module type Param = sig
  val stack_size : int
  val stack_base : int64
end

module Make(Param : Param)(Machine : Primus.Machine.S)  = struct
  open Param
  open Machine.Syntax

  module Env = Primus.Env.Make(Machine)
  module Mem = Primus.Memory.Make(Machine)
  module Val = Primus.Value.Make(Machine)

  let target = Machine.arch >>| target_of_arch

  let make_word addr =
    Machine.arch >>| Arch.addr_size >>| fun size ->
    Addr.of_int64 ~width:(Size.in_bits size) addr

  let set_word name x =
    let t = Type.imm (Word.bitwidth x) in
    let var = Var.create name t in
    Val.of_word x >>=
    Env.set var

  (* bottom points to the end of the stack, ala STL end pointer.
     Note: bottom is usually depicted at the top of the stack
     structure, and it is the highest address.
     Note: memory beyond top is readable, as it is the place,
     where kernel should put argc, argv, and other info to a
     user process *)
  let setup_stack () =
    target >>= fun (module Target) ->
    make_word stack_base >>= fun bottom ->
    let top = Addr.(bottom -- stack_size) in
    Val.of_word bottom >>= fun bottom ->
    Env.set Target.CPU.sp bottom >>= fun () ->
    Mem.allocate
      ~readonly:false
      ~executable:false
      top stack_size

  let setup_registers () =
    let zero = Generator.static 0 in
    target >>= fun (module Target) ->
    Set.to_sequence Target.CPU.gpr |>
    Machine.Seq.iter ~f:(fun reg -> Env.add reg zero)

  let rec is set = function
    | Backend.Or (p1,p2) -> is set p1 || is set p2
    | bit -> [%compare.equal : Backend.perm] bit set
  [@@warning "-D"]

  let segmentations =
    let open Image.Scheme in
    Ogre.foreach Ogre.Query.(begin
        select (from segment)
      end)
      ~f:ident

  let get_segmentations proj =
    match Project.get proj Image.specification with
    | None -> Ok Seq.empty
    | Some spec -> Ogre.eval segmentations spec

  let load_segments () =
    Machine.project >>= fun proj ->
    make_word 0L >>= fun null ->
    match get_segmentations proj with
    | Error _ -> assert false
    | Ok segs ->
      Machine.Seq.fold ~init:null segs
        ~f:(fun endp {Image.Scheme.addr; size; info=(_,w,x)} ->
            make_word addr >>= fun lower ->
            make_word Int64.(size-1L) >>= fun diff ->
            let upper = Word.(lower + diff) in
            Mem.add_region () ~lower ~upper
              ~readonly:(not w)
              ~executable:x
              ~generator:(Generator.static 0) >>| fun () ->
            Addr.max endp Addr.(succ upper))

  let map_segments () =
    Machine.project >>= fun proj ->
    make_word 0L >>= fun null ->
    Memmap.to_sequence (Project.memory proj) |>
    Machine.Seq.fold ~init:null ~f:(fun endp (mem,tag) ->
        match Value.get Image.segment tag with
        | None -> Machine.return endp
        | Some seg ->
          let alloc =
            if Image.Segment.is_executable seg
            then Mem.add_text else Mem.add_data in
          let maddr = Memory.max_addr mem in
          let update_ends = match Image.Segment.name seg with
            | ".text" -> set_word "etext" maddr
            | ".data" -> set_word "edata" maddr
            | _ -> Machine.return () in
          alloc mem >>= fun () ->
          update_ends >>| fun ()  ->
          Addr.max endp maddr)

  let bytes_in_array =
    Array.fold ~init:0 ~f:(fun sum str ->
        sum + String.length str + 1)

  let word_of_char ch = Word.of_int ~width:8 (Char.to_int ch)

  let save_string str ptr =
    String.to_list str |>
    Machine.List.fold ~init:ptr ~f:(fun ptr char ->
        Mem.store ptr (word_of_char char) >>| fun () ->
        Word.succ ptr)

  let save_args array ptr =
    Seq.of_array array |>
    Machine.Seq.fold ~init:(ptr,[]) ~f:(fun (ptr',ptrs) str ->
        save_string str ptr' >>=
        save_string "\x00"  >>| fun ptr ->
        (ptr,ptr'::ptrs)) >>| fun (ptr,ptrs) ->
    ptr, List.rev ptrs

  let save_word endian word ptr =
    Word.enum_bytes word endian |>
    Machine.Seq.fold ~init:ptr ~f:(fun ptr byte ->
        Mem.store ptr byte >>| fun () ->
        Word.succ ptr)

  let save_table endian addrs ptr =
    Machine.List.fold addrs ~init:ptr ~f:(fun ptr addr ->
        save_word endian addr ptr)

  let setup_main_frame () =
    target >>= fun (module Target) ->
    Machine.arch >>= fun arch ->
    Machine.args >>= fun argv ->
    Machine.envp >>= fun envp ->
    make_word stack_base >>= fun sp ->
    let endian = Arch.endian arch in
    let addr_size = Arch.addr_size arch in
    let argc = Array.length argv |>
               Word.of_int ~width:(Size.in_bits addr_size) in
    let bytes_in_addr = Size.in_bytes addr_size in
    let null = String.make bytes_in_addr '\x00' in
    let frame_size args = bytes_in_array args in
    let table_size args = bytes_in_addr * (Array.length args + 1) in
    let total_size =
      bytes_in_addr +    (* argc *)
      table_size argv + table_size envp +
      frame_size argv + table_size envp in
    let argv_frame_ptr =
      bytes_in_addr +
      table_size  argv + table_size envp |>
      Addr.nsucc sp in
    let argv_table_ptr = Addr.nsucc sp bytes_in_addr in
    Mem.allocate
      ~readonly:false
      ~executable:false
      sp total_size >>= fun () ->
    save_args argv argv_frame_ptr >>=
    fun (envp_frame_ptr, argv_table) ->
    save_args envp envp_frame_ptr >>=
    fun (_end_of_stack, envp_table) ->
    save_table endian argv_table argv_table_ptr >>=
    fun end_of_argv_table ->
    save_string null end_of_argv_table >>=
    fun envp_table_ptr ->
    save_table endian envp_table envp_table_ptr >>=
    fun end_of_envp_table ->
    save_string null end_of_envp_table >>=
    fun _argv_frame_ptr ->
    assert Word.(argv_frame_ptr = _argv_frame_ptr);
    save_word endian argc sp >>= fun _ ->
    set_word "environ" envp_table_ptr


  let names prog = (object
    inherit [addr String.Map.t] Term.visitor
    method! enter_term _ t env =
      match Term.get_attr t address with
      | None -> env
      | Some addr -> Map.set env ~key:(Term.name t) ~data:addr
  end)#run prog String.Map.empty

  let init_names () =
    Machine.get () >>= fun proj ->
    Map.to_sequence (names (Project.program proj)) |>
    Machine.Seq.iter ~f:(fun (name,addr) -> set_word name addr)

  let init () =
    setup_stack () >>= fun () ->
    setup_main_frame () >>= fun () ->
    load_segments () >>= fun e1 ->
    map_segments () >>= fun e2 ->
    let endp = Addr.max e1 e2 in
    set_word "endp" endp >>= fun () ->
    set_word "brk"  endp >>= fun () ->
    setup_registers () >>= fun () ->
    init_names ()
end
