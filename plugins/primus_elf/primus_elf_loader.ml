open Bap.Std
open Primus.Std

module type Param = sig
  val stack_size : int
  val stack_base : int64
end

module Make(Param : Param)(Machine : Machine.S)  = struct
  open Param
  open Machine.Syntax

  module Env = Env.Make(Machine)
  module Memory = Memory.Make(Machine)

  let proj () = Machine.get () >>| fun ctxt -> ctxt#project
  let arch () = proj () >>| Project.arch
  let target () = arch () >>| target_of_arch


  let make_addr addr =
    arch () >>| Arch.addr_size >>| fun size ->
    Addr.of_int64 ~width:(Size.in_bits size) addr


  let setup_stack () =
    target () >>= fun (module Target) ->
    make_addr stack_base >>= fun bottom ->
    let base = Addr.(bottom -- stack_size) in
    Env.set Target.CPU.sp base >>= fun () ->
    Memory.allocate
      ~readonly:false
      ~executable:false
      base stack_size

  let load_segments () =
    proj () >>| Project.memory >>= fun memory ->
    Memmap.to_sequence memory |>
    Machine.Seq.iter ~f:(fun (mem,tag) ->
        match Value.get Image.segment tag with
        | None -> Machine.return ()
        | Some seg ->
          let alloc =
            if Image.Segment.is_executable seg
            then Memory.add_text else Memory.add_data in
          alloc mem)

  let init () =
    setup_stack () >>= fun () ->
    load_segments ()
end
