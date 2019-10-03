open Core_kernel
open Bap.Std
open Bap_primus.Std
open X86_cpu

let is_section name v =
  match Value.get Image.section v with
  | Some x -> String.(x = name)
  | _ -> false

let address_range mem arch =
  let step = Arch.addr_size arch |> Size.in_bytes in
  let rec loop acc n =
    if Addr.(n > Memory.max_addr mem)
    then acc
    else
      let next = Addr.(n ++ step) in
      loop (n :: acc) next in
  loop [] (Memory.min_addr mem)

module Make_unresolved(Machine : Primus.Machine.S) = struct
  module Linker = Primus.Linker.Make(Machine)

  let exec =
    Linker.exec (`symbol Primus.Linker.unresolved_handler)
end

let try_load mem size addr =
  match Memory.get ~scale:size ~addr mem with
  | Ok data -> Some data
  | _ -> None

module Plt_jumps(Machine : Primus.Machine.S) = struct
  module Interp = Primus.Interpreter.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  module Value  = Primus.Value.Make(Machine)

  open Machine.Syntax

  let section_memory sec_name =
    Machine.get () >>| fun proj ->
    Memmap.filter (Project.memory proj) ~f:(is_section sec_name) |>
    Memmap.to_sequence |>
    Seq.map ~f:fst |>
    Seq.to_list

  let got_cells =
    Machine.arch >>= fun arch ->
    section_memory ".got.plt" >>| fun memory ->
    List.fold memory ~init:[]
      ~f:(fun acc mem -> acc @ address_range mem arch)

  let load_word addr =
    Machine.arch >>= fun arch ->
    Value.of_word addr >>= fun addr ->
    Interp.load addr (Arch.endian arch)
      (Arch.addr_size arch :> size) >>|
    Value.to_word

  let load_table =
    got_cells >>=
    Machine.List.map ~f:load_word

  let load_table =
    Machine.arch >>= fun arch ->
    section_memory ".got.plt" >>| fun memory ->
    List.fold memory ~init:[]
      ~f:(fun acc mem ->
        let range = address_range mem arch in
        acc @ List.filter_map range
                ~f:(try_load mem (Arch.addr_size arch :> size)))

  let filter_plt addrs =
    section_memory ".plt" >>= fun memory ->
    Machine.return @@
    List.filter addrs ~f:(fun a ->
        List.exists memory ~f:(fun mem -> Memory.contains mem a))

  let unlink addrs =
    Machine.List.iter addrs ~f:(fun addr ->
        Linker.link ~addr (module Make_unresolved))

  let unresolve =
    load_table >>=
    filter_plt >>=
    unlink

end

module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Interpreter = Primus.Interpreter.Make(Machine)
  module Plt_jumps = Plt_jumps(Machine)

  let zero = Primus.Generator.static 0

  let initialize_flags flags =
    Machine.Seq.iter (Set.to_sequence flags) ~f:(fun reg ->
        Env.add reg zero)

  let correct_sp sp addend _ =
    Env.get sp >>= fun x ->
    Interpreter.binop Bil.plus x addend >>= Env.set sp

  let correct_sp sp addend =
    match Var.typ sp with
    | Mem _ -> Machine.return ()
    | Imm width ->
      Value.of_int ~width addend >>= fun addend ->
      Primus.Linker.Trace.lisp_call_return >>> correct_sp sp addend

  let init () =
    Machine.get () >>= fun proj ->
    Machine.sequence @@
    match Project.arch proj with
    | `x86 ->
      [initialize_flags IA32.flags;
       correct_sp IA32.sp 4;
       Plt_jumps.unresolve ]
    | `x86_64 ->
      [initialize_flags AMD64.flags;
       correct_sp AMD64.sp 8;
       Plt_jumps.unresolve ]
    | _ -> []

end
