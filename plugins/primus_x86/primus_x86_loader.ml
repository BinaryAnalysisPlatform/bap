open Core_kernel
open Bap.Std
open Bap_primus.Std
open X86_cpu

module Plt = struct
  let is_plt_section v =
    match Value.get Image.section v with
    | Some x -> x = ".plt"
    | _ -> false

  let is_plt_symbol sec sym =
    match Term.get_attr sym address with
    | None -> false
    | Some a -> Memmap.contains sec a

  let symbols p =
    let sec = Memmap.filter (Project.memory p) ~f:is_plt_section in
    Term.to_sequence sub_t (Project.program p) |>
    Seq.filter ~f:(is_plt_symbol sec)
end

module Got = struct

  let cells plt_sym =
    let find j =
      match Jmp.kind j with
      | Goto _ | Int _ | Ret _ -> None
      | Call c ->
        match Call.target c with
        | Indirect Bil.(Load (_,Int addr,_,_)) -> Some addr
        | _ -> None in
    (object
      inherit [addr list] Term.visitor

      method! enter_jmp c acc =
        match find c with
        | None -> acc
        | Some a -> a :: acc
    end)#visit_sub plt_sym []
end

module Make_unresolved(Machine : Primus.Machine.S) = struct
  module Linker = Primus.Linker.Make(Machine)
  open Machine.Syntax

  let exec =
    Linker.exec (`symbol Primus.Linker.unresolved_handler)
end

module Plt_entry(Machine : Primus.Machine.S) = struct
  module Memory = Primus.Memory.Make(Machine)
  module Linker = Primus.Linker.Make(Machine)
  open Machine.Syntax

  let load bytes endian addr =
    let concat = match endian with
      | LittleEndian -> fun x y -> Addr.concat y x
      | BigEndian -> Addr.concat in
    let last = Addr.nsucc addr bytes in
    let rec load data addr =
      if Addr.(addr < last) then
        Memory.load addr >>= fun x ->
        load (concat data x) (Addr.succ addr)
      else Machine.return data in
    Memory.load addr >>= fun data ->
    load data (Addr.succ addr)

  let unresolve_jumps sym =
    Machine.arch >>= fun arch ->
    let endian = Arch.endian arch in
    let size = Size.in_bytes (Arch.addr_size arch) in
    Machine.List.iter (Got.cells sym) ~f:(fun cell ->
        load size endian cell >>= fun addr ->
        Linker.link ~addr (module Make_unresolved))
end

module Component(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Env = Primus.Env.Make(Machine)
  module Value = Primus.Value.Make(Machine)
  module Interpreter = Primus.Interpreter.Make(Machine)
  module Plt_entry = Plt_entry(Machine)

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

  let unresolve_plt_jumps p =
    Machine.Seq.iter (Plt.symbols p) ~f:Plt_entry.unresolve_jumps

  let init () =
    Machine.get () >>= fun proj ->
    Machine.sequence @@
    match Project.arch proj with
    | `x86 ->
      [initialize_flags IA32.flags;
       correct_sp IA32.sp 4;
       unresolve_plt_jumps proj]
    | `x86_64 ->
      [initialize_flags AMD64.flags;
       correct_sp AMD64.sp 8;
       unresolve_plt_jumps proj]
    | _ -> []

end
