open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap_c.Std

module Arg = C.Abi.Arg
open Arg.Language

let data model = object
  inherit C.Size.base model
  method! real = function
    | `float -> `r32
    | `double | `long_double -> `r64
end

let arena t name ~from len =
  Arg.Arena.create @@
  List.init len ~f:(fun i ->
      Option.value_exn
        (Theory.Target.var t (sprintf "%s%d" name (from+i))))

let define t =
  let bits = Theory.Target.bits t in
  let data = data (if bits = 32 then `ILP32 else `ILP64) in
  let is_old = Fn.const (bits = 32) in
  let regs = if bits = 32 then 4 else 8 in
  install t data @@ fun declare ->
  let* iargs = arena t "R" ~from:4 regs in
  let* irets = Arg.Arena.irets t in
  let* fargs = arena t "F" ~from:1 regs in
  let* frets = Arg.Arena.frets t in
  let ipass = sequence [
      always, choose [
        Arg.split_with_memory iargs;
        Arg.memory;
      ];
      is_old, const @@ Arg.deplet fargs;
    ] in
  let fpass size = choose [
      sequence [
        is_old, const@@Arg.align_even fargs;
        always, Arg.registers fargs;
        is_old, const@@Arg.discard ~n:(size/32) iargs;
      ];
      Arg.memory;
    ] in

  let return ~alignment:_ _ = select [
      C.Type.is_floating, choose [
        Arg.registers frets;
        Arg.reference iargs;
      ];
      otherwise, choose [
        Arg.register irets;
        Arg.reference iargs;
      ]
    ] in
  declare ~return @@ fun ~alignment:_ size -> select [
    C.Type.is_floating, fpass size;
    otherwise, ipass;
  ]

let setup () =
  Theory.Target.filter ~parent:Bap_mips_target.parent ()
    ~abi:Theory.Abi.gnu |> List.iter ~f:define
