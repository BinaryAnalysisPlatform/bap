open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap.Std
open Bap_c.Std
module Arg = C.Abi.Arg
open Arg.Language

let arena t name ~from len =
  Arg.Arena.create @@
  List.init len ~f:(fun i ->
      let name = sprintf "%s%d" name (from+i) in
      match Theory.Target.var t name with
      | Some v -> v
      | None -> failwithf "Target %s doesn't have a register named %s"
                  (Theory.Target.to_string t) name ())


let sysv32 t =
  let rev = Theory.Endianness.(equal le) (Theory.Target.endianness t) in
  install t (new C.Size.base `ILP32) @@ fun declare ->
  let* iargs = arena t ~from:3 "R" 8 in
  let* fargs = arena t ~from:1 "F" 8 in
  let* irets = Arg.Arena.irets t in
  let* frets = Arg.Arena.frets t in
  let return ~alignment:_ _ = select [
      C.Type.is_floating, Arg.registers ~rev frets;
      otherwise, choose [
        Arg.registers ~rev irets;
        Arg.reference iargs;
      ]
    ] in
  declare ~return @@ fun ~alignment:_ size -> select [
    C.Type.is_floating, choose [
      Arg.register fargs;
      Arg.memory;
    ];
    is (size > 32), choose [
      Arg.pointer iargs;
      Arg.memory;
    ];
    otherwise, choose [
      Arg.pointer iargs;
      Arg.memory;
    ]
  ]


let setup () =
  Theory.Target.family Bap_powerpc_target.parent |>
  List.iter ~f:(fun t ->
      if Theory.Target.bits t = 32 &&
         Theory.Abi.(Theory.Target.abi t = gnu) then sysv32 t)
