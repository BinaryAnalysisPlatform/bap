open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_c.Std

include Self()

module Aapcs32 = struct
  open Bap_core_theory
  open Bap_c.Std
  open Bap.Std

  module Arg = C.Abi.Arg
  open Arg.Language

  let model = object(self)
    inherit C.Size.base `ILP32
    method! enum elts =
      if Int64.(C.Size.max_enum_elt elts < (1L lsl 32))
      then self#integer `uint
      else self#integer `ulong_long
    method! real = function
      | `float -> `r32
      | `double | `long_double -> `r64
  end

  let define t =
    install t model @@ fun describe ->
    let* iargs = Arg.Arena.iargs t in
    let* irets = Arg.Arena.irets t in
    let rev = Theory.Endianness.(Theory.Target.endianness t = le) in
    let return ~alignment:_ size = select [
        C.Type.is_basic, select [
          is (size <= 32 * 2), Arg.registers ~rev  irets;
          otherwise, Arg.reference iargs;
        ];
        is (size <= 32), Arg.register irets;
        otherwise, Arg.reference iargs;
      ] in
    describe ~return @@ fun ~alignment _ ->
    sequence [
      is (alignment = 64), const (Arg.align_even iargs);
      always, choose [
        Arg.split_with_memory ~rev iargs;
        Arg.memory
      ];
    ]

  let supported_abis = Theory.Abi.[unknown; gnueabi; eabi]
  let is_our_abi abi = List.exists supported_abis ~f:(Theory.Abi.equal abi)


  let install () =
    Theory.Target.family Arm_target.parent |>
    List.iter ~f:(fun t ->
        if Theory.Target.bits t = 32 &&
           is_our_abi (Theory.Target.abi t)
        then define t)
end


module Aapcs64 = struct
  open Bap_core_theory
  open Bap_c.Std
  open Bap.Std

  let name = "aapcs64"

  module Arg = C.Abi.Arg
  open Arg.Language

  let data_model t =
    let bits = Theory.Target.bits t in
    new C.Size.base (if bits = 32 then `ILP32 else `LP64)

  let is_composite t =
    C.Type.(is_structure t || is_union t)

  let define t =
    let model = data_model t in
    let rev = Theory.Endianness.(Theory.Target.endianness t = le) in

    install t model @@ fun describe ->
    let* iargs = Arg.Arena.iargs t in
    let* irets = Arg.Arena.irets t in
    let* fargs = Arg.Arena.fargs t in
    let* frets = Arg.Arena.frets t in
    let* x8 = Arg.Arena.create [
        Option.value_exn (Theory.Target.var t "X8")] in

    let return ~alignment size = select [
        C.Type.is_floating, choose [
          Arg.registers ~rev frets;
          Arg.reference x8;
        ];
        otherwise, sequence [
          is (alignment >= 128), const (Arg.align_even irets);
          always, select [
            is (size > 16 * 8), Arg.reference x8;
            always, choose [
              Arg.registers ~rev irets;
              Arg.reference x8;
            ]
          ]
        ]
      ] in
    describe ~return @@ fun ~alignment size -> select [
      C.Type.is_floating, choose [
        Arg.registers ~rev fargs;
        Arg.reference iargs;
      ];
      otherwise, sequence [
        is (alignment >= 128), const (Arg.align_even iargs);
        always, select [
          is (size > 16 * 8), Arg.reference iargs;
          is_composite, choose [
            Arg.split_with_memory ~rev iargs;
            Arg.memory;
          ];
          otherwise, choose [
            Arg.registers ~rev iargs;
            combine [
              const (Arg.deplet iargs);
              Arg.memory;
            ]
          ]
        ]
      ]
    ]

  let is_our_abi abi = List.exists ~f:(Theory.Abi.equal abi) Theory.Abi.[
      unknown; gnu; eabi;
    ]

  let install () =
    Theory.Target.family Arm_target.parent |>
    List.iter ~f:(fun t ->
        if Theory.Target.bits t = 64 && is_our_abi (Theory.Target.abi t)
        then define t)


end

let setup () =
  Aapcs32.install ();
  Aapcs64.install ();
