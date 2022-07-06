open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_c.Std
open Bap_core_theory

module Arg = C.Abi.Arg
open Arg.Language

let data model = object(self)
  inherit C.Size.base model
  method! enum elts =
    if Int64.(C.Size.max_enum_elt elts < (1L lsl 32))
    then self#integer `uint
    else self#integer `ulong_long
  method! real = function
    | `float -> `r32
    | `double | `long_double -> `r64
end

module Aapcs32 = struct

  let define t =
    install t (data `ILP32) @@ fun describe ->
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
end

module Aapcs64 = struct
  let is_composite t =
    C.Type.(is_structure t || is_union t)

  let define t =
    let model = data `LP64 in
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
end

let is_our_abi abi = List.exists ~f:(Theory.Abi.equal abi) Theory.Abi.[
    unknown; gnu; eabi; gnueabi;
  ]

let setup () =
  Theory.Target.family Arm_target.parent |>
  List.iter ~f:(fun t ->
      if is_our_abi (Theory.Target.abi t)
      then match Theory.Target.bits t with
        | 64 -> Aapcs64.define t
        | 32 -> Aapcs32.define t
        | _ -> ())
