open Core_kernel.Std
open Bap_bundle.Std
open Bap.Std
include Self()
open Option.Monad_infix

let call_of_jmp jmp = match Jmp.kind jmp with
  | Ret _ | Int _ | Goto _ -> None
  | Call call -> Some call

let callee call prog = match Call.target call with
  | Indirect _ -> None
  | Direct tid -> Term.find sub_t prog tid

let require x = Option.some_if x ()

let def_of_arg arg =
  let x = Arg.lhs arg in
  let e = Arg.rhs arg in
  Some (Def.create x e)

let intent_matches x y = match Arg.intent x with
  | None -> true
  | Some x -> match x,y with
    | In,In | Out,Out -> true
    | Both,_| _,Both -> true
    | _ -> false

let transfer_attr attr t1 t2 =
  match Term.get_attr t1 attr with
  | None -> t2
  | Some v -> Term.set_attr t2 attr v

let transfer_attrs t1 t2 =
  let t2 = Term.set_attr t2 Term.synthetic () in
  Term.set_attr t2 Term.origin (Term.tid t1) |>
  transfer_attr Disasm.insn t1 |>
  transfer_attr address t1

let add_def intent blk def =
  if intent = Out
  then Term.prepend def_t blk def
  else Term.append  def_t blk def

let defs_of_args call intent args : def term seq =
  Seq.filter_map args ~f:(fun arg ->
      require (intent_matches arg intent) >>= fun () ->
      def_of_arg arg >>| transfer_attrs call)

let target intent sub blk call =
  if intent = Out
  then Call.return call >>= function
    | Direct tid -> Term.find blk_t sub tid
    | _ -> None
  else Some blk

let insert_defs prog sub =
  let blk_with_def intent blk jmp sub : blk term option =
    call_of_jmp jmp >>= fun caller ->
    callee caller prog >>= fun callee ->
    target intent sub blk caller >>| fun blk ->
    Term.enum arg_t callee |>
    defs_of_args jmp intent |>
    Seq.fold ~init:blk ~f:(add_def intent) in
  let insert intent blk jmp sub =
    Option.value_map (blk_with_def intent blk jmp sub)
      ~default:sub ~f:(Term.update blk_t sub) in
  List.fold [In;Out] ~init:sub ~f:(fun sub intent ->
      Term.enum blk_t sub |> Seq.fold ~init:sub ~f:(fun sub blk ->
          Term.enum jmp_t blk |> Seq.fold ~init:sub ~f:(fun sub jmp ->
              insert intent blk jmp sub)))

let fill_calls program =
  Term.map sub_t program ~f:(insert_defs program)


let main proj =
  let prog = Project.program proj in
  Project.with_program proj (fill_calls prog)

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "This pass will inject artifical definitions of a subroutine
      arguments at call sites. Consider function $(b,malloc) that has
      the following declaration in BIR:";
    `Pre "
      sub malloc(malloc_size, malloc_result)
      malloc_size :: in u32 = R0
      malloc_result :: out u32 = R0";
    `P "This plugin will add two definitions, one just before the call
    to the malloc:";
    `Pre "
      ...
      000001c3: malloc_size := R0
      0000015b: call @malloc with return %0000015c";
    `P "And prepend another to the block to which malloc will return:";
    `Pre "
      0000015c:
      000001c4: R0 := malloc_result
      ...";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-api)(1)"
  ];
  Config.when_ready (fun _ -> Project.register_pass ~deps:["abi"] main)
