open Core_kernel.Std
open Bap_bundle.Std
open Bap.Std
include Self()
open Cabs

type arg_size =
  | Word                        (** same size as CPU word  *)
  | Size of Size.t              (** the specified size     *)

type pos =
  | Ret_0
  | Ret_1
  | Arg of int

type arg = {
  arg_name : string;
  arg_pos  : pos;
  arg_intent : intent option;
  arg_size : arg_size;
}

type fn_proto = arg list

exception Failed_to_parse of string

include struct
  let stack mem sp endian sz off =
    let width = Size.in_bits sz in
    let off = Word.of_int ~width (off * (Size.in_bytes sz)) in
    let mem = Bil.var mem in
    let addr = if Word.is_zero off
      then Bil.(var sp)
      else Bil.(var sp + int off) in
    Bil.load ~mem ~addr endian sz

  module IA32  = X86_cpu.IA32
  module AMD64 = X86_cpu.AMD64
  module ARM   = ARM.CPU

  let arm_stack = ARM.(stack mem sp LittleEndian `r32)
  let x86_stack = IA32.(stack mem sp LittleEndian `r32)
  let x64_stack = AMD64.(stack mem sp LittleEndian `r64)

  open Bil
  let abi = function
    | #Arch.arm ->
      ARM.(function
          | Ret_0 -> var r0
          | Ret_1 -> var r1
          | Arg 0 -> var r0
          | Arg 1 -> var r1
          | Arg 2 -> var r2
          | Arg 3 -> var r3
          | Arg n -> arm_stack Int.(n-4))
    | `x86_64 ->
      AMD64.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg 0 -> var rdi
          | Arg 1 -> var rsi
          | Arg 2 -> var rdx
          | Arg 3 -> var rcx
          | Arg 4 -> var r.(0)
          | Arg 5 -> var r.(1)
          | Arg n -> x64_stack Int.(n-6))
    | `x86 ->
      IA32.(function
          | Ret_0 -> var rax
          | Ret_1 -> var rdx
          | Arg n -> x86_stack Int.(n+1))
    | _ -> raise Not_found
end

let intent_of_type = function
  | PTR (CONST _) -> Some In
  | PTR (_) -> Some Both
  | _ -> Some In

let size_of_type typ = match typ with
  | PTR _ -> Word
  | CHAR _ -> Size `r8
  | INT (SHORT,_) -> Size `r16
  | INT (LONG_LONG,_) -> Size `r64
  | FLOAT _ -> Size `r32
  | DOUBLE false -> Size `r64
  | DOUBLE true -> Size `r128
  | _ -> Word

let arg_of_name n = function
  | (_,VOID,_,_) -> None
  | (name,typ,_,_) -> Some {
      arg_pos = Arg n;
      arg_name = if name <> "" then name else sprintf "x%d" (n+1);
      arg_intent = intent_of_type typ;
      arg_size = size_of_type typ;
    }

let string_of_single_name n (_,_,name) = arg_of_name n name

let args_of_single_names = List.filter_mapi ~f:string_of_single_name

let ret_word n = {
  arg_pos = n;
  arg_name = if n = Ret_1 then "result_ext" else "result";
  arg_intent = Some Out;
  arg_size = Word; (* compiler will cast return value itself *)
}

let push = List.map ~f:(fun a -> match a with
    | {arg_pos = Arg n} -> {a with arg_pos = Arg (n+1)}
    | a -> a)

let fn_of_definition = function
  | DECDEF (_,_,[(name, PROTO (ret,args,false),[],NOTHING)])
  | FUNDEF ((_,_,(name, PROTO (ret,args,false),[], NOTHING)), _) ->
    let args = args_of_single_names args in
    let args = match ret with
      | VOID -> args
      | STRUCT _ | CONST (STRUCT _) ->
        {(ret_word (Arg 0)) with arg_intent = Some In} :: push args
      | INT (LONG_LONG,_) -> args @ [ret_word Ret_0; ret_word Ret_1]
      | _ -> args @ [ret_word Ret_0] in
    Some (name,args)
  | _ -> None

let fns_of_definitions =
  List.fold ~init:String.Map.empty ~f:(fun fns defn ->
      match fn_of_definition defn with
      | None -> fns
      | Some (name,data) -> Map.add fns ~key:name ~data)

let args_of_file file =
  let open Frontc in
  match Frontc.parse_file file stderr with
  | PARSING_ERROR -> raise Parsing.Parse_error
  | PARSING_OK ds -> fns_of_definitions ds


let merge_args =
  Map.merge ~f:(fun ~key (`Left a |`Right a |`Both (_,a)) -> Some a)

let is_api name =
  match String.split ~on:'/' name with
  | ["api"; _] -> true
  | _ -> false

let args_of_bundle bundle =
  Bundle.list bundle |> List.filter ~f:is_api |>
  List.fold ~init:String.Map.empty ~f:(fun args file ->
      let name = Filename.temp_file "api" ".h" in
      match Bundle.get_file ~name bundle (Uri.of_string file) with
      | None -> args
      | Some uri ->
        let dst = Uri.path uri in
        let finally () = Sys.remove dst in
        let args' = protect ~f:(fun () -> args_of_file dst) ~finally in
        merge_args args args')

let (>:) s1 s2 e =
  match Size.(in_bits s2 - in_bits s1) with
  | 0 -> e
  | n when n > 0 -> Bil.(cast high n e)
  | n -> Bil.(cast low n e)

let term_of_arg arch sub {arg_name; arg_size; arg_intent; arg_pos} =
  let word_size = Arch.addr_size arch in
  let size = match arg_size with
    | Word -> (word_size :> Size.t)
    | Size size -> size in
  let typ = Type.imm (Size.in_bits size) in
  let exp = try abi arch arg_pos with
      exn -> Bil.unknown "unkown abi" typ in
  (* so far we assume, that [abi] returns expressions of word size,
     if this will ever change, then we need to extend abi function
     to return the size for us. *)
  let exp = (word_size >: size) exp in
  let var = Var.create (Sub.name sub ^ "_" ^ arg_name) typ in
  Arg.create ?intent:arg_intent var exp

let fill_args arch fns program =
  Term.map sub_t program ~f:(fun sub ->
      match Map.find fns (Sub.name sub) with
      | None -> sub
      | Some args ->
        List.fold args ~init:sub ~f:(fun sub arg ->
            Term.append arg_t sub (term_of_arg arch sub arg)))

let main bundle proj =
  let prog = Project.program proj in
  let arch = Project.arch proj in
  let args = args_of_bundle bundle in
  fill_args arch args prog |>
  Project.with_program proj

module Cmdline = struct
  include Cmdliner
  let headers : string list Term.t =
    let doc = "C header with function prototypes" in
    Arg.(value & opt (list file) [] & info ["add"] ~doc)

  let parse argv =
    let info = Term.info ~doc name in
    let spec = Term.(pure ident $headers) in
    match Term.eval ~argv (spec,info) with
    | `Ok res -> res
    | `Error err -> exit 1
    | `Version | `Help -> exit 0
end

let insert_headers bundle file =
  let name = "api/" ^ Filename.basename file in
  Bundle.insert_file ~name bundle (Uri.of_string file)

let () =
  let bundle = main_bundle () in
  List.iter (Cmdline.parse argv) ~f:(insert_headers bundle);
  Project.register_pass ~autorun:true (main bundle)
