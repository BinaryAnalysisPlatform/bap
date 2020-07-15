open Core_kernel
open Bap.Std
open Bap_future.Std
open Bap_core_theory
include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Provides lifter and ABI processor for ARM architecture.";
    `S "SEE ALSO";
    `P "$(b,bap-arm)(3)"
  ]

module ARM = struct
  open Format
  include ARM

  let pp_insn ppf (mem,insn) =
    fprintf ppf "%a: %s"
      Addr.pp_hex (Memory.min_addr mem)
      (Disasm_expert.Basic.Insn.asm insn)

  let lift mem insn =
    match lift mem insn with
    | Error err as failed ->
      warning "can't lift instruction %a - %a"
        pp_insn (mem,insn) Error.pp err;
      failed
    | Ok bil as ok -> match Type.check bil with
      | Ok () -> ok
      | Error te ->
        warning "BIL doesn't type check %a - %a"
          pp_insn (mem,insn) Type.Error.pp te;
        Error (Error.of_string "type error")
end

let symbol_values doc =
  let field = Ogre.Query.(select (from Image.Scheme.symbol_value)) in
  match Ogre.eval (Ogre.collect field) doc with
  | Ok syms -> syms
  | Error err -> error "the file specification is ill-formed: %a"
                   Error.pp err;
    failwith "broken file specification"



(* let compute_arch_from_symbol_table file spec =
 *   let open KB.Syntax in
 *   let init = Map.empty (module Bitvec_order) in
 *   let (>>=?) x f = x >>= function
 *     | None -> KB.return `unknown
 *     | Some x -> f x in
 *   let require_arm unit f =
 *     KB.collect Theory.Unit.target unit >>= fun t ->
 *     if Theory.Target.belongs Arm_target.parent t
 *     then f t
 *     else KB.return `unknown in
 *   let symbols =
 *     symbol_values spec |>
 *     Seq.fold ~init ~f:(fun symbols (addr,value) ->
 *         let arch = match Int64.(value land 1L) with
 *           | 0L -> `armv7
 *           | _ -> `thumbv7 in
 *         let addr = Bitvec.M32.int64 addr in
 *         Map.add_exn symbols addr arch) in
 *   KB.promise Arch.slot @@ fun label ->
 *   KB.collect Theory.Label.unit label >>=? fun unit ->
 *   require_arm unit @@ fun target ->
 *   KB.collect Theory.Unit.path unit >>= function
 *   | None -> KB.return arch
 *   | Some path when path <> file -> KB.return `unknown
 *   | Some _ ->
 *     KB.collect Theory.Label.addr label >>=? fun addr ->
 *     KB.return @@ match Map.find symbols addr with
 *     | Some arch -> arch
 *     | None ->
 *       if Map.is_empty symbols then arch else `unknown *)



let () =
  Config.when_ready @@ fun _ ->
  Arm_target.load ();
  List.iter Arch.all_of_arm ~f:(fun arch ->
      register_target (arch :> arch) (module ARM);
      Arm_gnueabi.setup ());
  (* let inputs = Stream.merge ~f:Tuple.T2.create
   *     Project.Info.file Project.Info.spec  in
   * Stream.observe inputs @@ fun (file,spec) ->
   * info "computing arch from symbol table";
   * compute_arch_from_symbol_table file spec *)
