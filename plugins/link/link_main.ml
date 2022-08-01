open Bap.Std
open Bap_core_theory
open Core_kernel[@@warning "-D"]

include Self()

open KB.Syntax

let current_package () =
  let result = Toplevel.var "package" in
  Toplevel.put result KB.Symbol.package;
  Toplevel.get result

let is_intrinsic = String.is_prefix ~prefix:"intrinsic:"
let is_interrupt = String.is_prefix ~prefix:"interrupt:"
let is_external  = String.is_suffix ~suffix:":external"
let is_tid       = String.is_prefix ~prefix:"%"

(* ARM-specific. *)
let is_svc =
  let re = Str.regexp {|__svc(0x[0-9A-F]+)|} in
  fun name -> Str.string_match re name 0

let should_ignore s =
  is_intrinsic s ||
  is_interrupt s ||
  is_external  s ||
  is_tid       s ||
  is_svc       s

let stubs proj package =
  Project.program proj |> Term.enum sub_t |>
  Seq.filter_map ~f:(fun sub ->
      if Term.has_attr sub Sub.stub then
        let name = Sub.name sub in
        if should_ignore name then None
        else begin
          info "Found stub %s in %s%!" name package;
          Some (Term.tid sub)
        end
      else None) |> Tid.Set.of_sequence

let collect_externals package target units =
  let is_unit u =
    String.(u <> package) &&
    match units with
    | [] -> true
    | _ -> List.mem units u ~equal:String.equal in
  let result = Toplevel.var "syms" in
  Toplevel.put result begin
    KB.objects Theory.Unit.cls >>=
    KB.Seq.fold ~init:[] ~f:(fun exts unit ->
        KB.collect Theory.Unit.target unit >>= fun target' ->
        if Theory.Target.(target <> target') then !!exts
        else KB.collect Theory.Unit.path unit >>= function
          | None ->
            warning "Unit for target %a with no path; skipping%!"
              Theory.Target.pp target;
            !!exts
          | Some path when not (is_unit path) -> !!exts
          | Some path ->
            KB.Symbol.set_package path >>= fun () ->
            let promise _ = !!(Some unit) in
            KB.promising Theory.Label.unit ~promise @@ fun () ->
            KB.collect Project.State.slot unit >>= fun state ->
            let disasm = Project.State.disassembly state in
            let calls = Project.State.subroutines state in
            info "Creating symtab for unit %s%!" path;
            Symtab.create disasm calls >>= fun symtab ->
            info "Lifting program for unit %s%!" path;
            Program.KB.lift symtab >>| fun prog ->
            let subs =
              Term.enum sub_t prog |>
              Seq.map ~f:(fun sub -> Sub.name sub, sub) |>
              String.Map.of_sequence_exn in
            (path, subs) :: exts)
  end;
  Toplevel.get result

let resolve_stub exts stub =
  let name = Sub.name stub in
  List.filter_map exts ~f:(fun (path, subs) -> Option.(
      Map.find subs name >>= fun sub ->
      let is_stub = Term.has_attr sub Sub.stub in
      if is_stub then
        warning "Stub %s was found in unit %s, but still has the stub \
                 attribute; skipping%!" name path;
      some_if (not is_stub) (Term.tid sub, path))) |> function
  | [] ->
    warning "Stub %s was not resolved, couldn't find suitable unit \
             containing this symbol%!" name;
    None
  | [x] ->
    info "Stub %s was resolved in unit %s%!" name (snd x);
    Some x
  | x ->
    let units = List.map x ~f:snd in
    failwithf "Stub %s was not resolved, multiple units were \
               found containing this symbol: %s%!"
      name (List.to_string ~f:Fn.id units) ()

let replace_calls_to_stub prog replace =
  Term.filter_map sub_t prog ~f:(fun sub ->
      if not @@ Map.mem replace @@ Term.tid sub then
        Option.some @@ Term.map blk_t sub ~f:(fun blk ->
            Term.map jmp_t blk ~f:(fun jmp ->
                match Jmp.alt jmp with
                | None -> jmp
                | Some alt -> match Jmp.resolve alt with
                  | First tid ->
                    Map.find replace tid |>
                    Option.value_map ~default:jmp ~f:(fun new_tid ->
                        Jmp.(with_alt jmp @@ Some (resolved new_tid)))
                  | Second _ -> jmp))
      else None)

let resolve proj package stubs units =
  let prog = Project.program proj in
  let target = Project.target proj in
  let exts = collect_externals package target units in
  Toplevel.exec @@ KB.Symbol.set_package package;
  let replace, to_link =
    let init = Tid.Map.empty, String.Set.empty in
    Term.enum sub_t prog |>
    Seq.fold ~init ~f:(fun ((r, l) as acc) sub ->
        let tid = Term.tid sub in
        if Set.mem stubs tid then
          match resolve_stub exts sub with
          | None -> acc
          | Some (new_tid, path) ->
            Tid.set_name new_tid @@ Sub.name sub;
            Map.set r ~key:tid ~data:new_tid, Set.add l path
        else acc) in
  let prog = replace_calls_to_stub prog replace in
  let find_ext p =
    List.find_exn exts ~f:(fun (path, _) ->
        String.(p = path)) in
  Set.fold to_link ~init:prog ~f:(fun prog path ->
      let _, subs = find_ext path in
      Map.fold subs ~init:prog ~f:(fun ~key:_ ~data:sub prog ->
          Term.append sub_t prog sub)) |>
  Project.with_program proj

let post proj =
  Project.passes () |>
  List.filter ~f:Project.Pass.autorun |>
  List.fold ~init:proj ~f:(Fn.flip Project.Pass.run_exn)

let main units proj =
  let package = current_package () in
  let stubs = stubs proj package in
  if Set.is_empty stubs then proj
  else
    let proj = resolve proj package stubs units in
    post proj

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "Resolves stub subroutines in the BIR program. It works as follows:";
    `I ("1.", "The program is searched for stub subroutines.");
    `I ("2.", "The available units in the Knowledge Base are collected.");
    `I ("3.", "Each unit is lifted into its own BIR program.");
    `I ("4.", "For each program, try to find subroutines that match the \
               names of our stubs.");
    `I ("5.", "If one and only one match is found, then all calls to the \
               stub are replaced with calls to the matching subroutine. \
               All subroutines from this unit are then merged into the \
               main program.");
  ]

let units =
  let doc = "The specific units to link with. If none are specified then \
             all available units will be searched." in
  Config.(param (list string) ~default:[] ~doc "units")

let () = Config.when_ready @@ fun {Config.get = (!)} ->
  Project.register_pass (main !units)
