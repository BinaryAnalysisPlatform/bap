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
  let is_unit u = String.(u <> package) && begin
      List.is_empty units ||
      List.mem units u ~equal:String.equal
    end in
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
            Program.KB.lift symtab >>= fun prog ->
            let cg = lazy (Program.to_graph prog) in
            let init = String.Map.(empty, empty) in
            Term.enum sub_t prog |>
            KB.Seq.fold ~init ~f:(fun (subs, aliases) sub ->
                let subs = Map.add_exn subs ~key:(Sub.name sub) ~data:sub in
                match Term.get_attr sub address with
                | None -> !!(subs, aliases)
                | Some addr ->
                  let addr = Word.to_bitvec addr in
                  Theory.Label.for_addr addr >>=
                  KB.collect Theory.Label.aliases >>| fun a ->
                  let aliases =
                    Set.fold a ~init:aliases ~f:(fun m key ->
                        Map.add_multi m ~key ~data:sub) in
                  subs, aliases) >>| fun (subs, aliases) ->
            (path, subs, aliases, cg) :: exts)
  end;
  let result = Toplevel.get result in
  Toplevel.exec @@ KB.Symbol.set_package package;
  result

let find_sub_in_ext name (path, subs, aliases, _) =
  match Map.find subs name with
  | Some sub -> Some sub
  | None -> match Map.find aliases name with
    | None -> None
    | Some [sub] -> Some sub
    | Some _ ->
      warning "Stub %s was found in unit %s, but has more \
               than one implementation; skipping%!" name path;
      None

let resolve_stub exts stub =
  let name = Sub.name stub in
  List.filter_map exts ~f:(fun ((path, _, _, _) as ext) -> Option.(
      find_sub_in_ext name ext >>= fun sub ->
      let is_stub = Term.has_attr sub Sub.stub in
      if is_stub then
        warning "Stub %s was found in unit %s, but still has the stub \
                 attribute; skipping%!" name path;
      some_if (not is_stub) (Term.tid sub, Sub.name sub, path))) |> function
  | [] ->
    warning "Stub %s was not resolved, couldn't find suitable unit \
             containing this symbol%!" name;
    None
  | [tid, name', path] ->
    info "Stub %s was resolved in unit %s (as %s)%!" name path name';
    Some (tid, path)
  | x ->
    let units = List.map x ~f:(fun (_, name, path) ->
        Format.sprintf "(%s, %s)" name path) in
    failwithf "Stub %s was not resolved, multiple units were \
               found containing this symbol: %s%!"
      name (String.concat units ~sep:", ") ()

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

let minimal_slice tids cg =
  let module N = Graphs.Callgraph.Node in
  let stack = Stack.of_list tids in
  let add acc s =
    Seq.iter s ~f:(fun tid ->
        if not @@ Set.mem acc tid then
          Stack.push stack tid) in
  let rec build acc = match Stack.pop stack with
    | None -> acc
    | Some tid ->
      let acc = Set.add acc tid in
      add acc @@ N.preds tid cg;
      add acc @@ N.succs tid cg;
      build acc in
  build Tid.Set.empty

let link_units tids exts to_link all prog =
  let find_ext p =
    List.find_exn exts ~f:(fun (path, _, _, _) ->
        String.(p = path)) in
  Set.fold to_link ~init:prog ~f:(fun prog path ->
      let _, subs, _, cg = find_ext path in
      if all then
        Map.fold subs ~init:prog ~f:(fun ~key:_ ~data:sub prog ->
            Term.append sub_t prog sub)
      else
        let tids = minimal_slice (Map.find_exn tids path) (Lazy.force cg) in
        Map.fold subs ~init:prog ~f:(fun ~key:_ ~data:sub prog ->
            if Set.mem tids @@ Term.tid sub then
              Term.append sub_t prog sub
            else prog))

let remove_stubs replace prog =
  Term.filter sub_t prog ~f:(fun sub ->
      not @@ Map.mem replace @@ Term.tid sub)

let resolve proj package stubs units all =
  let prog = Project.program proj in
  let target = Project.target proj in
  let exts = collect_externals package target units in
  let replace, tids, to_link =
    let init = Tid.Map.empty, String.Map.empty, String.Set.empty in
    Term.enum sub_t prog |>
    Seq.fold ~init ~f:(fun ((r, t, l) as acc) sub ->
        let tid = Term.tid sub in
        if Set.mem stubs tid then
          match resolve_stub exts sub with
          | None -> acc
          | Some (new_tid, path) ->
            Map.set r ~key:tid ~data:new_tid,
            Map.add_multi t ~key:path ~data:new_tid,
            Set.add l path
        else acc) in
  replace_calls_to_stub prog replace |>
  link_units tids exts to_link all |>
  remove_stubs replace |>
  Project.with_program proj

let post proj =
  Project.passes () |>
  List.filter ~f:Project.Pass.autorun |>
  List.fold ~init:proj ~f:(Fn.flip Project.Pass.run_exn)

let main units all proj =
  let package = current_package () in
  let stubs = stubs proj package in
  if Set.is_empty stubs then proj
  else resolve proj package stubs units all |> post

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
               All subroutines that are reachable and/or reached from this \
               subroutine are then merged into the main program; e.g. the \
               minimal set of subroutines will be linked in according to \
               the callgraph.");
  ]

let units =
  let doc = "The specific units to link with. If none are specified then \
             all available units will be searched." in
  Config.(param (list string) ~default:[] ~doc "units")

let all =
  let doc = "If the implementation of a stub is found in a particular unit, \
             then the entire unit will be linked in." in
  Config.flag ~doc "all"

let () =
  Config.when_ready @@ fun {Config.get = (!)} ->
  Project.register_pass @@ main !units !all
