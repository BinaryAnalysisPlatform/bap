let doc = "
Enables ad-hoc support for glibc runtime code. In particular it
detects the locations of $(b,main) and $(b,__libc_start_main)
functions (and adds the latter if it is absent).
"
open Core_kernel[@@warning "-D"]
open Bap_main
open Bap_core_theory
open Bap.Std
open Bap_c.Std

include Bap_main.Loggers()

let enable = Extension.Configuration.flag "enable"
    ~doc:"Override the glib detection heuristic and enable the \
          runtime fixup."

let references_glib_start_main doc =
  let open Ogre.Syntax in
  let query =
    Ogre.request Image.Scheme.external_reference
      ~that:(fun (_,name) -> Poly.(name = "__libc_start_main"))
    >>| Option.is_some in
  match Ogre.eval query doc with
  | Ok r -> r
  | Error err ->
    warning "unable to determine the presence of glibc runtime: %a"
      Error.pp err;
    false

let is_glibc proj = references_glib_start_main (Project.specification proj)

let find_by_name prog name =
  Term.enum sub_t prog |> Seq.find ~f:(fun sub -> String.equal (Sub.name sub) name)

let find_first_caller prog tid =
  Term.enum sub_t prog |> Seq.find_map ~f:(fun sub ->
      Term.enum blk_t sub |> Seq.find ~f:(fun blk ->
          Term.enum jmp_t blk |> Seq.exists ~f:(fun jmp ->
              match Jmp.kind jmp with
              | Call c -> Label.equal (Call.target c) (Direct tid)
              | _ -> false)))

let proj_int = function Bil.Int x -> Some x | _ -> None

let is_sub_exists prog name = Option.is_some @@ find_by_name prog name
let is_sub_absent prog name = not (is_sub_exists prog name)

let find_entry_point prog =
  Term.enum sub_t prog |>
  Seq.find ~f:(fun sub -> Term.has_attr sub Sub.entry_point)


let insert_call prog start entry jmp =
  let name = "__libc_start_main" in
  let tid = Tid.for_name name in
  let sub = Sub.create ~tid ~name () in
  let prog = Term.append sub_t prog sub in
  let entry = Term.remove jmp_t entry (Term.tid jmp) in
  let call = Call.create (Direct (Term.tid sub)) () in
  let jmp = Jmp.create_call call in
  let entry = Term.prepend jmp_t entry jmp in
  let start = Term.update blk_t start entry in
  let prog = Term.update sub_t prog start in
  Some (Term.tid sub, prog)


let find_libc_start_main prog =
  let open Option.Monad_infix in
  find_entry_point prog >>= fun start ->
  Term.first blk_t start >>= fun entry ->
  Term.first jmp_t entry >>= fun jmp ->
  match Jmp.kind jmp with
  | Goto _ -> insert_call prog start entry jmp
  | Ret _ | Int _ -> None
  | Call call -> match Call.target call with
    | Direct tid -> Some (tid,prog)
    | Indirect _ ->
      insert_call prog start entry jmp

let detect_main_address prog =
  let open Option.Monad_infix in
  find_by_name prog "__libc_start_main" >>= fun start ->
  find_first_caller prog (Term.tid start) >>= fun entry ->
  Term.first arg_t start >>= fun arg ->
  let defs = Term.enum def_t ~rev:true entry in
  match Arg.rhs arg with
  | Bil.Var reg -> Seq.find defs ~f:(fun def ->
      Var.same (Def.lhs def) reg) >>| Def.rhs >>= proj_int
  | Bil.Load (_,addr,_,_) ->
    Seq.find_map defs ~f:(fun def -> match Def.rhs def with
        | Bil.Store (_,a,e,_,_) when Exp.equal addr a -> Some e
        | _ -> None) >>= proj_int
  | _ -> None


let reinsert_args_for_new_name ?(abi=Fn.id) sub name =
  List.fold ~init:sub ~f:(|>) [
    Term.filter arg_t ~f:(fun _ -> false);
    (fun sub -> Term.del_attr sub C.proto);
    (fun sub -> Sub.with_name sub name);
    abi
  ]

let rename_main abi prog =
  if is_sub_absent prog "main"
  then match detect_main_address prog with
    | None -> prog
    | Some addr ->
      info "the main subroutine address is %a" Addr.pp addr;
      Term.map sub_t prog ~f:(fun sub ->
          match Term.get_attr sub address with
          | Some a when Addr.equal addr a ->
            reinsert_args_for_new_name ~abi sub "main"
          | _ -> sub)
  else prog

let rename_libc_start_main prog =
  if is_sub_absent prog "__libc_start_main"
  then match find_libc_start_main prog with
    | None -> prog
    | Some (tid,prog) ->
      Term.change sub_t prog tid @@ function
      | None -> None
      | Some sub ->
        Option.some @@
        reinsert_args_for_new_name sub "__libc_start_main"
  else prog

module Main = struct
  let proto : C.Type.proto =
    match C.Type.function_
            ~return:(C.Type.basic `sint) [
            "argc", C.Type.basic `sint;
            "argv", C.Type.(pointer (pointer (basic `char)))
          ]
    with `Function {t=proto} -> proto
       | _ -> assert false

  let abi proj main =
    let t = Project.target proj in
    match C.Abi.lookup t with
    | None -> main
    | Some proc ->
      C.Abi.apply proc (C.Abi.model t) [] proto main
end

let fix_main proj =
  Project.map_program proj ~f:(rename_main (Main.abi proj))

let is_enabled ctxt proj =
  Extension.Configuration.get ctxt enable || is_glibc proj

let recover_main ctxt proj =
  if is_enabled ctxt proj
  then fix_main proj
  else proj

let discover_libc_start_main ctxt proj =
  if is_enabled ctxt proj
  then Project.map_program proj ~f:rename_libc_start_main
  else proj

let () = Extension.declare ~doc ~provides:["abi"; "api"] @@ fun ctxt ->
  Bap_abi.register_pass (discover_libc_start_main ctxt);
  Project.register_pass ~autorun:true ~deps:["api"] (recover_main ctxt);
  Ok ()
