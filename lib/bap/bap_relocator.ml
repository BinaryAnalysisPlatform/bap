open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_future.Std
open Bap_sema.Std
open Bap_disasm_std
open Monads.Std

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Bap_image.Scheme
  open Fact.Syntax

  let of_seq s ~fkey ~fdata =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key:(fkey key) ~data:(fdata data))

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let external_symbols  =
    Fact.collect Ogre.Query.(select (from external_reference))

  let externals =
    addr_width >>= fun width ->
    external_symbols >>= fun exts ->
    let to_addr = Addr.of_int64 ~width in
    Fact.return (of_seq exts ~fkey:to_addr ~fdata:ident)

end

let find_name externals min_addr max_addr =
  let rec get addr =
    if Addr.(addr > max_addr) then None
    else
      match Map.find externals addr with
      | None -> get (Addr.succ addr)
      | Some value -> Some value in
  get min_addr

let create_synthetic_sub name =
  let s = Sub.create ~name () in
  Tid.set_name (Term.tid s) name;
  Term.(set_attr s synthetic ())

let add_externals insns externals pr =
  let subs = String.Table.create () in
  let memory = Seq.fold insns ~init:Addr.Map.empty
      ~f:(fun mems (m,_) ->
          let min,max = Bap_memory.(min_addr m, max_addr m) in
          Map.add mems min max) in
  let get_sub name =
    Hashtbl.find_or_add subs name
      ~default:(fun () -> create_synthetic_sub name) in
  let fixup jmp jmp_to =
    match Jmp.kind jmp with
    | Call call ->
      let return = Call.return call in
      let target = Direct (Term.tid (get_sub jmp_to)) in
      Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
    | _ -> jmp in
  let (>>=) = Option.(>>=) in
  let program = (object
    inherit Term.mapper
    method! map_jmp jmp =
      Option.value ~default:jmp
        (Term.get_attr jmp address >>= fun addr ->
         Map.find memory addr >>= fun max_addr ->
         find_name externals addr max_addr >>= fun fix ->
         Some (fixup jmp fix))
  end)#run pr in
  let append_ext prg sub = Term.append sub_t prg sub in
  List.fold ~init:program ~f:append_ext (Hashtbl.data subs)

let run prog insns spec =
  match Fact.eval Rel.externals spec with
  | Error _ ->  prog
  | Ok exts -> add_externals insns exts prog
