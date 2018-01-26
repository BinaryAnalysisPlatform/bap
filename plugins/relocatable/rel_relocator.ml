open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Image
open Monads.Std

include Self ()

module Fact = Ogre.Make(Monad.Ident)

let of_aseq width x =
  Seq.fold x ~init:Addr.Map.empty ~f:(fun m (key,data) ->
      let key = Addr.of_int64 ~width key in
      Map.add m ~key ~data)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let relocations =
    Fact.collect Ogre.Query.(select (from relocation))

  let external_symbols  =
    Fact.collect Ogre.Query.(
        select (from external_reference))

  let relocations =
    addr_width >>= fun width ->
    relocations >>= fun rels ->
    external_symbols >>= fun ext ->
    Fact.return (of_aseq width rels, of_aseq width ext)

end

(** TODO: using stub is wrong!!!  *)
let get addr data =
  let stub = Addr.of_int ~width:64 3 in
  let rec find addr =
    if Addr.(addr > addr + stub) then None
    else
      match Map.find data addr with
      | None -> find (Addr.succ addr)
      | Some value -> Some value in
  find addr

let synthetic_sub () =
  let s = Sub.create () in
  Term.(set_attr s synthetic ())

let spec = ref None

let relink rels pr =
  let subs = Term.to_sequence sub_t pr in
  let find a =
    Seq.find subs ~f:(fun s ->
        match Term.get_attr s address with
        | None -> false
        | Some a' ->
          let a = Addr.of_int64 ~width:(Addr.bitwidth a') a in
          Addr.equal a a') in
  (object
    inherit Term.mapper
    method! map_jmp jmp =
      match Term.get_attr jmp address with
      | None -> jmp
      | Some addr ->
        match get addr rels with
        | None -> jmp
        | Some rel_addr ->
          printf "found %Lx %s\n" rel_addr (Addr.to_string addr);
          match Jmp.kind jmp with
          | Call call ->
            let return = Call.return call in
            let target = match find rel_addr with
              | None -> Call.target call
              | Some s ->
                printf " tids: found %s for %s\n"
                  (Tid.to_string (Term.tid s))
                  (Tid.to_string (Term.tid jmp));
                Direct (Term.tid s) in
            Jmp.with_kind jmp (Call (Call.create ?return ~target ()))
          | _ -> jmp
  end)#run pr

let main proj =
  match !spec with
  | None -> proj
  | Some spec ->
    match Fact.eval Rel.relocations spec with
    | Error er ->
      error "%a" Error.pp er;
      proj
    | Ok (rels, exts) ->
      let pr = Project.program proj in
      let pr =
        String.Set.of_list (Map.data exts) |>
        Set.to_list |>
        List.fold ~init:pr ~f:(fun pr name ->
            let sub = Sub.create ~name () in
            let sub = Term.(set_attr sub synthetic ()) in
            Term.append sub_t pr sub) in
      let pr = relink rels pr in
      Project.with_program proj pr

let init () =
  Stream.observe Project.Info.spec (fun s -> spec := Some s);
  Project.register_pass ~autorun:true main
