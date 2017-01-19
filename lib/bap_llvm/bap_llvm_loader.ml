open Core_kernel.Std
open Bap.Std
open Bap_llvm_types

include Self()

module Binary = Bap_llvm_binary

let make_addr arch addr =
  match Arch.addr_size arch with
  | `r32 -> Addr.of_int64 ~width:32 addr
  | `r64 -> Addr.of_int64 ~width:64 addr

let to_segment arch s : Backend.Segment.t option =
  let module S = Segment in
  let name = S.name s in
  let perm = Backend.([
      S.is_readable s, R;
      S.is_writable s, W;
      S.is_executable s, X ] |>
      List.filter_map ~f:(fun (e, p) -> if e then Some p else None) |>
      List.reduce ~f:(fun p1 p2 -> Or (p1, p2))) in
  let off = S.offset s |> Int64.to_int_exn in
  let location = Location.Fields.create
      ~addr:(S.addr s |> make_addr arch)
      ~len:(S.size s |> Int64.to_int_exn) in
  match perm with
  | Some perm when S.size s <> Int64.zero ->
    Backend.Segment.Fields.create ~name ~perm ~off ~location |>
    Option.some
  | _ -> None

let to_symbol arch s : Backend.Symbol.t option =
  let module S = Symbol in
  let name = S.name s in
  let is_function = S.kind s = S.Function in
  let is_debug = S.kind s = S.Debug in
  let locations = Location.Fields.create
      ~addr:(S.addr s |> make_addr arch)
      ~len:(S.size s |> Int64.to_int_exn), [] in
  if S.size s <> Int64.zero then
    Backend.Symbol.Fields.create ~name ~is_function ~is_debug ~locations |>
    Option.some
  else None

let to_section arch s : Backend.Section.t =
  let module S = Section in
  let name = S.name s in
  let location = Location.Fields.create
      ~addr:(S.addr s |> make_addr arch)
      ~len:(S.size s |> Int64.to_int_exn) in
  Backend.Section.Fields.create ~name ~location

let of_data data : Backend.Img.t option =
  let of_data_exn () =
    let b = Binary.create data in
    let arch = Binary.arch b |>
               Arch.of_string |>
               (fun v -> Option.value_exn v) in
    let entry = Binary.entry b |> make_addr arch in
    let segments =
      Binary.segments b |> List.filter_map ~f:(to_segment arch) |>
      (fun s -> List.hd_exn s, List.tl_exn s) in
    let symbols =
      Binary.symbols b |> List.filter_map ~f:(to_symbol arch) in
    let sections =
      Binary.sections b |> List.map ~f:(to_section arch) in
    Backend.Img.Fields.create ~arch ~entry ~segments ~symbols ~sections in
  try Some (of_data_exn ()) with
  | exn -> Format.eprintf "Can't create binary: %a%!" Exn.pp exn; None

let init () =
  match Image.register_backend ~name of_data with
  | `Ok -> Ok ()
  | `Duplicate ->
    Error
      (Error.of_string
         (sprintf "llvm loader: name «%s» is already used\n" name))
