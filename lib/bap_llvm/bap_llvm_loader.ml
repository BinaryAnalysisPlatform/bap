open Core_kernel.Std
open Bap.Std
open Monads.Std

open Bap_llvm_binary

let make_addr arch addr =
  match Arch.addr_size arch with
  | `r32 -> Addr.of_int64 ~width:32 addr
  | `r64 -> Addr.of_int64 ~width:64 addr

let make_segment arch bin ind =
  let name = segment_name bin ind in
  let perm = Backend.([
      segment_is_readable bin ind, R;
      segment_is_writable bin ind, W;
      segment_is_executable bin ind, X ] |>
      List.filter_map ~f:(fun (e, p) -> if e then Some p else None) |>
      List.reduce ~f:(fun p1 p2 -> Or (p1, p2))) in
  let off = segment_offset bin ind |> Int64.to_int_exn in
  let size = segment_size bin ind in
  let location = Location.Fields.create
      ~addr:(segment_addr bin ind |> make_addr arch)
      ~len:(Int64.to_int_exn size) in
  match perm with
  | Some perm when size <> 0L ->
    Backend.Segment.Fields.create ~name ~perm ~off ~location |>
    Option.some
  | _ -> None

let make_symbol arch bin ind =
  let name = symbol_name bin ind in
  let is_function = symbol_is_fun bin ind in
  let is_debug = symbol_is_debug bin ind in
  let addr = symbol_addr bin ind in
  let size = symbol_size bin ind in
  let locations = Location.Fields.create
      ~addr:(make_addr arch addr)
      ~len:(Int64.to_int_exn size), [] in
  if size <> 0L then
    Backend.Symbol.Fields.create ~name ~is_function ~is_debug ~locations |>
    Option.some
  else None

let make_section arch bin ind =
  let name = section_name bin ind in
  let addr = section_addr bin ind in
  let size = section_size bin ind in
  let location = Location.Fields.create
      ~addr:(make_addr arch addr)
      ~len:(Int64.to_int_exn size) in
  Some (Backend.Section.Fields.create ~name ~location)

let collection arch bin (f,max) =
  List.init (max bin) ~f:(f arch bin) |>
  List.filter_opt

let nonempty = function
  | [] -> None
  | hd :: tl -> Some (hd, tl)

let segment = make_segment, segments_number
let section = make_section, sections_number
let symbol  = make_symbol, symbols_number

let with_err_message msg f x = match f x with
  | None -> eprintf "%s\n" msg; None
  | r -> r

let arch_of_string arch =
  with_err_message
    (sprintf "unknown arch %s\n" arch) Arch.of_string arch

let collect_segments arch bin =
  collection arch bin segment |>
  with_err_message "segments list is empty" nonempty

let create_binary data =
  with_err_message "create binary failed" create_binary data

let of_data data : Backend.Img.t option =
  let open Option in
  create_binary data >>= fun bin ->
  arch_of_string (image_arch bin) >>= fun arch ->
  collect_segments arch bin >>= fun segments ->
  let entry = make_addr arch (image_entry bin) in
  let symbols = collection arch bin symbol in
  let sections = collection arch bin section in
  Some (Backend.Img.Fields.create
          ~arch ~entry ~segments ~symbols ~sections)

let init () =
  match Image.register_backend ~name:"legacy-llvm" of_data with
  | `Ok -> Ok ()
  | `Duplicate -> Or_error.errorf "legacy-llvm loader: duplicate name"
