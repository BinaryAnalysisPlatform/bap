open Core_kernel.Std
open Bap_types.Std
open Image_backend
open Image_common

let make_addr arch addr =
  match Arch.addr_size arch with
  | `r32 -> Addr.of_int64 ~width:32 addr
  | `r64 -> Addr.of_int64 ~width:64 addr

let to_section arch s : Section.t =
  let module S = Llvm_binary_segment in
  let name = S.name s in
  let perm =
    let p = [ S.is_readable s, R;
              S.is_writable s, W;
              S.is_executable s, X ] in
    List.filter_map ~f:(fun (e, p) -> if e then Some p else None) p |>
    List.reduce_exn ~f:(fun p1 p2 -> Or (p1, p2)) in
  let off = S.offset s |> Int64.to_int_exn in
  let location = Location.Fields.create
      ~addr:(S.addr s |> make_addr arch)
      ~len:(S.size s |> Int64.to_int_exn) in
  Section.Fields.create ~name ~perm ~off ~location

let to_sym arch s : Sym.t =
  let module S = Llvm_binary_symbol in
  let name = S.name s in
  let is_function, is_debug =
    let open S in
    (match kind s with Function -> true | _ -> false),
    (match kind s with Debug -> true | _ -> false) in
  let locations =
    let l = Location.Fields.create
        ~addr:(S.addr s |> make_addr arch)
        ~len:(S.size s |> Int64.to_int_exn) in
    l, [] in
  Sym.Fields.create ~name ~is_function ~is_debug ~locations

let to_tag arch s : Tag.t =
  let module S = Llvm_binary_section in
  let name = "section" in
  let data = S.name s in
  let location = Location.Fields.create
                   ~addr:(S.addr s |> make_addr arch)
                   ~len:(S.size s |> Int64.to_int_exn) in
  Tag.Fields.create ~name ~data ~location

let from_data data : Img.t option =
  let from_data_exn () =
    let module B = Llvm_binary in
    let b = B.create data in
    let arch = B.arch b in
    let entry = B.entry b |> make_addr arch in
    let sections =
      B.segments b |>
      List.map ~f:(to_section arch) |>
      (fun s -> List.hd_exn s, List.tl_exn s) in
    let symbols =
      B.symbols b |>
      List.map ~f:(to_sym arch) in
    let tags =
      B.sections b |>
      List.map ~f:(to_tag arch) in
    Img.Fields.create ~arch ~entry ~sections ~symbols ~tags in
  try from_data_exn () |> Option.some
  with exn -> None


let from_file path =
  Bap_fileutils.readfile path |>
  from_data
