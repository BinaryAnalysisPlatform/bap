open Core_kernel.Std
open Bap.Std
open Format

let demangle_native str =
  let open String in
  let open Option.Monad_infix in
  let extract_number pos_ref =
    lfindi str ~pos:!pos_ref ~f:(fun _ c -> Char.is_digit c)
    >>= fun s1_p0 ->
    lfindi str ~pos:s1_p0 ~f:(fun _ c -> not (Char.is_digit c))
    >>= fun s1_p1 ->
    let len = (s1_p1 - s1_p0) in
    let n = Substring.create ~pos:s1_p0 ~len str |>
            Substring.to_string |> Int.of_string in
    pos_ref := s1_p0 + len;
    Some n in
  let extract_name pos_ref =
    extract_number pos_ref >>= fun len ->
    let name = Substring.create ~pos:!pos_ref ~len str |>
               Substring.to_string in
    pos_ref := !pos_ref + len;
    Some name in
  let pos = ref 0 in
  let rec extract_names acc =
    match extract_name pos with
    | None | Some "" -> List.rev acc
    | Some name -> extract_names (name::acc) in
  match extract_names [] |> String.concat ~sep:"__" with
  | "" -> str
  | s  -> s

let demangle_external ?(prog="c++filt") name =
  let command = sprintf "%s %s" prog name in
  let inp = Unix.open_process_in command in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let maybe_mangled name =
  String.length name > 2 &&
  name.[0] = '_' &&
  Char.is_uppercase name.[1] &&
  Char.is_alpha name.[1]

let demangle_name ?(how=`internal) name =
  if maybe_mangled name then
    match how with
    | `program prog -> demangle_external ~prog name
    | `internal -> demangle_native name
  else name

let read ?demangle ~filename base  : string table =
  let demangle name = match demangle with
    | None -> name
    | Some how -> demangle_name ~how name in
  let sym_of_sexp x = <:of_sexp<string * int64 * int64>> x in
  In_channel.with_file filename ~f:(fun out ->
      let buf = Lexing.from_channel out in
      Sexp.scan_fold_sexps buf ~init:Table.empty ~f:(fun syms sexp ->
          try
            let (name,es,ef) = sym_of_sexp sexp in
            let words = Int64.(ef - es |> to_int_exn) in
            let from = Addr.of_int64 ~width:32 es in
            let mem = Memory.view ~from ~words base |> ok_exn in
            let name = demangle name in
            Table.add syms mem name |> ok_exn
          with exn -> syms))
