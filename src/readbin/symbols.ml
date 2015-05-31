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
  match extract_names [] |> String.concat ~sep:"." with
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

let demangle ?tool name =
  if maybe_mangled name then
    match tool with
    | Some prog -> demangle_external ~prog name
    | None -> demangle_native name
  else name

let read arch ic : (string * addr * addr) list =
  let sym_of_sexp x = <:of_sexp<string * int64 * int64>> x in
  let addr_of_int64 x =
    let width = Arch.addr_size arch |> Size.to_bits in
    Addr.of_int64 ~width x in
  List.(Sexp.input_sexps ic >>| sym_of_sexp >>| (fun (s, es, ef) ->
      s, addr_of_int64 es, addr_of_int64 ef))

let read_addrs ic : addr list =
  List.t_of_sexp Addr.t_of_sexp @@ Sexp.input_sexp ic

let write_addrs oc (addrs : addr list) : unit =
  Sexp.output oc @@ List.sexp_of_t Addr.sexp_of_t addrs

let write oc (syms : (string * addr * addr) list) : unit =
  let sexp_of_sym x = <:sexp_of<string * int64 * int64>> x in
  try
    let syms = List.map syms ~f:(fun (s, es, ef) -> s, Addr.to_int64 es |> ok_exn,
                                                    Addr.to_int64 ef |> ok_exn) in
    List.iter syms ~f:(fun sym -> Sexp.output_hum oc @@ sexp_of_sym sym;
                        output_char oc '\n')
  with exn ->
    printf "Output error: %a." Exn.pp exn;
    ()
