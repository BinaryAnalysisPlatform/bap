open Core_kernel.Std

let maybe_mangled name =
  String.length name > 2 &&
  name.[0] = '_' &&
  Char.is_uppercase name.[1] &&
  Char.is_alpha name.[1]

let demangle_internal str =
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
  match extract_names [] |> String.concat ~sep:"::" with
  | "" -> str
  | s  -> s

let demangle_internal name =
  if maybe_mangled name then
    Option.try_with (fun () -> demangle_internal name)
  else None

let run_internal name =
  Option.value_map ~default:name ~f:ident (demangle_internal name)

module Std = struct
  type demangler = {
    name : string;
    run : string -> string
  }

  module Demangler = struct
    type t = demangler
    let create name run = {name; run}
    let run d = d.run
    let name d = d.name
  end

  module Demanglers = struct
    let demanglers = ref []
    let register d = demanglers := d :: !demanglers
    let available () = !demanglers
  end

  let internal = {
    name = "internal";
    run = run_internal;
  }

  let () = Demanglers.register internal
end
