open Core_kernel[@@warning "-D"]
open Bap_core_theory
open Bap_main
open Bap_demangle.Std

let doc = "
# DESCRIPTION

Performs ABI-specific name resolution and demangling.
"

let provides = [
  "symbolizer";
  "demangling";
  "demangler";
  "symbolizer";
]

let override = Extension.Configuration.parameter
    ~doc:"Overrides the default name demangler with the specified one"
    Extension.Type.(some string) "override"
    ~aliases:["with"]


module Internal = struct
  let maybe_mangled name =
    String.length name > 2 &&
    Char.(name.[0] = '_') &&
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
      let str = Bytes.of_string str in
      let n = Substring.create ~pos:s1_p0 ~len str |>
              Substring.to_string |> Int.of_string in
      pos_ref := s1_p0 + len;
      Some n in
    let extract_name pos_ref =
      let str = Bytes.of_string str in
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

  let run name =
    Option.value_map ~default:name ~f:Fn.id (demangle_internal name)

  let install () =
    Demangler.declare ~package:"bap" "internal" run
end

let decide_name_from_possible_name ?override () : unit =
  let open KB.Syntax in
  KB.Rule.(declare ~package:"core" "name-of-possible-names" |>
           require Theory.Label.possible_name |>
           provide Theory.Label.name |>
           comment "resolves and demangles symbol's name");
  KB.promise Theory.Label.name @@ fun lbl ->
  let* target = Theory.Label.target lbl in
  let demangler = match override with
    | None -> Demanglers.select target
    | Some name -> Demanglers.get ~package:"bap" name in
  let+ name = KB.resolve Theory.Label.possible_name lbl in
  Option.map name ~f:(Demangler.run demangler)


let () = Extension.declare ~provides ~doc @@ fun ctxt ->
  let override = Extension.Configuration.get ctxt override in
  decide_name_from_possible_name ?override ();
  Internal.install ();
  Ok ()
