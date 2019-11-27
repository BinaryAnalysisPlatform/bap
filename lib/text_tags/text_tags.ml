open Core_kernel
open Format

type mode = string
exception Unknown_mode of string

let expect_tag () =
  invalid_arg "Expect: tag := (<name> [<arg> <arg>...]) | <name>"


let expect_arg () = invalid_arg "Expect: arg := (<name> <val>)"

let is_quoted = String.is_prefix ~prefix:"\""

let string_of_stag = function
    | String_tag tag -> tag
    | _ -> invalid_arg "String tag expected"


module Html = struct
  open Sexp

  let mark_open_stag stag : string =
    match Sexp.of_string @@ string_of_stag stag with
    | List (Atom tag :: attrs) ->
      let attrs = List.map attrs ~f:(function
          | List [Atom name; Atom value] ->
            if is_quoted value
            then sprintf "%s=%s" name value
            else sprintf "%s=%S" name value
          | other -> expect_arg ()) |> String.concat ~sep:" " in
      sprintf "<%s %s>" tag attrs
    | Atom tag -> sprintf "<%s>" tag
    | _ -> expect_tag ()

  let mark_close_stag stag : string =
    match Sexp.of_string @@ string_of_stag stag with
    | List (Atom tag :: _) | Atom tag -> sprintf "</%s>" tag
    | _ -> expect_tag ()


  let print_open_stag fmt  _ =  pp_open_vbox fmt 1; pp_print_cut fmt ()
  let print_close_stag fmt _ =  pp_close_box fmt (); pp_print_cut fmt ()

  let install fmt =
    pp_set_mark_tags  fmt true;
    pp_set_print_tags fmt true;
    pp_set_formatter_stag_functions fmt {
      mark_open_stag;
      mark_close_stag;
      print_open_stag = print_open_stag fmt;
      print_close_stag = print_close_stag fmt;
    }
end

module Blocks = struct
  open Sexp

  let filter_attrs stag : string option =
    match Sexp.of_string @@ string_of_stag stag with
    | List (Atom _ :: attrs) ->
      List.filter_map attrs ~f:(function
          | List [Atom "title"; Atom v] -> Some (`T v)
          | List [Atom "id"; Atom v] -> Some (`I v)
          | List [_;_] -> None
          | _ -> expect_arg ()) |> (function
          | [] -> None
          | [`T v; _] | [_; `T v]
          | [`T v] | [`I v] -> Some v
          | _ -> None)
    | Atom _ -> None
    | _ -> expect_tag ()

  let mark_open_stag stag : string =
    filter_attrs stag |>
    Option.value_map ~default:"" ~f:(sprintf "begin(%s) ")

  let mark_close_stag stag : string =
    filter_attrs stag |>
    Option.value_map ~default:"" ~f:(sprintf "end(%s)")

  let print_open_stag fmt stag : unit =
    if Option.is_some (filter_attrs stag) then begin
      pp_open_vbox fmt 1;
      pp_print_cut fmt ();
    end

  let print_close_stag fmt stag =
    if Option.is_some (filter_attrs stag) then begin
      pp_close_box fmt ();
      pp_print_cut fmt ();
    end

  let install fmt =
    pp_set_mark_tags fmt true;
    pp_set_print_tags fmt true;
    pp_set_formatter_stag_functions fmt {
      mark_open_stag;
      mark_close_stag;
      print_open_stag = print_open_stag fmt;
      print_close_stag = print_close_stag fmt;
    }
end

module Attr = struct
  open Scanf

  let attrs = String.Hash_set.create ()
  let show = Hash_set.add attrs
  let hide = Hash_set.remove attrs
  let colorify = ref (Unix.isatty Unix.stdout)
  let print_colors enabled =
    colorify := enabled

  (* ideally, we should clean in a [print_close_tag] method,
     but, thanks to a bug #6769 in Format library
     (see OCaml Mantis), we need to hijack the newline method,
     and use this ugly reference  *)
  let clean = ref true

  let foreground tag = sscanf tag " .foreground %s@\n" ident
  let background tag = sscanf tag " .background %s@\n" ident

  let name_of_attr tag = sscanf tag " .%s %s@\n" (fun s _ -> s)

  let ascii_color tag =
    try Option.some @@ match name_of_attr tag with
      | "foreground" -> foreground tag
      | "background" -> background tag
      | _ -> invalid_arg "Expected foreground | background"
    with _ -> None

  let need_to_print tag =
    Hash_set.mem attrs (name_of_attr tag)

  let reset ppf =
    if not clean.contents
    then pp_print_string ppf "\x1b[39;49m";
    clean := true

  let print_open_tag ppf tag : unit =
    if need_to_print tag then
      match ascii_color tag with
      | Some color when colorify.contents ->
        pp_print_string ppf color;
        clean := false
      | _ ->
        pp_print_string ppf tag;
        pp_print_newline ppf ()

  let install ppf =
    pp_set_print_tags ppf true;
    let tags = pp_get_formatter_tag_functions ppf () in
    pp_set_formatter_tag_functions ppf {
      tags with
      print_open_tag = print_open_tag ppf;
    };
    let out = pp_get_formatter_out_functions ppf () in
    pp_set_formatter_out_functions ppf {
      out with
      out_newline = (fun () ->
          reset ppf;
          out.out_newline ())
    }
end

module None_mode = struct
  let install fmt =
    pp_set_mark_tags fmt false;
    pp_set_print_tags fmt false

end

let modes = ref []

let register_mode mode install =
  modes := (mode,install) :: !modes

let available_modes () = List.map ~f:fst !modes

let install fmt mode =
  List.Assoc.find ~equal:String.equal !modes mode |> function
  | Some install -> install fmt
  | None -> raise (Unknown_mode mode)

let with_mode fmt mode =
  let g = pp_get_formatter_stag_functions fmt () in
  let mark = pp_get_mark_tags fmt () in
  let print = pp_get_print_tags fmt () in
  let finally () =
    pp_set_mark_tags fmt mark;
    pp_set_print_tags fmt print;
    pp_set_formatter_stag_functions fmt g in
  install fmt mode;
  Exn.protect ~finally

let () =
  register_mode "attr" Attr.install;
  register_mode "html" Html.install;
  register_mode "none" None_mode.install;
  register_mode "blocks" Blocks.install
