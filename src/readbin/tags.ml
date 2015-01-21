open Core_kernel.Std
open Format

type mode = [`Text | `Html | `None]

let expect_tag () =
  invalid_arg "Expect: tag := (<name> [<arg> <arg>...]) | <name>"


let expect_arg () = invalid_arg "Expect: arg := (<name> <val>)"

let is_quoted = String.is_prefix ~prefix:"\""

module Html = struct
  open Sexp
  let mark_open_tag tag : string =
    match Sexp.of_string tag with
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

  let mark_close_tag tag : string =
    match Sexp.of_string tag with
    | List (Atom tag :: _) | Atom tag -> sprintf "</%s>" tag
    | _ -> expect_tag ()


  let print_open_tag fmt  _ =  pp_open_vbox fmt 1; pp_print_cut fmt ()
  let print_close_tag fmt _ =  pp_close_box fmt (); pp_print_cut fmt ()

  let install fmt =
    pp_set_mark_tags  fmt true;
    pp_set_print_tags fmt true;
    pp_set_formatter_tag_functions fmt {
      mark_open_tag;
      mark_close_tag;
      print_open_tag = print_open_tag fmt;
      print_close_tag = print_close_tag fmt;
    }
end

module Text = struct
  open Sexp
  (** The following attributes will be outputed:

      [id] will be outputed if there is no [title] [title] will be
      outputed if present with quotes removed.  if neither [id] or
      [title] attributes are found, then *)

  let filter_attrs tag : string option =
    match Sexp.of_string tag with
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

  let mark_open_tag tag : string =
    filter_attrs tag |>
    Option.value_map ~default:"" ~f:(sprintf "begin(%s) ")

  let mark_close_tag tag : string =
    filter_attrs tag |>
    Option.value_map ~default:"" ~f:(sprintf "end(%s)")

  let print_open_tag fmt tag : unit =
    if Option.is_some (filter_attrs tag) then begin
      pp_open_vbox fmt 1;
      pp_print_cut fmt ();
    end

  let print_close_tag fmt tag =
    if Option.is_some (filter_attrs tag) then begin
      pp_close_box fmt ();
      pp_print_cut fmt ();
    end

  let install fmt =
    pp_set_mark_tags fmt true;
    pp_set_print_tags fmt true;
    pp_set_formatter_tag_functions fmt {
      mark_open_tag;
      mark_close_tag;
      print_open_tag = print_open_tag fmt;
      print_close_tag = print_close_tag fmt;
    }
end

let install fmt = function
  | `Text -> Text.install fmt
  | `Html -> Html.install fmt
  | `None ->
    pp_set_mark_tags fmt false;
    pp_set_print_tags fmt false


let with_mode fmt mode =
  let g = pp_get_formatter_tag_functions fmt () in
  let mark = pp_get_mark_tags fmt () in
  let print = pp_get_print_tags fmt () in
  let finally () =
    pp_set_mark_tags fmt mark;
    pp_set_print_tags fmt print;
    pp_set_formatter_tag_functions fmt g in
  install fmt mode;
  Exn.protect ~finally
