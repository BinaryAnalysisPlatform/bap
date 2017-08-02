open Core_kernel.Std
open Bap_bundle.Std
open Format

open Manifest.Fields

exception Provides_field_syntax_error
exception Target_unspecified
exception Target_is_directory
exception Target_doesn't_exist
exception Destdir_is_not_a_dir

let target = ref ""
let manifest = ref (Manifest.create "")
let modified = String.Hash_set.create ()
let destdir = ref Bap_config.plugindir

let destdir_arg =
  "-destdir", Arg.Set_string destdir,
  sprintf "DIR Set the destination directory (default: %s)"
    !destdir

let field_modified field =
  Hash_set.add modified (Field.name field)

let is_field_modified field =
  Hash_set.mem modified (Field.name field)

let set field value =
  field_modified field;
  manifest := Field.fset field !manifest value

let add field x =
  let xs = Field.get field !manifest in
  match List.mem xs x with
  | true -> ()
  | false ->
    field_modified field;
    manifest := Field.fset field !manifest (xs @ [x])

let set_target t = target := t

let chop_extension name =
  if String.mem name '.'
  then Filename.chop_extension name
  else name

let name_of_path path =
  Filename.basename path |> chop_extension

let fields = String.split ~on:','

let named_resource r = match String.split ~on:'=' r with
  | [name;file] -> Some name, file
  | _ -> None,r

let normalized x =
  let suffix = ".plugin" in
  let with_suffix name =
    if Filename.check_suffix name suffix then name
    else name ^ suffix in
  let is_plugin x = Sys.file_exists (with_suffix x) in
  if is_plugin x then with_suffix x else x

let open_bundle () = match normalized (target.contents) with
  | "" -> raise Target_unspecified
  | x when Sys.is_directory x -> raise Target_is_directory
  | x when Sys.file_exists x -> Bundle.of_uri (Uri.of_string x)
  | x -> raise Target_doesn't_exist

module Update = struct
  let resources = ref []

  let add_res r =
    resources := r :: !resources

  let set_list s f =
    String.split ~on:',' s |> List.map ~f:String.strip |> set f

  let set_name s =
    set name @@
    String.map ~f:(function
        | '_' -> '-'
        | c -> c) s

  let set_tags s = set_list s tags
  let set_cons s = set_list s cons

  let common_args = Arg.[
      "-author",    String (set author), "<name> Set bundle's author name";
      "-name",      String set_name,     "<name> Set bundle's name";
      "-desc",      String (set desc),   "<text> Set bundle's description";
      "-tags",      String set_tags,     "<list> Set bundle's tags";
      "-cons",      String set_cons,     "<list> Set bundle's constraints";
    ]

  let args = common_args @ Arg.[
      "-add-resources", String add_res,"<list> Add resource files";
    ]

  let update m field =
    if is_field_modified field
    then Field.fset field m (Field.get field !manifest)
    else m

  let main () =
    let bundle = open_bundle () in
    Bundle.update_manifest bundle ~f:(fun init ->
        let b = List.fold [author;name;desc;] ~init ~f:update in
        List.fold [cons;tags;] ~init:b ~f:update);
    List.map !resources ~f:named_resource |>
    List.map ~f:(fun (name,path) -> name, Uri.of_string path) |>
    Bundle.insert_files bundle
end

module Pack = struct
  let pack = Bundle.Builder.create ()

  let named_library f = match String.split ~on:'=' f with
    | [name;file] -> Some (Some name,file)
    | _ -> None


  let bundle_file (name,uri) =
    Bundle.Builder.put_file pack ?name (Uri.of_string uri)

  let bundle_files files = List.iter files ~f:bundle_file

  let add_requires fs =
    List.iter (fields fs) ~f:(fun f -> match String.split ~on:'=' f with
        | [name] -> add requires name
        | [name;path] ->
          add requires name;
          bundle_file (None,path)
        | _ -> raise Provides_field_syntax_error)

  let add_provides fs = List.iter (fields fs) ~f:(add provides)

  let add_resources rs =
    List.map (fields rs) ~f:named_resource |>
    bundle_files

  let set_main file =
    set main (Filename.chop_extension file);
    bundle_file (None,file)

  let args = Update.common_args @ Arg.[
      "-main",      String (set_main),      "<name> Set the entry point";
      "-requires",  String (add_requires),  "<list> List of required artifacts";
      "-provides",  String (add_provides),  "<list> List of provided interfaces";
      "-resources", String (add_resources), "<list> List of embedded resources";
    ]

  let main () =
    let manifest = !manifest in
    let target = if target.contents = ""
      then Manifest.name manifest
      else target.contents in
    if target = "" then raise Target_unspecified;
    let uri = Uri.of_string target in
    Bundle.Builder.embed_manifest pack manifest;
    Bundle.Builder.flush pack uri
end

module Show = struct
  let smanifest = ref false
  let resources = ref false
  let libraries = ref false

  let args = Arg.[
      "-m", Set smanifest, "Show meta information";
      "-r", Set resources, "Show available resources";
      "-l", Set libraries, "Show packed libraries"
    ]

  let show_manifest m =
    printf "%s@." @@ Manifest.to_string m

  let show_files k m b =
    let choose = match k with
      | `libraries when libraries.contents -> ident
      | `resources when resources.contents -> not
      | _ -> fun _ -> false in
    let is_library name =
      Filename.check_suffix name ".cmxs" ||
      Filename.check_suffix name ".cma" in
    List.iter (Bundle.list b) ~f:(fun name ->
        if choose (is_library name)
        then printf "%s@." name)

  let main () =
    let bundle = open_bundle () in
    let m = Bundle.manifest bundle in
    if smanifest.contents then show_manifest m;
    show_files `resources m bundle;
    show_files `libraries m bundle
end

module Install = struct
  let args = [destdir_arg]

  let main () =
    if target.contents = "" then raise Target_unspecified;
    target := normalized !target;
    if not (Sys.file_exists !destdir)
    then FileUtil.mkdir ~parent:true !destdir;
    if not (Sys.is_directory !destdir)
    then raise Destdir_is_not_a_dir;
    if Sys.file_exists !target
    then if Sys.is_directory !target
      then raise Target_is_directory
      else FileUtil.cp [!target] !destdir
    else raise Target_doesn't_exist
end

module Remove = struct
  let args = [destdir_arg]
  let main () =
    if target.contents = "" then raise Target_unspecified;
    let file = normalized @@ Filename.concat !destdir !target in
    if Sys.file_exists file
    then Sys.remove file
end


let args = ref []

let parse = ref ignore
let main = ref ident

module type Command = sig
  val args : (string * Arg.spec * string) list
  val main : unit -> unit
end

let select arg =
  let switch (module Command : Command) =
    args := Arg.align (Command.args @ !args);
    parse := set_target;
    main := Command.main in
  match arg with
  | "pack"    -> switch (module Pack)
  | "update"  -> switch (module Update)
  | "show"    -> switch (module Show)
  | "install" -> switch (module Install)
  | "remove"  -> switch (module Remove)
  | msg -> raise (Arg.Bad ("Unknown command: " ^ msg))

let dispatch arg = !parse arg

let usage_msg = "USAGE

  bapbundle -help
  bapbundle <command> -help
  bapbundle install [<install-options>] <bundle>
  bapbundle remove  [<remove-options>]  <bundle>
  bapbundle show    [<show-options>]    <bundle>
  bapbundle update  [<update-options>]  <bundle>
  bapbundle pack    [<pack-options>]    <bundle>

DESCRIPTION

  Manages BAP bundles. If you're looking for a way to
  create a bundle, then bapbundle is the tool that you're
  looking for.

  See bapbundle(1) man-page for a full manual.

OPTIONS
"

let error fmt =
  kfprintf (fun ppf -> pp_print_newline ppf (); exit 2) err_formatter fmt

let usage fmt =
  kfprintf (fun ppf ->
      pp_print_newline ppf ();
      pp_print_newline ppf ();
      pp_print_string ppf @@
      Arg.usage_string !args usage_msg;
      pp_print_newline ppf ();
      exit 1)
    err_formatter fmt


let () =
  parse := select;
  Arg.parse_dynamic args dispatch usage_msg;
  try !main () with
  | Provides_field_syntax_error ->
    error "An entry in the provides list should be <name> | <name>=<path>"
  | Target_unspecified ->
    usage "Please, specify the bundle name"
  | Target_is_directory ->
    error "The specified bundle is a directory"
  | Target_doesn't_exist ->
    error "Can't open the specified bundle file, it doesn't exist"
  | Not_a_bundle ->
    error "The provided bundle file is either malformed or is not a bundle"
  | Destdir_is_not_a_dir ->
    error "The destination should be a directory, not a file"
  | Sys_error err ->
    error "%s" err
  | exn ->
    error "Unexpected error: %s\nPlease report" @@
    Exn.to_string exn
