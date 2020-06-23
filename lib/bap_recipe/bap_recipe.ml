open Base
open Stdio
open Result.Monad_infix
open Caml.Format

module Filename = Stdlib.Filename
module Buffer = Stdlib.Buffer
module Sys = Stdlib.Sys

exception Bad_substitution of string

type format = Zip | Dir | Raw

type error =
  | Expect_atom of Sexp.t list
  | Expect_var of string
  | Bad_item of Sexp.t
  | Bad_param of string
  | Missing_entry of string
  | Bad_sexp of Parsexp.Parse_error.t
  | No_recipe of string
  | Bad_subst of string
  | Unbound_param of string
  | Duplicate_var of string
  | Malformed_recipe of string * string * string
  | System_error of string
[@@deriving sexp_of]

type param = {
  name : string;
  defl : string;
  desc : string;
}

type item =
  | Cmd of string
  | Use of string
  | Opt of string * string list
  | Par of param

type spec = {
  cmd : string option;
  uses : string list;
  opts : (string * string list) list;
  pars : param list;
}

type entry = {path : string;}

type env = {
  vars : string Map.M(String).t;
  paths : string list;
}

type workspace = {
  path : string;
  temp : bool;
  main : string;
}

type t = {
  root  : workspace;
  args : string list;
  descr : string;
  spec : spec;
  loads : t list;
}

let empty_spec = {
  cmd = None;
  uses = [];
  opts = [];
  pars = [];
}

let atom = function
  | Sexp.Atom x -> Ok x
  | Sexp.List xs -> Error (Expect_atom xs)

let atoms xs = Result.all (List.map ~f:atom xs)

let item_of_sexp = function
  | Sexp.List [Sexp.Atom "extend"; Sexp.Atom x] ->
    Ok (Use x)
  | Sexp.List [
      Sexp.Atom "parameter";
      Sexp.Atom name;
      Sexp.Atom defl;
      Sexp.Atom desc
    ] -> Ok (Par {name;defl;desc})
  | Sexp.List (Sexp.Atom "option" :: Sexp.Atom name :: args) ->
    atoms args >>| fun args -> Opt (name, args)
  | Sexp.List (Sexp.Atom "parameter" :: Sexp.Atom s :: _) ->
    Error (Bad_param s)
  | Sexp.List [Sexp.Atom "command";  Sexp.Atom name] -> Ok (Cmd name)
  | x -> Error (Bad_item x)


let items_of_sexps xs = Result.all (List.map xs ~f:item_of_sexp)

let spec_of_items = List.fold ~init:empty_spec ~f:(fun spec -> function
    | Use x -> {spec with uses = x :: spec.uses}
    | Opt (x,xs) -> {spec with opts = (x,xs) :: spec.opts}
    | Par x -> {spec with pars = x :: spec.pars}
    | Cmd name -> {spec with cmd = Some name})

let (/) = Filename.concat

let input file parse root =
  let path = root.path / file in
  if Sys.file_exists path
  then parse (In_channel.read_all path)
  else Error (Missing_entry file)

let parse_recipe str =
  match Parsexp.Many.parse_string str with
  | Ok sexps -> items_of_sexps sexps >>| spec_of_items
  | Error err -> Error (Bad_sexp err)

let parse_descr str = Ok str

let get_recipe w = input w.main parse_recipe w
let get_descr w = match input "descr" parse_descr w with
  | Error (Missing_entry _) -> Ok "Description was not provided"
  | x -> x

let subst_string f str =
  let buf = Buffer.create (String.length str) in
  Buffer.add_substitute buf f str;
  Buffer.contents buf

let make_subst root pars vars arg =
  if String.equal arg "prefix" then root.path
  else match Map.find vars arg with
    | Some x -> x
    | None -> List.find_map pars ~f:(fun {name; defl} ->
        if String.equal name arg
        then Some defl else None) |> function
              | Some x -> x
              | None -> raise (Bad_substitution arg)

let linearize =
  List.map ~f:(fun (name,xs) -> match xs with
      | [] -> name
      | xs -> String.concat ~sep:"=" [
          name;
          String.concat ~sep:"," xs
        ])

let apply_subst_exn root pars vars opts =
  let subs = List.map ~f:(subst_string (make_subst root pars vars)) in
  let args =
    linearize @@ List.map opts ~f:(fun (name,args) ->
        "--"^name, subs args) in
  args

let apply_subst root pars vars opts =
  try Ok (apply_subst_exn root pars vars opts)
  with Bad_substitution s -> Error (Bad_subst s)

let rng = Caml.Random.State.make_self_init ()

let mkdtemp ?(mode=0o0700) ?tmp_dir ?(prefix="") ?(suffix="") () =
  let genname () = Uuidm.v4_gen rng () |> Uuidm.to_string in
  let rec create name =
    let tmp = match tmp_dir with
      | None -> Filename.get_temp_dir_name ()
      | Some tmp -> tmp in
    let path =
      String.concat [tmp; Filename.dir_sep; prefix; name; suffix] in
    match Unix.mkdir path mode with
    | () -> path
    | exception Unix.Unix_error(Unix.EEXIST,_,_) ->
      genname () |> create in
  genname () |> create

let unzip file dst =
  let zip = Zip.open_in file in
  Zip.entries zip |> List.filter ~f:(fun entry -> not entry.Zip.is_directory) |>
  List.iter ~f:(fun ({Zip.filename} as entry) ->
      let path = dst / filename in
      FileUtil.mkdir ~parent:true (Filename.dirname path);
      Zip.copy_entry_to_file zip entry path);
  Zip.close_in zip

let is_zip path =
  try Zip.close_in (Zip.open_in path); true with Zip.Error _ -> false

let target_format t =
  if Sys.is_directory t then Dir
  else if is_zip t then Zip else Raw

let read t = match target_format t with
  | Zip ->
    let path = mkdtemp ~prefix:"recipe-" ~suffix:".unzipped" () in
    unzip t path;
    {path; temp=true; main="recipe.scm"}
  | Raw -> {path=Filename.dirname t; temp=false; main=t}
  | Dir -> {path=t; temp=false; main="recipe.scm"}

let check_vars env spec loads =
  let specs = spec :: List.map loads ~f:(fun s -> s.spec) in
  Map.keys env |> List.find ~f:(fun arg ->
      not (List.exists specs ~f:(fun {pars} ->
          List.exists pars ~f:(fun par ->
              String.equal par.name arg)))) |> function
  | None -> Ok ()
  | Some x -> Error (Unbound_param x)

let search paths =
  List.concat_map paths ~f:(fun p ->
      if Sys.file_exists p && Sys.is_directory p
      then Array.to_list (Sys.readdir p) |>
           List.filter_map ~f:(fun file ->
               if String.is_suffix file ~suffix:".recipe"
               then Some (p/file)
               else None)
      else [])

let which paths name =
  List.find_map paths ~f:(fun p ->
      if Sys.file_exists p && Sys.is_directory p
      then List.find ~f:Sys.file_exists [
          p / name;
          p / name ^ ".scm";
          p / name ^ ".recipe";
        ]
      else None)

let rec load_path env path =
  let root = read path in
  get_descr root >>= fun descr ->
  get_recipe root >>= fun spec ->
  List.map spec.uses ~f:(load env) |> Result.all >>= fun loads ->
  check_vars env.vars spec loads >>= fun () ->
  apply_subst root spec.pars env.vars spec.opts >>| fun args ->
  {root; descr; spec; loads; args}
and load env name = match which env.paths name with
  | None -> Error (No_recipe name)
  | Some path -> load_path env path

let parse_var str =
  match String.split str ~on:'=' with
  | [var;x] -> Ok (String.strip var, String.strip x)
  | _ -> Error (Expect_var str)

let parse_vars ?(env=[]) vars =
  List.map vars ~f:parse_var |> Result.all >>= fun vars ->
  match Map.of_alist (module String) (env@vars) with
  | `Ok v -> Ok v
  | `Duplicate_key s -> Error (Duplicate_var s)

let load_exn ?(paths=[]) ?env name =
  let name,vars = match String.split name ~on:':' with
    | [] -> failwith "Recipe.load: an empty name was passed"
    | name :: vars -> String.strip name, vars in
  parse_vars ?env vars >>= fun vars ->
  load {paths; vars} name

let load ?paths ?env name =
  try load_exn ?paths ?env name with
  | Zip.Error (name,entry,msg) ->
    Error (Malformed_recipe (name,entry,msg))
  | Unix.Unix_error (err,_,_) ->
    Error (System_error (Unix.error_message err))
  | Sys_error s ->
    Error (System_error s)

let rec args t =
  List.concat_map t.loads ~f:args @ t.args

let prepend_before_dash_dash argv args =
  match Array.findi argv ~f:(fun _ -> String.equal "--") with
  | None -> Array.append argv args
  | Some (p,_) -> Array.concat [
      Array.subo ~len:p argv;
      args;
      Array.subo ~pos:p argv
    ]

let args t = Array.of_list @@ args t
let command t = t.spec.cmd
let argv ?(argv=[||]) t = match command t with
  | None -> prepend_before_dash_dash argv (args t)
  | Some cmd ->
    let argv = Array.of_list @@ match Array.to_list argv with
      | [] | [_] as argv -> argv @ [cmd]
      | self :: arg :: rest when String.is_prefix ~prefix:arg cmd ->
        self :: cmd :: rest
      | self :: arg :: rest ->
        self :: cmd :: arg :: rest in
    prepend_before_dash_dash argv (args t)


let rec params t = t.spec.pars @ List.concat_map t.loads ~f:params

let doc t =
  t.descr

let pp_param ppf {name;defl;desc} =
  fprintf ppf "%s - %s (defaults to %s)" name desc defl

module Param = struct
  type t = param
  let name {name} = name
  let doc {desc} = desc
  let default {defl} = defl
  let pp = pp_param
end

let rec close t =
  if t.root.temp
  then FileUtil.rm ~force:FileUtil.Force ~recurse:true [t.root.path];
  List.iter t.loads ~f:close

let pp_error ppf = function
  | Expect_atom sexp ->
    fprintf ppf "Parse error: expected an atom got (%a)"
      (pp_print_list Sexp.pp_hum) sexp
  | Expect_var s ->
    fprintf ppf "Parse error: bad variable definition, got `%s'" s
  | Bad_item s ->
    fprintf ppf "Parse error: unknown recipe item - %a" Sexp.pp_hum s
  | Missing_entry s ->
    fprintf ppf "The required file `%s' is missing" s
  | Bad_sexp err ->
    fprintf ppf "Parse error: %s" (Parsexp.Parse_error.message err)
  | No_recipe r ->
    fprintf ppf "Can't find the required recipe `%s'" r
  | Bad_subst v ->
    fprintf ppf "Don't know how to substitute $%s" v
  | Malformed_recipe (x,y,z) ->
    fprintf ppf "Malformed recipe: %s, %s, %s" x y z
  | Duplicate_var s ->
    fprintf ppf "Variable %s was specified twice" s
  | Bad_param s ->
    fprintf ppf "Syntax error in the parameter `%s' specification. \
                 Expecting: (parameter <name> <value> <desc>)" s
  | Unbound_param s ->
    fprintf ppf "Unknown parameter `%s'" s
  | System_error s ->
    fprintf ppf "System error: %s" s
