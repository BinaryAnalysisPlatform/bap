open Core_kernel
open Bap.Std
open Result.Monad_infix
open Format

include Self()

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
  | Use of string  (* (Use x) include steps from the recipe x *)
  | Opt of string * string list
  | Par of param

type spec = {
  uses : string list;
  opts : (string * string list) list;
  pars : param list;
}

type entry = {path : string;}

type env = {
  vars : string String.Map.t;
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
  | x -> Error (Bad_item x)


let items_of_sexps xs = Result.all (List.map xs ~f:item_of_sexp)

let spec_of_items items =
  let uses,opts,pars = List.partition3_map items ~f:(function
      | Use x -> `Fst x
      | Opt (x,xs) -> `Snd (x,xs)
      | Par x -> `Trd x) in
  {uses; opts; pars}

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
  if arg = "prefix" then root.path
  else match Map.find vars arg with
    | Some x -> x
    | None -> List.find_map pars ~f:(fun {name; defl} ->
        if name = arg then Some defl else None) |> function
              | Some x -> x
              | None -> raise (Bad_substitution arg)

let apply_subst_exn root pars vars opts =
  let subst = make_subst root pars vars in
  List.map opts ~f:(fun (name,args) ->
      "--"^name,List.map args ~f:(subst_string subst))

let apply_subst root pars vars opts =
  try Ok (apply_subst_exn root pars vars opts)
  with Bad_substitution s -> Error (Bad_subst s)

let linearize =
  List.map ~f:(fun (name,xs) -> match xs with
      | [] -> name
      | xs -> String.concat ~sep:"=" [
          name;
          String.concat ~sep:"," xs
        ])

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
  Zip.entries zip |> List.filter ~f:(fun entry -> not entry.is_directory) |>
  List.iter ~f:(fun ({filename} as entry) ->
      let path = dst / filename in
      FileUtil.mkdir ~parent:true (Filename.dirname path);
      Zip.copy_entry_to_file zip entry path);
  Zip.close_in zip

let is_zip path =
  Sys.is_directory path ||
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
              par.name = arg)))) |> function
  | None -> Ok ()
  | Some x -> Error (Unbound_param x)

let rec load_path env path =
  let root = read path in
  get_descr root >>= fun descr ->
  get_recipe root >>= fun spec ->
  List.map spec.uses ~f:(load env) |> Result.all >>= fun loads ->
  check_vars env.vars spec loads >>= fun () ->
  apply_subst root spec.pars env.vars spec.opts >>| fun args ->
  {root; descr; spec; loads; args = linearize args}
and load env name =
  List.find_map env.paths ~f:(fun p ->
      if Sys.file_exists p && Sys.is_directory p
      then List.find ~f:Sys.file_exists [
          p / name;
          p / name ^ ".scm";
          p / name ^ ".recipe";
        ]
      else None) |> function
  | None -> Error (No_recipe name)
  | Some path -> load_path env path


let parse_var str =
  match String.split str ~on:'=' with
  | [var;x] -> Ok (String.strip var, String.strip x)
  | _ -> Error (Expect_var str)

let parse_vars vars =
  List.map vars ~f:parse_var |> Result.all >>= fun vars ->
  match String.Map.of_alist vars with
  | `Ok v -> Ok v
  | `Duplicate_key s -> Error (Duplicate_var s)


let load_exn ?(paths=[]) name =
  let name,vars = match String.split name ~on:':' with
    | [] -> failwith "Recipe.load: an empty name was passed"
    | name :: vars -> String.strip name, vars in
  parse_vars vars >>= fun vars ->
  load {paths; vars} name


let load ?paths name =
  try load_exn ?paths name with
  | Zip.Error (name,entry,msg) ->
    Error (Malformed_recipe (name,entry,msg))
  | Unix.Unix_error (err,_,_) ->
    Error (System_error (Unix.error_message err))
  | Sys_error s ->
    Error (System_error s)

let rec args t =
  List.concat_map t.loads ~f:args @ t.args

let argv t = Array.of_list (args t)

let rec params t = t.spec.pars @ List.concat_map t.loads ~f:params

let descr t =
  t.descr

let pp_param ppf {name;defl;desc} =
  fprintf ppf "%s - %s (defaults to %s)" name desc defl

let rec cleanup t =
  if t.root.temp
  then FileUtil.rm ~force:FileUtil.Force ~recurse:true [t.root.path];
  List.iter t.loads ~f:cleanup

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
