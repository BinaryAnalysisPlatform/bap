open Core_kernel.Std
open Result.Monad_infix

exception Bad_substitution of string

type error =
  | Expect_atom of Sexp.t list
  | Expect_var of string
  | Bad_item of Sexp.t
  | Missing_entry of string
  | Bad_sexp of Parsexp.Parse_error.t
  | No_recipe of string
  | Bad_subst of string
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

type env = {
  vars : string String.Map.t;
  paths : string list;
}

type t = {
  files : string String.Map.t;
  descr : string;
  loads : t list;
  args : string list;
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
  | x -> Error (Bad_item x)


let items_of_sexps xs = Result.all (List.map xs ~f:item_of_sexp)

let spec_of_items items =
  let uses,opts,pars = List.partition3_map items ~f:(function
      | Use x -> `Fst x
      | Opt (x,xs) -> `Snd (x,xs)
      | Par x -> `Trd x) in
  {uses; opts; pars}

let input files file parse =
  match Map.find files file with
  | None -> Error (Missing_entry file)
  | Some name -> parse (In_channel.read_all name)

let parse_recipe str =
  match Parsexp.Many.parse_string str with
  | Ok sexps -> items_of_sexps sexps >>| spec_of_items
  | Error err -> Error (Bad_sexp err)

let parse_descr str = Ok str

let (/) = Filename.concat

let make_path path name =
  path / sprintf "%s.recipe" name

let subst_string f str =
  let buf = Buffer.create (String.length str) in
  Buffer.add_substitute buf f str;
  Buffer.contents buf

let make_subst pars vars arg =
  match Map.find vars arg with
  | Some x -> x
  | None -> List.find_map pars ~f:(fun {name; defl} ->
      if name = arg then Some defl else None) |> function
            | Some x -> x
            | None -> try Sys.getenv arg with
              | Not_found -> raise (Bad_substitution arg)

let apply_subst_exn pars vars opts =
  let subst = make_subst pars vars in
  List.map opts ~f:(fun (name,args) ->
      "--"^name,List.map args ~f:(subst_string subst))

let apply_subst pars vars opts =
  try Ok (apply_subst_exn pars vars opts)
  with Bad_substitution s -> Error (Bad_subst s)

let linearize  =
  List.concat_map ~f:(fun (name,xs) -> name :: xs)

let rec load_path env path =
  let zip = Zip.open_in path in
  let files =
    List.fold ~init:String.Map.empty (Zip.entries zip)
      ~f:(fun files ({filename} as entry) ->
          let tmp,out = Filename.open_temp_file "bap" filename in
          Zip.copy_entry_to_channel zip entry out;
          Out_channel.close out;
          Map.add files ~key:filename ~data:tmp) in
  input files "descr" parse_descr >>= fun descr ->
  input files "recipe.scm" parse_recipe >>= fun spec ->
  List.map spec.uses ~f:(load env) |> Result.all >>= fun loads ->
  apply_subst spec.pars env.vars spec.opts >>| fun args ->
  {files; descr; loads; args = linearize args}
and load env name =
  List.find env.paths ~f:(fun p ->
      Sys.file_exists p &&
      Sys.is_directory p &&
      Sys.file_exists (make_path p name)) |> function
  | None -> Error (No_recipe name)
  | Some parent -> load_path env (make_path parent name)


let parse_var str =
  match String.split str ~on:'=' with
  | [var;x] -> Ok (String.strip var, String.strip x)
  | _ -> Error (Expect_var str)

let parse_vars vars =
  match vars with
  | None -> Ok (String.Map.empty)
  | Some vars ->
    String.split vars ~on:',' |>
    List.map ~f:parse_var |> Result.all >>= fun vars ->
    match String.Map.of_alist vars with
    | `Ok v -> Ok v
    | `Duplicate_key s -> Error (Duplicate_var s)


let load_exn ?(paths=[]) name =
  let name,vars = match String.split name ~on:':' with
    | [name;vars] -> name, Some vars
    | _ -> name, None in
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

let descr t = t.descr

let rec cleanup t =
  Map.iter t.files ~f:Sys.remove;
  List.iter t.loads ~f:cleanup

let pp_error ppf err =
  Sexp.pp_hum ppf (sexp_of_error err)
