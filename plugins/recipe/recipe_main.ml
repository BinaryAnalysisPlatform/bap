open Base
open Bap_main
open Stdlib.Format


module Recipe = Bap_recipe
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys


type Extension.Error.t += Recipe_error of Recipe.error

let recipe_paths = [
  Filename.current_dir_name;
  Extension.Parameter.datadir
]

let print_and_close_recipe r =
  printf "DESCRIPTION@\n@\n%s@\n@\n" (Recipe.doc r);
  let params = Recipe.params r in
  if not (List.is_empty params) then begin
    printf "PARAMETERS@\n@\n";
    List.iter params ~f:(printf "- %a@\n" Recipe.Param.pp);
    printf "@\n";
  end;
  let args = Recipe.argv r in
  let sep = if Array.length args > 4 then " \\\n" else " " in
  printf "COMMAND LINE@\n@\n%s@\n" (String.concat_array ~sep args);
  Recipe.close r

let summary str =
  match String.index str '\n' with
  | None -> str
  | Some p -> String.subo ~len:p str

let print_all_recipes () =
  let (/) = Filename.concat in
  List.iter recipe_paths ~f:(fun dir ->
      if Sys.file_exists dir &&
         Sys.is_directory dir
      then Array.iter (Sys.readdir dir) ~f:(fun entry ->
          let file = dir / entry in
          if Filename.check_suffix file ".recipe"
          then
            let name = Filename.chop_suffix entry ".recipe" in
            match Recipe.load ~paths:recipe_paths name with
            | Ok r ->
              printf "%-32s %s\n" (Filename.basename name)
                (summary (Recipe.doc r));
              Recipe.close r
            | Error err ->
              eprintf "Malformed recipe %s: %a@\n%!" file
                Recipe.pp_error err))

let recipes = Extension.Command.arguments Extension.Type.string

let () =
  Extension.Command.(declare "print-recipes" (args $ recipes))
  @@ fun recipes _ctxt -> match recipes with
  | [] -> Ok (print_all_recipes ())
  | recipes ->
    let open Result.Monad_infix in
    Result.all_unit @@
    List.map recipes ~f:(fun r ->
        Recipe.load ~paths:recipe_paths r |>
        Result.map_error ~f:(fun err -> Recipe_error err) >>| fun r ->
        print_and_close_recipe r)
