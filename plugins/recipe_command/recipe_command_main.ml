open Base
open Bap_main
open Stdlib.Format

let doc = {|
# DESCRIPTION

Provides commands to manipulate the recipe subsystem.

# RECIPE DESCRIPTION

A recipe is either a single file or a directory (optionally
zipped) that contains a parametrized specification of command
line parameters and support files if necessary.

The purpose of the recipe is to make bap runs reproducible, so
that you can share one simple file - the recipe, instead of a
bunch of scripts together with some verbal instructions. Since
recipes introduce an extra layer of abstraction they actually
simplify interaction with bap by hiding unnecessary
details. Recipes also provide a mechanism for building ready
solutions and sharing them with users.

To use a recipe, just specify its name using the $(b,--recipe)
command line parameter. If a recipe has parameters then they could
be specified as colon separated list of $(b,<key>=<value>)
pairs. See the $(b,--recipe) parameter for more information. To
read the recipe description, including the list of parameters and
resulting command line, use the $(b,--show-recipe) command line
option. To list all installed recipes use the $(b,--list-recipes)
option.

The main (and the only necessary) part of a recipe is the recipe
specification, that is a file that contains a list of recipe items
in an arbitrary order. Each item is either a command line
option, a parameter, or a reference to another recipe. All items
share the same syntax - they are flat s-expressions, i.e., a
whitespace separated list of strings enclosed in parentheses. The
first string in the lists denotes the type of the item. E.g.,
$(b,(option run-entry-points malloc calloc free)).


The $(b,option) command requires one mandatory parameter, the
option name, and an arbitrary number of arguments that will be
passed to the corresponding command line option. If there are more
than one argument then they will be concatenated with the comma
symbol, e.g., $(b,(option opt a b c d)) will be translated to
$(b,--opt=a,b,c,d). Option arguments may contain substitution
symbols. A substitution symbol starts with the dollar sign, that is
followed by a named (optionally delimited with curly braces, to
disambiguate it from the rest of the argument). There is one built
in parameter called $(b,prefix), that is substituted with the path
to the recipe top folder. See the $(b,parameter) command to learn
how to introduce parameters.

The $(b,parameter) command introduces a parameter to the recipe, i.e.,
a variable ingredient that could be changed when the recipe is
used. The parameter command has 3 arguments, all required. The
first argument is the parameter name, the second is the default
value, that is used if the a parameter wasn't set, and the last
argument is the parameter description.  The substitution symbol
will be replaced with the default value of a parameter, if a value
of the parameter wasn't passed through the command line. Example,"

```
    (parameter depth 128 "maximum depth of analysis")
    (option analysis-depth \$depth)
```

If the parameter is not set through the command line, then it will
be substituted with $(b,128) otherwise it will receive whatever
value a user has passed.

The $(b,command) stanza specifies the command that the recipe should
run. It is optional, since recipes could be generic and applicable to
different commands, which gives an extra freedom to the recipe
user.

Finally, the $(b,extend) command is like the include statement in
the C preprocessor as it includes all the ingredients from another
recipe. (Make sure that you're not introducing loops!). The
command has one mandatory argument, the name of the recipe to
include.;

# RECIPE GRAMMAR

```
    recipe ::= {<recipe-item>}
    recipe-item ::= <option> | <parameter> | <extend> | <command>
    option ::= (option <atom> {<atom>})
    parameter ::= (parameter <atom> <atom> <atom>)
    extend ::= (extend <atom>)
    command ::= (command <atom>)
```
|}

module Recipe = Bap_recipe
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys

type Extension.Error.t += Recipe_error of Recipe.error

let recipe_paths = [
  Filename.current_dir_name;
  Extension.Configuration.datadir;
  Extension.Configuration.sysdatadir;
]

let cleanup ~keep r =
  if not keep then Recipe.close r

let print_recipe ~keep r =
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
  cleanup ~keep r

let minimize str =
  let len = String.length str in
  if len < 60 then str
  else String.subo ~len:60 str ^ "..."

let summary str =
  minimize @@
  String.uncapitalize @@
  match String.index str '\n' with
  | None -> str
  | Some p -> String.subo ~len:(min p 60) str


let print_all_recipes ~keep () =
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
              printf "  %-24s %s\n" (Filename.basename name)
                (summary (Recipe.doc r));
              cleanup ~keep r
            | Error err ->
              eprintf "Malformed recipe %s: %a@\n%!" file
                Recipe.pp_error err))

let recipes = Extension.Command.arguments Extension.Type.string
let keep_open =
  Extension.Command.flag ~aliases:["k"] "keep-open"
    ~doc:"Do not delete the temporary workspace."

let () =
  Extension.Command.(declare ~doc "recipes" args) @@ fun _ctxt ->
  Ok ()

let () =
  let doc = "prints available recipes." in
  Extension.Command.(declare ~doc "print-recipes" (args $ recipes $keep_open))
  @@ fun recipes keep _ctxt -> match recipes with
  | [] -> Ok (print_all_recipes ~keep ())
  | recipes ->
    let open Result.Monad_infix in
    Result.all_unit @@
    List.map recipes ~f:(fun r ->
        Recipe.load ~paths:recipe_paths r |>
        Result.map_error ~f:(fun err -> Recipe_error err) >>| fun r ->
        print_recipe ~keep r)


let () = Extension.Error.register_printer @@ function
  | Recipe_error err ->
    Some (Stdlib.Format.asprintf "%a" Recipe.pp_error err)
  | _ -> None
