let provides = ["rooter"; "symbolizer"]
let doc = "
Provides functions addresses and (optionally) names from a
user specified file(s). The file format is a list of entries, where
each entry is either an address or a delimited with parentheses
tuple. The tuple must have at least two fields separated with space,
with the first field denoting the name of the function and the second
field denoting its start, e.g., $(b,(main 0xDEADBEEF)) or $(b,(main
0xDEADBEEF some other fields)). The function name can be delimited
with double quotes, which is useful if it contains or can contain
whitespaces or special characters. Both kinds of entries can occur in
the same file. The nameless entries will be registered only as function
starts in the knowledge base. And named entries will be both
registered as function starts and as named labels.

If the same name occurs in several entries with different addresses
then the minimal address is chosen as the starting address. If the same
address occurs in entries with different names then a warning is
reported and inconsistent names and addresses are ignored.

If more than one file is specified then the information will be provided
only for units (binaries) that has paths that match with the file
names. In matching only basenames are compared, i.e., directory
names and extensions (if present) are removed. If the file itself is a
directory, then it is expanded to its context. If more than one file
matches with the unit name then all files will be used.

To enable the same behavior when only one file is present, use the
$(b,--read-symbols-when-matches) flag. This flag also accepts an
optional argument that denotes the name of the unit (binary). If the
argument is provided then the information from the file will be
provided only for units with names that match the specified
argument. The same rules of matching are applied (i.e., basenames
without extension). When the flag is used with an argument and more
than one files are provided, then both the unit names and the names
of the files with function information shall match.

When the $(b,--read-symbols-when-matches) flag is not set or only file
is provided, then the information from this file will be promised as a
rule in the knowledge base for each unit (binary).

# EXAMPLES

The input that denotes only function starts:

```
;; use semicolon to denote comments
;; file mv.scm
0xf00 0xba1
0xDEADBEEF
;; end of file
```

The input that denotes both function names and starts:

```
;; file ls.scm
(main 0x4000100)
(foo 0x400200 0x400300) ; anything past the second element is ignored
0x400500 0x400600 0x400700
0x400700 ; any whitespace works as a separator
(bar 0x400900)
;; end of file
```

Passing the files to bap, with a single file

```
bap /bin/mv --read-symbols-from=mv.scm
```

For two files at once,

```
bap compare /bin/{mv,ls} --read-symbols-from={mv,ls}.scm
```

For one file, but use symbols only if the match the filename
of the binary,

```
bap \\$binary --read-symbols-from=mv.scm --read-symbols-when-matches
```

"

open Core_kernel
open Bap_main
open Bap_core_theory
open KB.Syntax
include Bap_main.Loggers()

let anonymous_prefix = "__anonymous_sub%"

let is_anonymous = String.is_prefix ~prefix:anonymous_prefix

let parse_entry = function
  | Sexp.List (Atom name :: Atom addr :: _) ->
    name, Bitvec.of_string addr
  | Sexp.Atom addr ->
    sprintf "%s%s" anonymous_prefix addr, Bitvec.of_string addr

  | _ -> invalid_arg "failed to read symbols, expects (<name> <addr> ...)"

let empty = Bap_relation.empty Bitvec.compare String.compare

let build_relation rels =
  List.fold ~f:(fun rel (name,start) ->
      Bap_relation.add rel start name)
    ~init:rels

let read_relations rels file =
  Sexp.load_sexps_conv_exn file parse_entry |>
  build_relation rels

let build_functions relation =
  let functions = Hashtbl.create (module struct
      include Bitvec
      let sexp_of_t x = Sexp.Atom (Bitvec.to_string x)
    end) in
  Bap_relation.matching relation ()
    ~saturated:(fun addr name () -> Hashtbl.add_exn functions addr name)
    ~unmatched:(fun problem () -> match problem with
        | Non_injective_bwd (names,addr) ->
          error "skipping names (%s) that has the same address %a"
            (String.concat ~sep:", " names) Bitvec.pp addr
        | Non_injective_fwd (addrs,name) ->
          match List.min_elt addrs ~compare:Bitvec.compare with
          | None -> assert false
          | Some addr -> Hashtbl.set functions addr name);
  functions

let chop_suffix p = try Filename.chop_extension p with _ -> p
let normalize p = chop_suffix@@Filename.basename p
let name_matches p1 p2 = String.equal (normalize p1) (normalize p2)

let data = KB.Class.property Theory.Unit.cls "read-symbols-data"
    ~package:"bap" @@
  KB.Domain.optional "functions"
    ~equal:(Hashtbl.equal String.equal)

let readdir path =
  Sys.readdir path |>
  Array.to_list |>
  List.map ~f:(Filename.concat path)

let collect path files filter =
  let always_matches = match files,filter with
    | [_],None -> true
    | _ -> false in
  let matches file =
    always_matches ||
    name_matches file path && match filter with
    | Some (Some other) -> name_matches other path
    | _ -> true in
  let paths =
    List.concat_map files ~f:(fun file->
        if Sys.file_exists file then
          if Sys.is_directory file
          then readdir path
          else [file]
        else []) |>
    List.filter ~f:matches in
  List.fold paths ~init:empty ~f:read_relations |>
  build_functions

let provide_data files matches =
  KB.promise data @@ fun unit ->
  KB.collect Theory.Unit.path unit >>|? fun path ->
  Some (collect path files matches)

let agent =
  let reliability = KB.Agent.authorative in
  KB.Agent.register ~package:"bap"
    ~reliability "user-symbolizer"
    ~desc:"reads symbols from the user provided file"


let get_entry label =
  KB.collect Theory.Label.addr label >>=? fun addr ->
  KB.collect Theory.Label.unit label >>=? fun unit ->
  KB.collect data unit >>|? fun data ->
  Hashtbl.find data addr

let provide_starts () =
  KB.promise Theory.Label.is_subroutine @@ fun label ->
  get_entry label >>|? fun _ -> Some true

let provide_names () =
  KB.propose agent Theory.Label.possible_name @@ fun label ->
  get_entry label >>|? fun name ->
  Option.some_if (not (is_anonymous name)) name


let files = Extension.Configuration.parameters
    Extension.Type.(list string) "from"

let enable_matches = Extension.Configuration.parameter
    ~as_flag:(Some None)
    Extension.Type.(some (some string))
    "when-matches"



let () = Extension.declare ~doc ~provides @@ fun ctxt ->
  let open Extension.Syntax in
  match List.concat (ctxt-->files) with
  | [] -> Ok ()
  | files ->
    provide_data files (ctxt-->enable_matches);
    provide_starts ();
    provide_names ();
    Ok ()
