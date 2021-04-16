let doc = "
# DESCRIPTION

Displays information about the binary. The information is printed in
the OGRE format.
"

open Core_kernel
open Bap_main
open Bap.Std
open Regular.Std

type problem =
  | Unknown_loader of string
  | Loader_error of Error.t

type Extension.Error.t += Fail of problem


let input = Extension.Command.argument
    ~doc:"The input file"
    Extension.Type.("FILE" %: string =? "a.out")

let formats = [
  "sexp", `Sexp;
  "yaml", `Yaml;
]

let format = Extension.Command.parameter
    (Extension.Type.enum formats) "format"
    ~aliases:["o"]
    ~doc:"The output format"

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader.
          Use the loader `raw' to load unstructured files"
    Extension.Type.(string =? "llvm")
    "loader"

let problem is = Error (Fail is)

let reader = Data.Read.create ()
    ~of_bigstring:(Binable.of_bigstring (module Ogre.Doc))
let writer = Data.Write.create ()
    ~to_bigstring:(Binable.to_bigstring (module Ogre.Doc))

let print fmt spec =
  let pp = match fmt with
    | `Yaml -> Ogre.Doc.pp_yaml
    | `Sexp -> Ogre.Doc.pp in
  Format.printf "%a@\n%!" pp spec

let () = Extension.Command.(begin
    declare "specification" (args $format $input $loader)
      ~doc
      ~requires:["loader"]
  end) @@ fun format input loader ctxt ->
  let digest = Data.Cache.Digest.create ~namespace:"specification" in
  let digest = Data.Cache.Digest.add digest "%s%s%s"
      input loader (Extension.Configuration.digest ctxt) in
  let cache = Data.Cache.Service.request reader writer in
  match Data.Cache.load cache digest with
  | Some spec -> print format spec; Ok ()
  | None -> match Image.find_loader loader with
    | None -> problem (Unknown_loader loader)
    | Some (module Load) -> match Load.from_file input with
      | Ok (Some spec) ->
        Data.Cache.save cache digest spec;
        print format spec; Ok ()
      | Ok None -> Ok ()
      | Error err -> problem (Loader_error err)


let string_of_problem = function
  | Unknown_loader name ->
    sprintf "The loader `%s' is not registers, known loaders are: %s."
      name (Image.available_backends () |> String.concat ~sep:", ")
  | Loader_error err ->
    sprintf "Failed to load the binary: %s"  (Error.to_string_hum err)

let () = Extension.Error.register_printer @@ function
  | Fail p -> Some (string_of_problem p)
  | _ -> None
