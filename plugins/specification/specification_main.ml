let doc = "
# DESCRIPTION

Displays information about the binary. The information is printed in
the OGRE format.
"

open Core_kernel
open Bap_main
open Bap.Std

type problem =
  | Unknown_loader of string
  | Loader_error of Error.t

type Extension.Error.t += Fail of problem


let input = Extension.Command.argument
    ~doc:"The input file" Extension.Type.("FILE" %: string =? "a.out" )

let loader =
  Extension.Command.parameter
    ~doc:"Use the specified loader.
          Use the loader `raw' to load unstructured files"
    Extension.Type.(string =? "llvm")
    "loader"

let problem is = Error (Fail is)

let () = Extension.Command.(begin
    declare "specification" (args $input $loader)
      ~doc
      ~requires:["loader"]
  end) @@ fun input loader _ctxt ->
  match Image.find_loader loader with
  | None -> problem (Unknown_loader loader)
  | Some (module Load) -> match Load.from_file input with
    | Ok (Some spec) -> Format.printf "%a@\n%!" Ogre.Doc.pp spec; Ok ()
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
