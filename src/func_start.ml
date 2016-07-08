open Core_kernel.Std
open Bap.Std
open Bap_future.Std
open Bap_plugins.Std

type truth_format = [
  | `unstripped_bin
  | `symbol_file ]

let format_of_filename f =
  if Filename.check_suffix f ".scm" then `symbol_file
  else `unstripped_bin

let of_truth truth ~testbin : addr seq Or_error.t future =
  let value =
    match format_of_filename truth with
    | `unstripped_bin -> Ground_truth.from_unstripped_bin truth
    | `symbol_file -> Ground_truth.from_symbol_file truth ~testbin in
  Future.return value

let of_tool tool ~testbin : addr seq Or_error.t future =
  let module EF = Monad.T.Or_error.Make(Future) in
  let rooter = Rooter.Factory.find tool in
  let rooter =
    match rooter with
    | Some x -> x
    | None -> invalid_argf "Unknown tool %S. Possible options: %s"
                tool (Rooter.Factory.list ()
                      |> List.map ~f:(fun x -> x,x)
                      |> Config.doc_enum) () in
  let rooter_fe = Stream.hd rooter in
  let input = Project.Input.file testbin in
  let _ = match Project.create ~rooter input with
    | Ok x -> x
    | Error e -> Error.raise e in
  let open EF in
  rooter_fe >>= (fun r ->
      let future, promise = Future.create () in
      Future.upon Plugins.loaded (fun () ->
          let addr_seq = Rooter.roots r in
          Promise.fulfill promise (Or_error.return addr_seq));
      future)
