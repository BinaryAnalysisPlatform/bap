open Core_kernel.Std
open Bap_ida.Std
open Ida_info

let read_image name =
  In_channel.with_file name ~f:(fun ch ->
      Sexp.input_sexp ch |> image_of_sexp)

let load_image =
  let script =
    {|
from bap.utils.ida import dump_loader_info
dump_loader_info('$output')
idc.Exit(0)
|} in
  Command.create `python
    ~script
    ~parser:read_image

let symbolizer_command =
  let script =
    {|
from bap.utils.ida import dump_symbol_info
dump_symbol_info('$output')
idc.Exit(0)
|} in
  Command.create
    `python
    ~script
    ~parser:(fun name ->
        let blk_of_sexp x = [%of_sexp:string*int64*int64] x in
        In_channel.with_file name ~f:(fun ch ->
            Sexp.input_sexps ch |> List.map ~f:blk_of_sexp))

let brancher_command =
  let script =
    {|
from bap.utils.ida import dump_brancher_info
dump_brancher_info('$output')
idc.Exit(0)
|} in
  Command.create
    `python
    ~script
    ~parser:(fun name ->
        let branch_of_sexp x =
          [%of_sexp: int64 * int64 option * int64 list] x in
        In_channel.with_file name ~f:(fun ch ->
            Sexp.input_sexps ch |> List.map ~f:branch_of_sexp))
