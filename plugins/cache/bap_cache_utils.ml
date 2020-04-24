open Core_kernel
open Regular.Std

module Filename = Caml.Filename

let ( / ) = Filename.concat

let write ?temp_dir file ~f =
  let temp_dir = Option.value ~default:(Filename.dirname file) temp_dir in
  let tmp,ch = Filename.open_temp_file ~temp_dir "tmp" "" in
  protect ~f:(fun () -> f ch)
    ~finally:(fun () ->
        Out_channel.close ch;
        Unix.chmod tmp 0o444;
        Sys.rename tmp file)

let read file ~f = In_channel.with_file file ~f

let from_file : type t.
  (module Binable.S with type t = t) -> string -> t = fun b file ->
  read file ~f:(fun ch ->
      let module T = (val b) in
      let data = Mmap.V1.map_file
          (Unix.descr_of_in_channel ch)
          Bigarray.char Bigarray.c_layout false [|-1|] in
      let pos_ref = ref 0 in
      T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref)

let from_file' reader file =
  read file ~f:(Data.Read.of_channel reader)

let to_file : type t.
  ?temp_dir:string ->
  (module Binable.S with type t = t) ->
  string -> t -> unit =
  fun ?temp_dir b file data ->
  write ?temp_dir file ~f:(fun ch ->
      let module T = (val b) in
      let size = T.bin_size_t data in
      let buf =
        Mmap.V1.map_file
          (Unix.descr_of_out_channel ch)
          Bigarray.char Bigarray.c_layout true [|size|] in
      ignore @@ T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0
        data)
[@@warning "-D"]

let to_file' ?temp_dir writer file data =
  write ?temp_dir file ~f:(fun ch ->
      Data.Write.to_channel writer ch data)
