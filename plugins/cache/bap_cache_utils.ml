open Core_kernel
open Regular.Std

module Filename = Caml.Filename
module Unix = Caml_unix

let ( / ) = Filename.concat

let write_to_file ?temp_dir writer file data =
  let temp_dir = Option.value ~default:(Filename.dirname file) temp_dir in
  let tmp,ch = Filename.open_temp_file ~temp_dir "tmp" "" in
  protect ~f:(fun () -> Data.Write.to_channel writer ch data)
    ~finally:(fun () ->
        Out_channel.close ch;
        Unix.chmod tmp 0o444;
        Sys.rename tmp file)

let open_temp temp_dir =
  let tmp = Filename.temp_file ~temp_dir "tmp" "index" in
  try tmp, Unix.(openfile tmp [O_RDWR;] 0o600)
  with e -> Sys.remove tmp; raise e

let binable_to_file : type t.
  ?temp_dir:string ->
  (module Binable.S with type t = t) ->
  string -> t -> unit =
  fun ?temp_dir b file data ->
  let module T = (val b) in
  let temp_dir = Option.value ~default:(Filename.dirname file) temp_dir in
  let tmp,fd = open_temp temp_dir in
  let size = T.bin_size_t data in
  protect ~f:(fun () ->
      let buf =
        Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout true [|size|] in
      ignore @@
      T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0 data)
    ~finally:(fun () ->
        Unix.close fd;
        Unix.chmod tmp 0o444;
        Sys.rename tmp file)

let binable_from_file : type t.
  (module Binable.S with type t = t) -> string -> t = fun b file ->
  let module T = (val b) in
  let fd = Unix.(openfile file [O_RDONLY] 0o400) in
  let data = Mmap.V1.map_file fd
      Bigarray.char Bigarray.c_layout false [|-1|] in
  let pos_ref = ref 0 in
  T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref

let read_from_file reader file =
  In_channel.with_file file ~f:(Data.Read.of_channel reader)
