open Core_kernel
open Regular.Std

module Filename = Caml.Filename

let ( / ) = Filename.concat

let open_temp temp_dir =
  let tmp = Filename.temp_file ~temp_dir "tmp" "" in
  try tmp, Unix.(openfile tmp [O_RDWR] 0o600)
  with e -> Sys.remove tmp; raise e

let write ?temp_dir file ~f =
  let dir = Option.value ~default:(Filename.dirname file) temp_dir in
  let tmp,fd = open_temp dir in
  protect ~f:(fun () -> f fd)
    ~finally:(fun () ->
        Unix.close fd;
        Unix.chmod tmp 0o444;
        Sys.rename tmp file)

let read file ~f =
  let fd = Unix.(openfile file [O_RDONLY] 0o400) in
  protect ~f:(fun () -> f fd)
    ~finally:(fun () -> Unix.close fd)

let from_file : type t.
  (module Binable.S with type t = t) -> string -> t = fun b file ->
  read file ~f:(fun fd ->
      let module T = (val b) in
      let data = Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout false [|-1|] in
      let pos_ref = ref 0 in
      T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref)

let from_file' reader file =
  read file ~f:(fun fd ->
      let ch = Unix.in_channel_of_descr fd in
      Data.Read.of_channel reader ch)

let to_file : type t.
  ?temp_dir:string ->
  (module Binable.S with type t = t) ->
  string -> t -> unit =
  fun ?temp_dir b file data ->
  write ?temp_dir file ~f:(fun fd ->
      let module T = (val b) in
      let size = T.bin_size_t data in
      let buf =
        Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout true [|size|] in
      ignore @@ T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0
        data)
[@@warning "-D"]

let to_file' ?temp_dir writer file data =
  write ?temp_dir file ~f:(fun fd ->
      let ch = Unix.out_channel_of_descr fd in
      Data.Write.to_channel writer ch data)
