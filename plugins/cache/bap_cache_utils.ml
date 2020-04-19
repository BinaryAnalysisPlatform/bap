open Core_kernel

module Filename = Caml.Filename

let open_temp temp_dir =
  let tmp =
    Filename.temp_file ~temp_dir "tmp" "" in
  try tmp, Unix.(openfile tmp [O_RDWR] 0o600)
  with e -> Sys.remove tmp; raise e

let unsafe_from_file : type t.
  (module Binable.S with type t = t) -> string -> t = fun b file ->
  let module T = (val b) in
  let fd = Unix.(openfile file [O_RDONLY] 0o400) in
  protect ~f:(fun () ->
      let data = Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout false [|-1|] in
      let pos_ref = ref 0 in
      T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref)
    ~finally:(fun () -> Unix.close fd)
[@@warning "-D"]

let unsafe_to_file : type t.
  (module Binable.S with type t = t) -> string -> t -> unit =
  fun b file data ->
  let module T = (val b) in
  let dir = Filename.dirname file in
  let tmp,fd = open_temp dir in
  let size = T.bin_size_t data in
  protect ~f:(fun () ->
      let buf =
        Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout true [|size|] in
      let _ = T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0
          data in
      Sys.rename tmp file)
    ~finally:(fun () ->
        Unix.close fd;
        Sys.remove tmp)
[@@warning "-D"]
