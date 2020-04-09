open Core_kernel

module Filename = Caml.Filename

let open_temp temp_dir =
  let tmp =
    Filename.temp_file ~temp_dir "tmp" "" in
  try tmp, Unix.(openfile tmp [O_RDWR] 0o600)
  with e -> Sys.remove tmp; raise e

let from_file : type t.
  (module Binable.S with type t = t) -> string -> t = fun b file ->
  let module T = (val b) in
  let fd = Unix.(openfile file [O_RDONLY] 0o400) in
  try
    let data = Mmap.V1.map_file
        fd Bigarray.char Bigarray.c_layout false [|-1|] in
    let pos_ref = ref 0 in
    let t = T.bin_read_t (Bigarray.array1_of_genarray data) ~pos_ref in
    Unix.close fd;
    t
  with e -> Unix.close fd; raise e
[@@warning "-D"]

let to_file : type t.
  (module Binable.S with type t = t) -> string -> t -> unit =
  fun b file data ->
  let module T = (val b) in
  let dir = Filename.dirname file in
  let tmp,fd = open_temp dir in
  let size = T.bin_size_t data in
  let () =
    try
      let buf =
        Mmap.V1.map_file
          fd Bigarray.char Bigarray.c_layout true [|size|] in
      let _ = T.bin_write_t (Bigarray.array1_of_genarray buf) ~pos:0
          data in
      Unix.close fd
    with e -> Unix.close fd; Sys.remove tmp; raise e in
  Sys.rename tmp file
[@@warning "-D"]
