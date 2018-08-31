open Core_kernel
open Bap.Std
open Regular.Std

let max = 500
let bils = String.Table.create ()
let actual = Int64.Table.create ()

let min = ref Int64.zero

let digest_of_bil b = Digest.string (Bil.to_string b)

let clean () =
  let d = Hashtbl.find_exn actual !min in
  Hashtbl.remove bils d;
  Int64.incr min

let add digest bil =
  Hashtbl.set bils digest bil;
  if Hashtbl.length bils > max then clean ()
