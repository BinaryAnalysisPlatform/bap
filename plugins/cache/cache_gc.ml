open Core_kernel
open Bap.Std
open Cache_types

include Self()

let remove_entry e =
  try Sys.remove e.path
  with exn ->
    warning "unable to remove entry: %s" (Exn.to_string exn)

let threshold c =
  Int64.(c.max_size + of_float (to_float c.max_size *. c.overhead))

let time_to_clean idx =
  Int64.(idx.current_size > threshold idx.config)

let remove_from_index idx key =
  match Map.find idx.entries key with
  | None -> idx
  | Some entry ->
    let current_size = Int64.(idx.current_size - entry.size) in
    { idx with entries = Map.remove idx.entries key; current_size }

let () = Random.self_init ()

let remove_entries idx keys =
  let rec loop idx size =
    if idx.current_size <= idx.config.max_size then idx
    else
      let last = size - 1 in
      Array.swap keys (Random.int size) last;
      let e = Map.find_exn idx.entries keys.(last) in
      remove_entry e;
      let idx = remove_from_index idx keys.(last) in
      loop idx (size - 1) in
  loop idx (Array.length keys)

let clean idx =
  Map.to_sequence idx.entries |>
  Seq.map ~f:fst |>
  Seq.to_array |>
  remove_entries idx

let run idx =
  if time_to_clean idx
  then clean idx
  else idx
