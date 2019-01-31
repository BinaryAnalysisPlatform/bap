open Core_kernel
open Regular.Std
open Bap.Std
open Format

let run dir (module Data : Data.S ) =
  let fmts = match dir with
    | `readers -> Data.available_readers
    | `writers -> Data.available_writers in
  fmts () |> List.fold ~init:String.Map.empty
    ~f:(fun fmts (name, `Ver ver, desc) ->
        Map.add_multi fmts ~key:name ~data:(ver,desc)) |>
  Map.iteri ~f:(fun ~key:name ~data:desc ->
      match desc with
      | [] -> ()
      | (v,desc) :: rest ->
        let vs = v :: List.map ~f:fst rest |>
                 String.concat ~sep:", " |>
                 sprintf "(%s)" in
        let desc = Option.value desc
            ~default: "No description provided" in
        printf "%-12s %-16s %s@." name vs desc)
