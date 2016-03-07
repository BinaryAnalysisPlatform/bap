let (/) = Filename.concat

let header = "common"

let everything =
  Sys.readdir "oasis" |> Array.to_list |>
  List.filter (fun sec -> sec <> header)

let enable_feature arg =
  let length = String.length arg in
  let enable = "--enable-" in
  let prefix = String.length enable in
  try
    if String.sub arg 0 prefix = enable
    then Some (String.sub arg prefix (length-prefix))
    else None
  with exn -> None

let selected args =
  args |> List.fold_left (fun fs arg ->
      match enable_feature arg with
      | Some f when Sys.file_exists ("oasis" / f) -> f :: fs
      | _ -> fs) []

let features args =
  if List.mem "--enable-everything" args
  then header :: everything
  else header :: selected args

let main args =
  features args |>
  List.map (fun s -> "oasis" / s) |>
  String.concat " " |> print_endline

let () = main (Array.to_list Sys.argv)
