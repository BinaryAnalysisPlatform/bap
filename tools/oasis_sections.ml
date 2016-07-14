let (/) = Filename.concat

let header = "common"

let has_extension name =
  String.contains name '.'

let enabled = "--enable-"
let disabled = "--disable-"

let feature enable arg =
  let length = String.length arg in
  let prefix = String.length enable in
  try
    if String.sub arg 0 prefix = enable
    then Some (String.sub arg prefix (length-prefix))
    else None
  with exn -> None

let everything_except disabled_args =
  Sys.readdir "oasis" |> Array.to_list |>
  List.filter (fun sec -> sec <> header && not (has_extension sec)
                          && not (List.mem sec disabled_args) )

let selected enable args =
  args |> List.fold_left (fun fs arg ->
      match feature enable arg with
      | Some f when Sys.file_exists ("oasis" / f) -> f :: fs
      | _ -> fs) []

let features args =
  let disabled_args = selected disabled args in
  if List.(mem "--enable-everything" args || mem "--help" args)
  then header :: everything_except disabled_args
  else header :: selected enabled args

let special_args =
  List.concat @@ List.map (fun f -> [enabled^f; disabled^f])
    ["docs"; "tests"; "llvm-static"; "debug"]

let filtered args =
  let enabled_args = selected enabled args in
  let disabled_args = selected disabled args in
  List.(filter (fun arg ->
      mem arg special_args ||
      not (mem arg (
          map (fun f -> enabled^f) enabled_args @
          map (fun f -> disabled^f) disabled_args))) args)

let main args =
  match args with
  | _ :: h :: args when h = "--args" ->
    let args =
      if List.(mem "--enable-everything" args || mem "--help" args)
      then filtered args
      else args in
    args |>
    String.concat "\n" |> print_endline
  | _ :: h :: args when h = "--sections" ->
    features args |>
    List.map (fun s -> "oasis" / s) |>
    String.concat " " |> print_endline
  | _ ->
    assert false

let () = main (Array.to_list Sys.argv)
