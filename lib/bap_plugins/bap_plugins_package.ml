let find_exn name =
  let dir = Findlib.package_directory name in
  let nat = if Dynlink.is_native then "native" else "byte" in
  let pre = ["plugin"; nat ] in
  let cmx = Findlib.package_property pre name "archive" in
  let file = Dynlink.adapt_filename cmx in
  Findlib.resolve_path ~base:dir file

let resolve name =
  try Some (find_exn name) with _ -> None

let list () = Findlib.list_packages' ()
