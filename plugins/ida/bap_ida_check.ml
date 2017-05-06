open Core_kernel.Std
open Or_error

let require req check =
  if check
  then Ok ()
  else Or_error.errorf "IDA configuration failure: %s" req

let check_headless is_headless =
  require "can't use headless on windows"
    (is_headless ==> not Sys.win32)

let check_path ida_path =
  let (/) = Filename.concat in
  let exists = Sys.file_exists in
  let exists_sub sub = exists (ida_path / sub) in
  let is_dir = Sys.is_directory in
  Result.all_ignore [
    require "path must exist"       (exists ida_path);
    require "path must be a folder" (is_dir ida_path);
    require "idaq must exist"       (exists_sub "idaq");
    require "idaq64 must exist"     (exists_sub "idaq64");
    require "idal must exist"       (exists_sub "idal");
    require "idal64 must exist"     (exists_sub "idal64");
    require "bap-ida-python must be installed"
      (exists_sub ("plugins"/"plugin_loader_bap.py"))]
