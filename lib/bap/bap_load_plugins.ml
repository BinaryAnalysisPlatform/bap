open Core_kernel.Std
open Bap_plugins.Std

let () =
  let loader = Topdirs.dir_load Format.std_formatter in
  setup_dynamic_loader loader;
  Plugins.load () |> ignore


