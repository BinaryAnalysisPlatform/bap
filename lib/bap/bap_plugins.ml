open Core_kernel.Std

module Std = struct
  module Plugin = Bap_plugin
  type plugin = Plugin.t

  let systems = [
    "bap.loader";
    "bap.disasm";
  ]

  let string_of_or_error = function
    | Ok () -> "ok"
    | Error err -> Error.to_string_hum err

  module Plugins = struct
    let load  () =
      Plugin.find_libraries () |>
      List.iter ~f:(fun pkg -> match Plugin.load pkg with
          | Ok () -> ()
          | Error err ->
            eprintf
              "failed to load plugin %s: %s\n"
              (Plugin.name pkg)
              (Error.to_string_hum err))
  end
end
