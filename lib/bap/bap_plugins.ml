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
    let load ?(systems=systems) () =
      List.iter systems
        ~f:(fun system ->
            List.iter (Bap_plugin.find_all ~system)
              ~f:(fun pkg -> match Bap_plugin.load pkg with
                  | Ok () -> ()
                  | Error err ->
                    eprintf
                      "failed to load plugin %s of system %s: %s\n"
                      (Bap_plugin.name pkg)
                      system
                      (Error.to_string_hum err)))
    let all () =
      List.map systems ~f:(fun s -> Bap_plugin.find_all ~system:s) |>
      List.concat
  end
end
