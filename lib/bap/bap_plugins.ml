open Core_kernel.Std

let systems = [
  "bap.image"
]

let string_of_or_error = function
  | Ok () -> "ok"
  | Error err -> Error.to_string_hum err

let load () =
  List.iter systems
    ~f:(fun system -> List.iter (Bap_plugin.load ~system:"bap.image")
           ~f:(function
               | _, Ok () -> ()
               | name, Error err ->
                 eprintf "failed to load plugin %s of system %s: %s\n"
                   name system (Error.to_string_hum err)))
