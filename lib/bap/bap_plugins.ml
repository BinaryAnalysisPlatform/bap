open Core_kernel.Std


let systems = [
  "bap.image"
]

let string_of_or_error = function
  | Ok () -> "ok"
  | Error err -> Error.to_string_hum err

let load () =
  List.iter systems
    ~f:(fun system -> List.iter (Bap_plugin.load_all ~system:"bap.image")
           ~f:(function
               | _, Ok () -> ()
               | pkg, Error err ->
                 eprintf "failed to load plugin %s of system %s: %s\n"
                   (Bap_plugin.name pkg)
                   system
                   (Error.to_string_hum err)))


let all () =
  List.map systems ~f:(fun s -> Bap_plugin.list ~system:s) |>
  List.concat
