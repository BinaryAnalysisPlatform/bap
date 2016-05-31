open Core_kernel.Std
open Bap_demangle.Std
open Cxxfilt_config


let demangle prog name =
  let command = sprintf "%s -p %s" prog name in
  let inp = Unix.open_process_in command in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  String.strip r


let run name =
  let demanglers =
    cxxfilt ::
    List.map targets ~f:(fun t -> t ^ "c++filt") in
  List.find_map demanglers ~f:(fun d ->
      let demangled = demangle d name in
      Option.some_if (demangled <> name) demangled) |> function
  | None -> name
  | Some name -> name


let () =
  let demangler = Demangler.create "c++filt" run in
  Demanglers.register demangler
