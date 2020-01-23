open Core_kernel
open Bap.Std
open Bap_demangle.Std
open Cxxfilt_config
include Self()

let () = Config.manpage [
    `S "DESCRIPTION";
    `P "A demangler that relies on a $(b,c++filt) utility";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-demangle)(1), $(b,bap-demangle)(3)"

  ]

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
      Option.some_if String.(demangled <> name) demangled) |> function
  | None -> name
  | Some name -> name


let () =
  Config.when_ready @@ fun _ ->
  let demangler = Demangler.create "c++filt" run in
  Demanglers.register demangler
