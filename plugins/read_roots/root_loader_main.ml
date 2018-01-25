open Core_kernel.Std
open Bap.Std
open Bap_future.Std

include Self ()

module File = struct

  let split_right ~after str =
    match String.rindex str after with
    | None -> str,None
    | Some n -> String.(subo ~len:n str, Some (subo ~pos:(n+1) str))

  let extension s = split_right ~after:'.' s |> snd

  let print_roots r =
    let seq = Rooter.roots r in
    printf "roots(%d): " (Seq.length seq);
    Seq.iter seq ~f:(fun a -> printf "%s; " @@ Addr.to_string a);
    printf "\n";
    r

  let rooter filename =
    let name,ver = split_right ~after:'-' filename in
    let fmt = extension name in
    In_channel.with_file filename ~f:(fun chan ->
        Addr.Io.load_all ?ver ?fmt chan |>
        Seq.of_list |>
        Rooter.create)
end

let register filename =
  let source = Stream.map Project.Info.arch (fun arch ->
      Ok (File.rooter filename |> File.print_roots)) in
  Rooter.Factory.register "file" source

let () =
  let () = Config.manpage [
      `S "DESCRIPTION";
      `P
        "Read roots information from a file and provide a rooter under
     the name $(b,file). So print $(b,--rooter=file) to use only this
     rooter. Input file format is [<name>.<fmt>[-<ver>]]. If fmt is not
     specified the default reader will be used, otherwise the `<fmt>`
     part should match with some reader, registered for Addr module.
     The `<ver>` part is optional, and defines format version.
     Example: \"functions.sexp\" or \"functions.sexp-1.0.
     The example above will use sexp serializer to read data from file.";
      `S "SEE ALSO";
      `P "$(b,bap-plugin-objdump)(1), $(b,bap-plugin-byteweight)(1), $(b,bap-plugin-ida)(1)";
    ] in
  let file = Config.(param (some non_dir_file) "from" ~docv:"SYMS"
                           ~doc:"Use this file as roots source") in
  Config.when_ready (fun {Config.get=(!)} ->
      match !file with
      | Some file -> register file
      | None -> () )


(** TODO:
    Probably we need to define a file format without
    and infer size from arch. It seems it would be much better.
    I don't see any reasons we need Addr io here. *)
