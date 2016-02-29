module File = struct
  open Option.Monad_infix

  let split_right ~after str =
    match String.rindex str after with
    | None -> str,None
    | Some n -> String.(subo ~len:n str, Some (subo ~pos:(n+1) str))

  let extension s = split_right ~after:'.' s |> snd

  (* input file format is [<name>.<fmt>[-<ver>]]. The `<fmt>` part is
     mandatory and should match with some reader, registered for Addr,
     data type. The `<ver>` part is optional, and defines format
     version. Example: "functions.sexp" or "functions.sexp-1.0".
     The example above will use sexp serializer to read data from file.
  *)
  let rooter filename =
    let name,ver = split_right ~after:'-' filename in
    extension name >>= fun fmt -> Addr.find_reader fmt >>| fun _ ->
    In_channel.with_file filename ~f:(fun chan ->
        Addr.Io.load_all ?ver ~fmt chan |>
        Seq.of_list |>
        create)
end
