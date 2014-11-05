open Core_kernel.Std
open Or_error
open Bap.Std

let string_of_perm s =
  let m f c = if f s then c else " " in
  String.concat Image.([
      m Sec.is_readable "R";
      m Sec.is_writable "W";
      m Sec.is_executable "X";
    ])

let main () =
  let r =
    Image.create Sys.argv.(1) >>| fun img -> begin
    let open Image in
    let bits = match addr_size img with
      | Word_size.W32 -> 32
      | Word_size.W64 -> 64 in
    let addrlen = bits / 4 in
    printf "File name:    %s\n" @@ filename img;
    printf "Architecture: %s\n" @@ Arch.to_string (arch img);
    printf "Address size: %d\n" bits;
    printf "Entry point:  %s\n" @@ Addr.to_string (entry_point img);
    printf "Symbols:\n";
    Sequence.iter (symbols img) ~f:(fun s ->
        printf "Symbol name: %s\n" @@
        Option.value ~default:"unknown" (Sym.name s);
        printf "Symbol data:\n%s\n" @@ Sym.hexdump s;

      );
    printf "Loadable sections: %d\n" @@
    Sequence.length (sections img);
    printf "\n";
    Sequence.iter (sections img) ~f:(fun s ->
        printf "Section name : %s\n" @@ Sec.name s;
        printf "Section start: %s\n" @@
        Addr.to_string @@ Sec.addr s;
        printf "Virtual size : 0x%0*X\n"  addrlen @@ Sec.size s;
        printf "Section perm : %s\n" @@ string_of_perm s;
        let mem = Sec.memory_exn s in
        Mem_exn.hexdump mem Format.str_formatter;
        printf "Section data:\n%s\n" (Format.flush_str_formatter ()));
  end in
  match r with
  | Ok () -> ()
  | Error err ->
    eprintf "Program failed with: %s\n" @@
    Error.to_string_hum err

let () =
  Plugins.load ();
  if Array.length Sys.argv = 2
  then main ()
  else eprintf "Usage: reading filename\n";
