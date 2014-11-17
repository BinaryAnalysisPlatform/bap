open Core_kernel.Std
open Or_error
open Bap.Std
open Format

let string_of_perm s =
  let m f c = if f s then c else " " in
  String.concat Image.([
      m Sec.is_readable "R";
      m Sec.is_writable "W";
      m Sec.is_executable "X";
    ])

let main () =
  let r =
    Image.create Sys.argv.(1) >>| fun (img,warns) -> begin
    let open Image in
    let bits = match addr_size img with
      | Word_size.W32 -> 32
      | Word_size.W64 -> 64 in
    printf "File name:    %s\n" @@ filename img;
    printf "Architecture: %s\n" @@ Arch.to_string (arch img);
    printf "Address size: %d\n" bits;
    printf "Entry point:  %s\n" @@ Addr.to_string (entry_point img);
    printf "Symbols:\n";
    Table.iteri (symbols img) ~f:(fun mem s ->
        printf "Symbol name: %s\n" (Sym.name s);
        printf "Symbol data:\n%a\n" Memory.pp mem;
      );
    printf "Loadable sections: %d\n" @@
    Table.length (sections img);
    printf "\n";
    Table.iteri (sections img) ~f:(fun mem s ->
        printf "Section name : %s\n" @@ Sec.name s;
        printf "Section start: %s\n" @@
        Addr.to_string @@ Memory.min_addr mem;
        printf "Section perm : %s\n" @@ string_of_perm s;
        printf "Section data:\n%a\n" Memory.pp mem);
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
