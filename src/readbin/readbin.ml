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

let print_disasm s () =
  let open Disasm in
  List.iter (Basic.insns s) ~f:(fun (mem,insn) ->
      let addr = ok_exn (Addr.to_int64 (Memory.min_addr mem)) in
      printf "%08LX  " addr;
      match insn with
      | None -> printf "skipped\n"
      | Some insn ->
        printf "%-48s" (Sexp.to_string (Insn.sexp_of_t insn));
        printf "|%s\n" (Insn.asm insn));
  Basic.stop s ()

let main () =
  Image.create Sys.argv.(1) >>= fun (img,warns) ->
  List.iter warns ~f:(fun w -> printf "Warning: %s\n" @@
                       Error.to_string_hum w);
  let open Image in
  let bits = match addr_size img with
    | Word_size.W32 -> 32
    | Word_size.W64 -> 64 in
  let target = match arch img with
    | Arch.ARM -> "arm"
    | Arch.X86_32 -> "i386"
    | Arch.X86_64 -> "x86_64" in
  Disasm.Basic.create ~backend:"llvm" target >>= fun dis ->
  let dis = Disasm.Basic.store_asm dis in
  let dis = Disasm.Basic.store_kinds dis in
  printf "File name:    %s\n" @@ filename img;
  printf "Architecture: %s\n" @@ Arch.to_string (arch img);
  printf "Address size: %d\n" bits;
  printf "Entry point:  %s\n" @@ Addr.to_string (entry_point img);
  printf "Symbols: (%d)\n" (Table.length (symbols img));
  Table.iteri (symbols img) ~f:(fun mem s ->
      printf "\nSymbol name: %s\n" (Sym.name s);
      printf "Symbol data:\n%a\n" Memory.pp mem;
      Disasm.Basic.run dis
        ~stopped:print_disasm ~return:ident ~init:() mem);
  printf "Loadable sections: %d\n" @@
  Table.length (sections img);
  Table.iteri (sections img) ~f:(fun mem s ->
      printf "Section name : %s\n" @@ Sec.name s;
      printf "Section start: %s\n" @@
      Addr.to_string @@ Memory.min_addr mem;
      printf "Section perm : %s\n" @@ string_of_perm s;
      printf "Linear sweep :\n";
      Disasm.Basic.run dis
        ~stopped:print_disasm ~return:ident ~init:() mem;
      printf "Section data:\n%a\n" Memory.pp mem);
  return (List.length warns)

let () =
  let () = try
      Plugins.load ();
      if Array.length Sys.argv = 2
      then match main () with
        | Ok n -> exit n
        | Error err -> printf "Failed with: %s\n" @@ Error.to_string_hum err
      else printf "Usage: reading filename\n"
    with exn -> printf "Unhandled exception: %s\n" (Exn.to_string exn) in
  exit (-1)
