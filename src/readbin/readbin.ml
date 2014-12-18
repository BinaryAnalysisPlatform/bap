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

let is_lifted =
  let open Arch in function
    | ARM -> true
    | _ -> false

let print_disasm arch s mem insn () =
  let open Disasm in
  printf "# insn: %s@;%s@."
    (Insn.asm insn)
    (Sexp.to_string (Insn.sexp_of_t insn));
  let () =
    if is_lifted arch then match Arm.Lift.insn mem insn with
      | Ok stmts -> printf "%a@.@." Stmt.pp_stmts stmts
      | Error err -> printf "@.failed to lift: %s@." @@
        Error.to_string_hum err in
  Basic.step s ()

let main () =
  Image.create Sys.argv.(1) >>= fun (img,warns) ->
  List.iter warns ~f:(fun w -> printf "Warning: %s\n" @@
                       Error.to_string_hum w);
  let open Image in
  let arch = arch img in
  let bits = match addr_size img with
    | `r32 -> 32
    | `r64 -> 64 in
  let target = match arch with
    | Arch.ARM -> "arm"
    | Arch.X86_32 -> "i386"
    | Arch.X86_64 -> "x86_64" in
  Disasm.Basic.create ~backend:"llvm" target >>= fun dis ->
  let name = Option.value (filename img) ~default:"<memory>" in
  printf "# File name:    %s\n" @@ name;
  printf "# Architecture: %s\n" @@ Arch.to_string arch;
  printf "# Address size: %d\n" bits;
  printf "# Entry point:  %s\n" @@ Addr.to_string (entry_point img);
  printf "# Symbols: (%d)\n" (Table.length (symbols img));
  Table.iteri (symbols img) ~f:(fun mem s ->
      printf "\n# Symbol name: %s\n" (Sym.name s);
      printf "# Symbol data:\n%a\n" Memory.pp mem;
      Disasm.Basic.run dis ~stop_on:[`valid]
        ~hit:(print_disasm arch) ~return:ident ~init:() mem);
  printf "# Loadable sections: %d\n" @@
  Table.length (sections img);
  Table.iteri (sections img) ~f:(fun mem s ->
      printf "Section name : %s\n" @@ Sec.name s;
      printf "Section start: %s\n" @@
      Addr.to_string @@ Memory.min_addr mem;
      printf "Section perm : %s\n" @@ string_of_perm s;
      printf "Section data:\n%a\n" Memory.pp mem );
  return (List.length warns)

let () =
  Printexc.record_backtrace true;
  let () = try
      Plugins.load ();
      if Array.length Sys.argv = 2
      then match main () with
        | Ok n -> exit n
        | Error err -> printf "Failed with: %s\n" @@ Error.to_string_hum err
      else printf "Usage: reading filename\n"
    with exn -> printf "Unhandled exception: %s : %s \n"
                  (Exn.to_string exn)
                  (Exn.backtrace ()) in
  exit (-1)
