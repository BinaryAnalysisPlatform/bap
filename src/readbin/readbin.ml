open Core_kernel.Std
open Or_error
open Bap.Std
open Format
open Options
open Image

module Program(Conf : Options.Provider) = struct
  open Conf

  let disassemble img mem =
    let syms = match options.symsfile with
      | Some filename ->
        Symtab.read ?demangle:options.demangle ~filename mem
      | None -> Table.map (symbols img) ~f:Symbol.name in
    let roots =
      Seq.(Table.regions syms >>| Memory.min_addr |> to_list) in
    let disasm = disassemble ~roots (arch img) mem in
    let module Env = struct
      let options = options
      let cfg = Disasm.blocks disasm
      let base = mem
      let syms = syms
      let arch = arch img
    end in
    let module Printing = Printing.Make(Env) in
    let module Helpers = Helpers.Make(Env) in
    let open Printing in
    let open Helpers in

    let pp_blk = List.map options.output_dump ~f:(function
        | `with_asm -> pp_blk Block.insns pp_insns
        | `with_bil -> pp_blk bil_of_block pp_bil) |> pp_concat in

    Tags.install std_formatter `Text;
    if options.output_dump <> [] then
      pp_code (pp_syms pp_blk) std_formatter syms;

    if options.output_phoenix <> None then
      let module Phoenix = Phoenix.Make(Env) in
      let dest = Phoenix.store () in
      printf "Phoenix data was stored in %s folder@." dest

  let main () =
    Image.create options.filename >>= fun (img,warns) ->
    List.iter warns ~f:(eprintf "Warning: %a\n" Error.pp);
    printf "%-20s: %a\n" "Arch"  Arch.pp (arch img);
    printf "%-20s: %a\n" "Entry" Addr.pp (entry_point img);
    printf "%-20s: %d\n" "Symbols" (Table.length (symbols img));
    printf "%-20s: %d\n" "Sections" (Table.length (sections img));
    Table.iteri (sections img) ~f:(fun mem s ->
        if Section.is_executable s then
          disassemble img mem);
    return (List.length warns)
end

let start options =
  let module Program = Program(struct
      let options = options
    end) in
  Program.main ()

let () =
  Printexc.record_backtrace true;
  Plugins.load ();
  match Cmdline.parse () >>= start with
  | Ok n -> exit n
  | Error err -> eprintf "%s" Error.(to_string_hum err);
    exit 1
