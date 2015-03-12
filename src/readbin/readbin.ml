open Core_kernel.Std
open Or_error
open Bap.Std
open Bap_plugins.Std
open Format
open Options
open Program_visitor

module Program(Conf : Options.Provider) = struct
  open Conf

  let find_roots arch mem =
    if options.bw_disable then None
    else
      let module BW = Byteweight.Bytes in
      let path = options.sigfile in
      match Signatures.load ?path ~mode:"bytes" arch with
      | None ->
        eprintf "No signatures found@.Please, use `bap-byteweight' \
                 utility to fetch/create/install them.@.%!";
        None
      | Some data ->
        let bw = Binable.of_string (module BW) data in
        let length = options.bw_length in
        let threshold = options.bw_threshold in
        Some (BW.find bw ~length ~threshold mem)

  let rename_symbols subs syms : string table =
    Table.mapi syms ~f:(fun mem sym ->
        let addr = Memory.min_addr mem in
        match Table.find_addr subs addr with
        | Some (m,name) when Addr.(Memory.min_addr m = addr) -> name
        | _ -> sym)

  let annotate_symbols name syms map : (string * string) memmap =
    Table.foldi ~init:map syms ~f:(fun mem sym map ->
        Memmap.add map mem (name,sym))

  (* rhs is recovered, lhs is static.
     must be called after symbol renaming  *)
  let merge_syms lhs rhs : string table =
    Table.iteri rhs ~f:(fun m sym ->
        match Table.find lhs m with
        | None when options.verbose ->
          let inters = Table.intersections lhs m in
          Seq.iter inters ~f:(fun (m',sym') ->
              if sym = sym' then
                (* starting addresses are equal *)
                let diff = Memory.(length m - length m') in
                printf "Symbol %s is %s by %d bytes@."
                  sym (if diff < 0 then "shrinked" else "grown")
                  (abs diff)
              else
                let s = Memory.min_addr in
                let miss = Addr.(signed (s m - s m') |> to_int) in
                printf "Symbol %s@@%a => %s@@%a start missed by %d bytes@."
                  sym' Addr.pp (s m') sym Addr.pp (s m) (ok_exn miss))
        | _ -> ());
    Table.foldi lhs ~init:rhs ~f:(fun m' sym' rhs ->
        match Table.find rhs m' with
        | Some _ -> rhs
        | None ->
          match Table.add rhs m' sym' with
          | Ok rhs ->
            if options.verbose then
              printf "Symbol %s@@%a wasn't found, adding@."
                sym' Addr.pp (Memory.min_addr m');
            rhs
          | Error _ ->
            if options.verbose then
              printf "Symbol %s@@%a wasn't found correctly, skipping@."
                sym' Addr.pp (Memory.min_addr m');
            rhs)

  let roots_of_table t : addr list =
    Seq.(Table.regions t >>| Memory.min_addr |> to_list)

  let pp_addr f (mem,_) = Addr.pp f (Memory.min_addr mem)
  let pp_size f (mem,_) = fprintf f "%-4d" (Memory.length mem)
  let pp_name f (_,sym) = fprintf f "%-30s" sym

  let disassemble ?img arch mem =
    let demangle = options.demangle in
    let usr_syms = match options.symsfile with
      | Some filename ->
        Symbols.read ?demangle ~filename arch mem
      | None -> Table.empty in
    let ida_syms = match options.use_ida with
      | None -> Table.empty
      | Some ida ->
        let result =
          Ida.(with_file ?ida options.filename
                 (fun ida -> get_symbols ?demangle ida arch mem)) in
        match result with
        | Ok syms -> syms
        | Error err ->
          eprintf "Failed to get symbols from IDA: %a@."
            Error.pp err;
          Table.empty in
    let img_syms = match img with
      | Some img -> Table.map (Image.symbols img) ~f:Symbol.name
      | None -> Table.empty in
    let rec_roots =
      Option.value (find_roots arch mem) ~default:[] in
    let roots = List.concat [
        rec_roots;
        roots_of_table usr_syms;
        roots_of_table img_syms;
        roots_of_table ida_syms;
      ] in
    let disasm = disassemble ~roots arch mem in
    let cfg = Disasm.blocks disasm in
    let rec_syms = Symtab.create roots mem cfg in
    let syms = rec_syms |>
               rename_symbols ida_syms |>
               merge_syms ida_syms     |>
               rename_symbols img_syms |>
               merge_syms img_syms     |>
               rename_symbols usr_syms |>
               merge_syms usr_syms     in
    let annots =
      Option.value_map img ~default:Memmap.empty ~f:Image.tags |>
      annotate_symbols "found-symbol" rec_syms |>
      annotate_symbols "ida-symbol" ida_syms |>
      annotate_symbols "image-symbol" img_syms |>
      annotate_symbols "user-symbol" usr_syms in
    let project = {
      arch; memory = mem;
      annots;
      program = disasm;
      symbols = syms;
    } in
    List.iter options.plugins ~f:(fun name ->
        let name = if Filename.check_suffix name ".plugin" then
            name else (name ^ ".plugin") in
        match
          Plugin.create ~system:"program" name |> Plugin.load
        with Ok () -> ()
           | Error err -> eprintf "Failed to load plugin %s: %a@."
                            (Filename.basename name) Error.pp err);
    let project =
      List.fold ~init:project (Program_visitor.registered ())
        ~f:(fun project visit -> visit project) in

    let module Env = struct
      let options = options
      let cfg = Disasm.blocks project.program
      let base = project.memory
      let syms = project.symbols
      let arch = project.arch
    end in
    let module Printing = Printing.Make(Env) in
    let module Helpers = Helpers.Make(Env) in
    let open Printing in
    let open Helpers in

    let pp_sym = List.map options.print_symbols ~f:(function
        | `with_name -> pp_name
        | `with_addr -> pp_addr
        | `with_size -> pp_size) |> pp_concat ~sep:pp_print_space in

    printf "Found %d symbols@." (Table.length syms);
    if options.print_symbols <> [] then
      Table.iteri syms
        ~f:(fun mem sym -> printf "@[%a@]@." pp_sym (mem,sym));

    let pp_blk = List.map options.output_dump ~f:(function
        | `with_asm -> pp_blk Block.insns pp_insns
        | `with_bil -> pp_blk bil_of_block pp_bil) |> pp_concat in

    Text_tags.install std_formatter `Text;
    if options.output_dump <> [] then
      pp_code (pp_syms pp_blk) std_formatter syms;

    if options.verbose <> false then
      pp_errs std_formatter (Disasm.errors disasm);

    if options.output_phoenix <> None then
      let module Phoenix = Phoenix.Make(Env) in
      let dest = Phoenix.store () in
      printf "Phoenix data was stored in %s folder@." dest

  let main () =
    match options.binaryarch with
    | None ->
      Image.create options.filename >>= fun (img,warns) ->
      List.iter warns ~f:(eprintf "Warning: %a@." Error.pp);
      printf "%-20s: %s@." "File" options.filename;
      printf "%-20s: %a@." "Arch" Arch.pp (Image.arch img);
      printf "%-20s: %a@." "Entry" Addr.pp (Image.entry_point img);
      Table.iteri (Image.sections img) ~f:(fun mem s ->
          if Section.is_executable s then
            disassemble ~img (Image.arch img) mem);
      return (List.length warns)
    | Some s -> match Arch.of_string s with
      | None -> eprintf "unrecognized architecture\n"; return 1
      | Some arch ->
        printf "%-20s: %a\n" "Arch" Arch.pp arch;
        let width_of_arch = arch |> Arch.addr_size |> Size.to_bits in
        let addr = Addr.of_int ~width:width_of_arch 0 in
        match Memory.of_file (Arch.endian arch) addr options.filename with
        | Ok m -> disassemble arch m; return 0
        | Error e -> eprintf "failed to create memory: %s\n" (Error.to_string_hum e);
          return 1
end

let start options =
  let module Program = Program(struct
      let options = options
    end) in
  Program.main ()

let () =
  at_exit (pp_print_flush err_formatter);
  Printexc.record_backtrace true;
  Plugins.load ();
  match Cmdline.parse () >>= start with
  | Ok n -> exit n
  | Error err -> eprintf "%s" Error.(to_string_hum err);
    exit 1
