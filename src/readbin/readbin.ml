open Core_kernel.Std
open Or_error
open Bap.Std
open Bap_plugins.Std
open Format
open Options
open Project

module Program(Conf : Options.Provider) = struct
  open Conf

  let paths_of_env () =
    try Sys.getenv "BAP_PLUGIN_PATH" |> String.split ~on:':'
    with Not_found -> []

  (** [create_plugin system name file] if file is not [None]
      then create a plugin targeting this file, otherwise
      search for the plugin with a given [system] and [name]
      if a system. Return an error, if nothing found.  *)
  let create_plugin system name = function
    | Some name -> Ok (Plugin.create ~system name)
    | None ->
      Plugin.find_all ~system |>
      List.filter ~f:(fun p -> Plugin.name p = name) |> function
      | [] ->
        errorf "Failed to find plugin in path or in system, \
                try to use -L option or set \
                BAP_PLUGIN_PATH environment variable"
      | _ :: _ :: _ ->
        errorf "The plugin name is ambigious, as I found more \
                than one plugin named '%s' in your system." name
      | [p] -> Ok p

  (** [load_plugin name] if [name] or [name.plugin] points to a
      file then load it, otherwise search for the plugin of a
      system "bap.project" with the given [name] using findlib.
      Bail-out with error if nothing found.
      Once plugin is loaded it is checked that it registered itself
      under the system. *)
  let load_plugin name =
    let system = "bap.project" in
    let name =
      if Filename.check_suffix name ".plugin"
      then name else name ^ ".plugin" in
    let before = Project.plugins () |> List.length in
    let paths = [
      [FileUtil.pwd ()]; paths_of_env (); options.load_path
    ] |> List.concat in
    List.find_map paths ~f:(fun dir ->
        let path = Filename.concat dir name in
        Option.some_if (Sys.file_exists path) path) |>
    create_plugin system name >>= Plugin.load >>= fun () ->
    if List.length (Project.plugins ()) = before
    then errorf "Plugin %s didn't register itself" name
    else return ()

  let prepare_args argv name =
    let prefix = "--" ^ name ^ "-" in
    Array.filter_map argv ~f:(fun arg ->
        if arg = argv.(0) then Some name
        else match String.chop_prefix arg ~prefix with
          | None -> None
          | Some arg -> Some ("--" ^ arg))

  type bound = [`min | `max] with sexp
  type spec = [`name | bound] with sexp

  type subst = [
    | `region of spec
    | `symbol of spec
    | `memory of bound
    | `block of bound
    | `asm
    | `bil
  ] with sexp

  let subst_of_string = function
    | "region" | "region_name" -> Some (`region `name)
    | "region_addr" | "region_min_addr" -> Some (`region `min)
    | "region_max_addr" -> Some (`region `max)
    | "symbol" | "symbol_name" -> Some (`symbol `name)
    | "symbol_addr" | "symbol_min_addr" -> Some (`symbol `min)
    | "symbol_max_addr" -> Some (`symbol `max)
    | "block_addr" | "block_min_addr" -> Some (`block `min)
    | "block_max_addr" -> Some (`block `max)
    | "min_addr" | "addr" -> Some (`memory `min)
    | "max_addr" -> Some (`memory `max)
    | _ -> None

  let addr which mem =
    let take = match which with
      | `min -> Memory.min_addr
      | `max -> Memory.max_addr in
    sprintf "0x%s" @@ Addr.string_of_value (take mem)

  let substitute project =
    let find_tag tag mem =
      Memmap.dominators project.memory mem |>
      Seq.find_map ~f:(fun (mem,v) -> match Tag.value tag v with
          | Some reg -> Some (mem,reg)
          | None -> None) in
    let find_region = find_tag Image.region in
    let subst_region (mem,name) = function
      | #bound as b -> addr b mem
      | `name -> name in
    let find_symbol mem =
      Table.find_addr project.symbols (Memory.min_addr mem) in
    let apply_subst find mem subst spec value =
      match find mem with
      | Some thing -> subst thing spec
      | None -> value in
    let find_block mem =
      Table.find_addr (Disasm.blocks project.disasm)
        (Memory.min_addr mem) in
    let subst_block (mem,_) spec = addr spec mem in
    let asm insn = Insn.asm insn in
    let bil insn = asprintf "%a" Bil.pp (Insn.bil insn) in
    let disasm mem out =
      let inj = match out with `asm -> asm | `bil -> bil in
      disassemble project.arch mem |> Disasm.insns |>
      Seq.map ~f:(fun (_,insn) -> inj insn) |> Seq.to_list |>
      String.concat ~sep:"\n" in
    let sub mem x =
      let buf = Buffer.create (String.length x) in
      Buffer.add_substitute buf (fun x -> match subst_of_string x with
          | Some (`region spec) ->
            apply_subst find_region mem subst_region spec x
          | Some (`symbol spec) ->
            apply_subst find_symbol mem subst_region spec x
          | Some (`memory bound) -> addr bound mem
          | Some (`block bound) ->
            apply_subst find_block mem subst_block bound x
          | Some (`bil | `asm as out) -> disasm mem out
          | None -> x) x;
      Buffer.contents buf in
    let memory = Memmap.mapi project.memory ~f:(fun mem value ->
        let tagval =
          List.find_map [text; html; comment; python; shell]
            ~f:(fun tag -> match Tag.value tag value with
                | Some value -> Some (tag,value)
                | None -> None) in
        match tagval with
        | Some (tag,value) -> Tag.create tag (sub mem value)
        | None -> value) in
    {project with memory}

  let find_roots arch mem =
    if options.bw_disable then None
    else
      let module BW = Byteweight.Bytes in
      let path = options.sigfile in
      match Signatures.load ?path ~mode:"bytes" arch with
      | None ->
        eprintf "No signatures found@.Please, use `bap-byteweight update' \
                 to get the latest available signatures.@.%!";
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
    let memory =
      Option.value_map img ~default:Memmap.empty ~f:Image.memory in
    let memory =
      Table.foldi syms ~init:memory ~f:(fun mem sym map ->
          Memmap.add map mem (Tag.create Image.symbol sym)) in

    List.iter options.plugins ~f:(fun name ->
        match load_plugin name with
        | Ok () -> ()
        | Error err ->
          let msg = asprintf "Failed to load plugin %s"
              (Filename.basename name) in
          Error.raise (Error.tag err msg));

    let project =
      List.fold2_exn options.plugins (Project.plugins ()) ~init:{
        arch; disasm; memory; storage = String.Map.empty;
        symbols = syms; base = mem
      } ~f:(fun p name f -> f (prepare_args Sys.argv name) p) |>
      substitute in

    Option.iter options.emit_ida_script (fun dst ->
        Out_channel.write_all dst
          ~data:(Idapy.extract_script project.memory));

    let module Target = (val target_of_arch arch) in
    let module Env = struct
      let options = options
      let cfg = Disasm.blocks project.disasm
      let base = project.base
      let syms = project.symbols
      let arch = project.arch
      module Target = Target
    end in
    let module Printing = Printing.Make(Env) in
    let module Helpers = Helpers.Make(Env) in
    let open Printing in

    let pp_sym = List.map options.print_symbols ~f:(function
        | `with_name -> pp_name
        | `with_addr -> pp_addr
        | `with_size -> pp_size) |> pp_concat ~sep:pp_print_space in

    if options.print_symbols <> [] then
      Table.iteri syms
        ~f:(fun mem sym -> printf "@[%a@]@." pp_sym (mem,sym));

    let pp_blk = List.map options.output_dump ~f:(function
        | `with_asm -> pp_blk Block.insns pp_insns
        | `with_bil -> pp_blk Helpers.bil_of_block pp_bil) |> pp_concat in

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
      Image.create ~backend:options.loader options.filename >>=
      fun (img,warns) ->
      if options.verbose then
        List.iter warns ~f:(eprintf "Warning: %a@." Error.pp);
      Table.iteri (Image.sections img) ~f:(fun mem s ->
          if Section.is_executable s then
            disassemble ~img (Image.arch img) mem);
      return 0
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
  match try_with_join (fun () -> Cmdline.parse () >>= start) with
  | Ok n -> exit n
  | Error err -> eprintf "%a@." Error.pp err;
    exit 1
