open Core_kernel.Std
open Bap.Std
open Format
open Options

type 'a pp = formatter -> 'a -> unit

module Make(Env : sig val project : project end) = struct
  open Env

  let pp_blk_name fmt blk =
    let addr = Block.addr blk in
    let pos =
      Symtab.fns_of_addr (Project.symbols project) addr |>
      List.hd |> function
      | None -> sprintf "text_0x%s" (Addr.string_of_value addr)
      | Some fn ->
        let name = Symtab.name_of_fn fn in
        let entry = Symtab.entry_of_fn fn in
        let off = Addr.Int_exn.(addr - Block.addr entry) in
        if Word.is_zero off then sprintf "%s_ENTRY" name
        else sprintf "%s_0x%s" name (Word.string_of_value off) in
    fprintf fmt "%s" pos

  (** [pp_seq pp] prints a sequence using given printer [pp] *)
  let pp_seq pp fmt seq = Seq.iter seq ~f:(fprintf fmt "%a" pp)

  let rec pp_list ?(sep = pp_print_cut) pp_v ppf = function
    | [] -> ()
    | [v] -> pp_v ppf v
    | v :: vs ->
      pp_v ppf v;
      sep ppf ();
      pp_list ~sep pp_v ppf vs

  let pp_bil : bil pp = Bil.pp

  let pp_err fmt err = fprintf fmt "%a: %a" Memory.pp (fst err) Disasm.Error.pp (snd err)

  let pp_errs fmt errs = pp_seq pp_err fmt errs

  let pp_insn_line fmt (mem,insn) =
    pp_print_cut fmt ();
    pp_print_tab fmt ();
    Memory.pp fmt mem;
    pp_print_tab fmt ();
    Insn.pp fmt insn

  let pp_nothing _ () = ()
  let pp_insns = pp_list ~sep:pp_nothing pp_insn_line

  (** [pp_blk fmt blk] creates a basic block printer. The block is
      printed inside a 2 space indented vertical box *)
  let pp_blk proj pp fmt blk =
    let xs = proj blk in
    fprintf fmt
      "@{<(a (id %a))>@{<(pre (style %S))>%a@;@}@}"
      pp_blk_name blk "margin : 0" pp xs

  (** [pp_sym base cfg symtab] creates a printer for a symbol.
      The symbol is dentoned by its memory region, i.e., the
      printer has [formatter -> mem -> unit] type.  *)
  let pp_sym pp_blk fmt fn =
    let sym = Symtab.name_of_fn fn in
    let syms = Project.symbols project in
    let cfg = Project.disasm project |> Disasm.blocks in
    let bound = unstage (Symtab.create_bound syms fn) in
    let blks = Table.to_sequence cfg
               |> Seq.map ~f:snd
               |> Seq.filter ~f:(fun blk -> bound (Block.addr blk))  in
    fprintf fmt "@;@[<v2>@{<(div (id %s))>%a@}@]"
      sym (pp_seq pp_blk) blks

  (** [pp_sym base cfg] return a printer that will print a symtab,
      where symtab is [string table]. Each symbol would be printed
      as a set of basic blocks that has intersections with the memory
      occupied by the symbol. Block will be printed in an order of
      their starting addresses.  *)
  let pp_syms pp_blk fmt syms =
    pp_seq (pp_sym pp_blk) fmt (Symtab.to_sequence
                                  (Project.symbols project))

  let pp_concat ?(sep=pp_nothing) pps fmt x =
    List.map pps ~f:(fun pp -> fun fmt () -> pp fmt x) |>
    List.intersperse ~sep |>
    List.iter ~f:(fun pp -> pp fmt ())

  let setup_tab_stops fmt =
    pp_print_as fmt 6 "";
    pp_set_tab fmt ();
    pp_print_as fmt 25 "";
    pp_set_tab fmt ();
    pp_print_as fmt 20 "";
    pp_set_tab fmt ()

  (** prints a code as html document  *)
  let pp_code pp fmt v =
    pp_print_cut fmt ();
    pp_open_vbox fmt 0;
    pp_open_tbox fmt ();
    setup_tab_stops fmt;
    fprintf fmt
      "@;@{<html>@{<head>@{<(link
       (rel stylesheet)
       (type css)
       (href ../../../css/code-panel.css))>@}@}@{<body>%a@;@}@}" pp v;
    pp_close_tbox fmt ();
    pp_close_box fmt ();
    pp_print_flush fmt ()
end
