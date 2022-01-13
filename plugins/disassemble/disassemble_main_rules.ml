open Core_kernel
open Bap_core_theory
open Bap.Std
open Bap_main

include Loggers()

let addr_of_mem () =
  let open KB.Syntax in
  KB.Rule.(begin
      declare ~package:"bap" "addr-of-mem" |>
      require Memory.slot |>
      provide Theory.Label.addr |>
      comment "addr of the first byte"
    end);
  KB.promise Theory.Label.addr @@ fun label ->
  KB.collect Memory.slot label >>|? fun mem ->
  Some (Addr.to_bitvec (Memory.min_addr mem))

let code_of_mem () =
  KB.Rule.(begin
      declare ~package:"bap" "code-of-mem" |>
      require Memory.slot |>
      provide Theory.Semantics.code |>
      comment "extracts the memory contents"
    end);
  let open KB.Syntax in
  KB.promise Theory.Semantics.slot @@ fun label ->
  let+ mem = label-->?Memory.slot in
  let empty = KB.Value.empty Theory.Semantics.cls in
  KB.Value.put Theory.Semantics.code empty @@
  Option.some @@
  Bigsubstring.to_string @@
  Memory.to_buffer mem

let arch_of_unit () : unit =
  KB.Rule.(declare ~package:"bap" "arch-of-unit" |>
           require Theory.Label.unit |>
           require Arch.unit_slot |>
           provide Arch.slot |>
           comment "propagates arch from the unit");
  let open KB.Syntax in
  KB.promise Arch.slot @@ fun obj ->
  KB.collect Theory.Label.unit obj >>= function
  | None -> KB.return `unknown
  | Some unit -> KB.collect Arch.unit_slot unit

let asm_of_basic () : unit =
  let module Basic = Disasm_expert.Basic.Insn in
  KB.Rule.(begin
      declare ~package:"bap" "asm-of-basic" |>
      require Basic.slot |>
      provide Insn.Slot.asm |>
      comment "provides the assembly string";
    end);
  let open KB.Syntax in
  KB.promise Theory.Semantics.slot @@ fun label ->
  let+ insn = label-->?Basic.slot in
  KB.Value.put Insn.Slot.asm Theory.Semantics.empty @@
  Basic.asm insn

let provide_sequence_semantics () =
  let module Basic = Disasm_expert.Basic.Insn in
  let open KB.Syntax in
  KB.Rule.(begin
      declare "sequential-instruction" |>
      require Basic.slot |>
      provide Theory.Semantics.slot |>
      comment "computes sequential instructions semantics";
    end);
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Basic.slot obj >>= function
  | None -> !!Theory.Semantics.empty
  | Some insn when not (String.equal (Basic.name insn) "seq") ->
    !!Theory.Semantics.empty
  | Some insn -> match Basic.subs insn with
    | [||] -> !!Theory.Semantics.empty
    | subs ->
      Theory.instance () >>= Theory.require >>= fun (module CT) ->
      let subs = Array.to_list subs |>
                 List.map ~f:(fun sub ->
                     Insn.Seqnum.fresh >>| fun lbl ->
                     lbl,sub) in
      KB.all subs >>=
      KB.List.map ~f:(fun (obj,sub) ->
          KB.provide Basic.slot obj (Some sub) >>= fun () ->
          KB.collect Theory.Semantics.slot obj >>= fun sema ->
          let nil = Theory.Effect.empty Theory.Effect.Sort.bot in
          CT.seq (CT.blk obj !!nil !!nil) !!sema) >>=
      KB.List.reduce ~f:(fun s1 s2 -> CT.seq !!s1 !!s2) >>| function
      | None -> Insn.empty
      | Some sema -> Insn.with_basic sema insn

module Symbols = struct
  open KB.Let
  open KB.Syntax

  module Addr = struct
    include Bitvec
    include Bitvec_order
    include Bitvec_binprot.Functions
    include Bitvec_sexp.Functions
  end

  type table = {
    roots : Set.M(Addr).t;
    names : string Map.M(Addr).t;
    aliases : Set.M(String).t Map.M(Addr).t;
  } [@@deriving compare, equal, bin_io, sexp]

  let empty = {
    roots = Set.empty (module Addr);
    names = Map.empty (module Addr);
    aliases = Map.empty (module Addr);
  }

  let slot = KB.Class.property Theory.Unit.cls
      ~package:"bap" "symbol-table"
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = table [@@deriving bin_io]
                   end)) @@
    KB.Domain.flat ~empty ~equal:equal_table "symbols"

  let is_ident s =
    String.length s > 0 &&
    (Char.is_alpha s.[0] || Char.equal s.[0] '_') &&
    String.for_all s ~f:(fun c -> Char.is_alphanum c ||
                                  Char.equal c '_')

  let from_spec t =
    let collect fld = Ogre.collect Ogre.Query.(select @@ from fld) in
    let open Ogre.Let in
    let to_addr =
      let m = Bitvec.modulus (Theory.Target.code_addr_size t) in
      let n = Theory.Target.code_alignment t / Theory.Target.byte t in
      let mask = Int64.(lnot (of_int n - 1L)) in
      fun x ->
        let x = Int64.(x land mask) in
        Bitvec.(int64 x mod m) in
    let add_alias tab addr alias = {
      tab with aliases = Map.update tab.aliases addr ~f:(function
        | None -> Set.singleton (module String) alias
        | Some names -> Set.add names alias)
    } in
    let pp_comma ppf () = Format.pp_print_string ppf ", " in
    let pp_addrs =
      Format.pp_print_list ~pp_sep:pp_comma Bitvec.pp
    and pp_names =
      Format.pp_print_list ~pp_sep:pp_comma Format.pp_print_string in
    let* roots =
      let+ roots =
        let* starts = collect Image.Scheme.code_start in
        let* values = collect Image.Scheme.symbol_value in
        let+ entry = Ogre.request Image.Scheme.entry_point in
        let roots = Seq.append starts (Seq.map ~f:fst values) in
        match entry with
        | None -> roots
        | Some entry -> Seq.cons entry roots in
      Seq.fold roots ~init:(Set.empty (module Bitvec_order))
        ~f:(fun xs x -> Set.add xs (to_addr x)) in
    let+ named_symbols = collect Image.Scheme.named_symbol in
    let init = {empty with roots},
               Bap_relation.empty Bitvec.compare String.compare in
    Seq.fold named_symbols ~init ~f:(fun (tab,rel) (data,name) ->
        let addr = to_addr data in
        if Set.mem roots addr && is_ident name
        then tab,Bap_relation.add rel (to_addr data) name
        else add_alias tab addr name, rel) |> fun (table,rel) ->
    Bap_relation.matching rel table
      ~saturated:(fun k v t -> {
            t with names = Map.add_exn t.names k v
          })
      ~unmatched:(fun reason t -> match reason with
          | Non_injective_fwd (addrs,name) ->
            info "the symbol %s has ambiguous addresses: %a@\n"
              name pp_addrs addrs;
            t
          | Non_injective_bwd (names,addr) ->
            info "the symbol at %a has ambiguous names: %a@\n"
              Bitvec.pp addr pp_names names;
            t)

  let build_table t spec = match Ogre.eval (from_spec t) spec with
    | Ok x -> x
    | Error err ->
      invalid_argf "Malformed ogre specification: %s"
        (Error.to_string_hum err) ()

  let collect_inputs from obj f =
    KB.collect Theory.Label.unit obj >>=? fun unit ->
    KB.collect Theory.Label.addr obj >>=? fun addr ->
    let+ data = KB.collect from unit in
    f data addr

  let promise_table () : unit =
    KB.promise slot @@ fun unit ->
    let* t = KB.collect Theory.Unit.target unit in
    let+ s = KB.collect Image.Spec.slot unit in
    build_table t s

  let promise_roots () : unit =
    KB.Rule.(begin
        declare "provides roots" |>
        require Image.Spec.slot |>
        provide Theory.Label.is_subroutine |>
        comment "computes roots from spec";
      end);
    KB.promise Theory.Label.is_subroutine @@ fun obj ->
    collect_inputs slot obj @@ fun {roots} addr ->
    Option.some_if (Set.mem roots addr) true


  let names_agent = KB.Agent.register
      ~package:"bap" "specification-provider"
      ~desc:"provides names obtained from the image specification."

  let promise_names () : unit =
    KB.Rule.(begin
        declare "provides names" |>
        require Image.Spec.slot |>
        provide Theory.Label.possible_name |>
        comment "computes symbol names from spec";
      end);
    KB.propose names_agent Theory.Label.possible_name @@ fun obj ->
    collect_inputs slot obj @@ fun {names} addr ->
    Map.find names addr


  let promise_aliases () : unit =
    KB.Rule.(begin
        declare "provides aliases" |>
        require Image.Spec.slot |>
        provide Theory.Label.possible_name |>
        comment "computes symbol aliases (names) from spec";
      end);
    KB.promise Theory.Label.aliases @@ fun obj ->
    let* unit = KB.collect Theory.Label.unit obj in
    let* addr = KB.collect Theory.Label.addr obj in
    match unit,addr with
    | None,_|_,None -> KB.return (Set.empty (module String))
    | Some unit, Some addr ->
      let+ {aliases} = KB.collect slot unit in
      match Map.find aliases addr with
      | None -> Set.empty (module String)
      | Some aliases -> aliases

  let init () =
    promise_table ();
    promise_roots ();
    promise_names ();
    promise_aliases ()
end

let () = Extension.declare @@ fun _ctxt ->
  addr_of_mem ();
  code_of_mem ();
  arch_of_unit ();
  asm_of_basic ();
  Symbols.init ();
  provide_sequence_semantics ();
  Ok ()
