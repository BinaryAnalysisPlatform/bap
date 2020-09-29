open Core_kernel
open Bap_main
open Bap_knowledge
open Bap_core_theory
open Bap.Std

open KB.Syntax

include Loggers()

let read_name name =
  KB.Symbol.package >>| fun package ->
  KB.Name.to_string (KB.Name.read ~package name)


let print_semantics label slots =
  KB.collect Theory.Semantics.slot label >>= fun sema ->
  KB.List.map slots ~f:read_name >>| fun slots ->
  match slots with
  | [] -> Format.printf "%a@\n" KB.Value.pp sema
  | slots -> Format.printf "%a@\n" (KB.Value.pp_slots slots) sema

let require prop label has =
  KB.collect prop label >>= function
  | None -> KB.return false
  | Some p -> has p

let belongs_to_unit unit insn =
  match unit with
  | None -> KB.return true
  | Some unit ->
    require Theory.Label.unit insn @@ fun unit' ->
    KB.return @@ Theory.Unit.equal unit unit'

let belongs_to_subr subr insn =
  match subr with
  | None -> KB.return true
  | Some subr ->
    require Theory.Label.addr subr @@ fun subr ->
    require Theory.Label.unit insn @@ fun unit ->
    require Theory.Label.addr insn @@ fun insn ->
    KB.collect Theory.Unit.target unit >>|
    Theory.Target.code_addr_size >>= fun bits ->
    KB.collect Project.State.slot unit >>| fun state ->
    let subrs = Project.State.subroutines state in
    let insn = Word.create insn bits
    and subr = Word.create subr bits in
    Disasm.Subroutines.belongs subrs ~entry:subr insn

let find_entry subrs name_or_addr =
  Disasm.Subroutines.entries subrs |>
  Set.to_sequence |>
  KB.Seq.find ~f:(fun addr ->
      let addr = Word.to_bitvec addr in
      if Bitvec.to_string addr = name_or_addr
      then KB.return true
      else Theory.Label.for_addr addr >>= fun label ->
        KB.collect Theory.Label.name label >>| function
        | None -> false
        | Some name -> String.equal name name_or_addr)

let iter_subr entry subrs disasm ~f =
  Disasm.Driver.explore disasm ~entry ~init:()
    ~follow:(fun next ->
        KB.return @@
        Disasm.Subroutines.belongs subrs ~entry next)
    ~block:(fun _mem -> Disasm.Driver.execution_order)
    ~node:(fun insns () -> KB.List.iter insns ~f)
    ~edge:(fun _ _ _ -> KB.return ())

let print_unit () =
  KB.objects Theory.Unit.cls >>=
  KB.Seq.iter ~f:(fun obj ->
      KB.Object.repr Theory.Unit.cls obj >>= fun str ->
      KB.collect Theory.Unit.target obj >>| fun triple ->
      Format.printf "%-40s %a@\n" str Theory.Target.pp triple)

let ensure x yes =
  x >>= function
  | true -> yes ()
  | false -> KB.return ()

let in_package package f = match package with
  | None -> f ()
  | Some package -> KB.Symbol.in_package package f

let print_insn ?package slots obj =
  in_package package @@ fun () ->
  KB.Object.repr Theory.Program.cls obj >>= fun str ->
  match slots with
  | None ->
    Format.printf "%s@\n" str;
    KB.return ()
  | Some slots ->
    Format.printf "%s@\n" str;
    print_semantics obj slots

let list_insns unit slots =
  KB.objects Theory.Program.cls >>=
  KB.Seq.iter ~f:(fun obj ->
      ensure (belongs_to_unit unit obj) @@ fun () ->
      print_insn slots obj)

let print_subr unit name_or_addr slots =
  KB.collect Project.State.slot unit >>= fun state ->
  KB.Symbol.package >>= fun current ->
  KB.collect Theory.Unit.path unit >>= fun package ->
  in_package package @@ fun () ->
  let sub = Project.State.subroutines state
  and dis = Project.State.disassembly state in
  find_entry sub name_or_addr >>= function
  | None -> KB.return ()
  | Some entry ->
    iter_subr entry sub dis ~f:(print_insn ~package:current slots)

let matches name filter = match filter with
  | None -> true
  | Some prefix ->
    String.is_prefix ~prefix name

let list_subrs unit filter =
  KB.objects Theory.Program.cls >>=
  KB.Seq.iter ~f:(fun obj ->
      belongs_to_unit unit obj >>= function
      | false -> KB.return ()
      | true ->
        KB.collect Theory.Label.is_subroutine obj >>= function
        | None | Some false -> KB.return ()
        | Some true ->
          KB.Object.repr Theory.Program.cls obj >>= fun str ->
          KB.collect Theory.Label.name obj >>| function
          | None when Option.is_none filter ->
            Format.printf "%s: unresolved@\n" str
          | Some name when matches name filter ->
            Format.printf "%s: %s\n" str name
          | _ -> () )

let register () =
  let open Project.Analysis in
  let package = "bap" in

  register ~package "instruction"
    (args program $ rest string) print_semantics
    ~desc:"Prints the instruction semantics. Prints the semantics of \
           an instruction with the given label. If no fields are \
           specified, then prints all properties in the semantics \
           object. Otherwise, prints only the specified fields.";

  register ~package "instructions"
    (args @@
     keyword "unit" unit $
     keyword "semantics" (rest string)) list_insns
    ~desc:"Prints all instructions. If :unit is specified then prints \
           only instructions that belong to that unit. If :semantics \
           is specified then prints the semantics of each \
           instruction. The :semantics keyword could be followed by \
           one or more field names. If no fields are specified, then \
           all properties of an instruction will be printed, otherwise \
           only those that are specified will be printed.";

  register ~package "units"
    (args empty) print_unit
    ~desc:"Prints all units. Prints all units stored in the knowledge \
           base." ;

  register ~package "subroutines"
    (args @@
     keyword "unit" unit $
     keyword "matches" string)
    list_subrs
    ~desc:"Prints all subroutines. If :unit is specified, then prints \
           only subroutines of that unit. If :matches is specified, \
           then prints only subroutines that match the name." ;

  register ~package "subroutine"
    (args @@ unit $ string $ keyword "semantics" (rest string))
    print_subr
    ~desc:"Prints a subroutine. Prints the subroutine in the \
           specified unit with the given name or address. If \
           :semantics is specified, then prints the semantics of each \
           instruction. The :semantics keyword could be followed by \
           one or more field names. If no fields are specified, then \
           all properties of an instruction will be printed, \
           otherwise only those that are specified will be printed.";
