open Core_kernel
open Bap_core_theory
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source
open KB.Syntax

include Bap_main.Loggers()

type t = {
  path : string option;
  find : addr -> string option;
  biased : bool;
}

type symbolizer = t

let name_of_addr addr =
  sprintf "sub_%s" @@ Addr.string_of_value addr

module Name = struct
  let is_empty name =
    String.is_prefix name ~prefix:"sub_"

  let order x y : KB.Order.partial =
    match is_empty x, is_empty y with
    | true,true -> EQ
    | false,false -> if String.equal x y then EQ else NC
    | true,false -> LT
    | false,true -> GT
end

let create find = {
  path = None;
  find;
  biased = false;
}

let path s = s.path
let set_path s p = {s with path = Some p}

let run {find} a = find a
let resolve sym addr = match run sym addr with
  | Some name -> name
  | None -> name_of_addr addr

let empty = create (fun _ -> None)

let chain ss =
  create (fun addr -> List.find_map ss ~f:(fun s -> run s addr))

let string_of_addr addrs =
  List.map addrs ~f:Addr.to_string |>
  String.concat ~sep:", "

let report_broken = function
  | Bap_relation.Non_injective_fwd (addrs,name) ->
    info "skipping (%s) as they all have the same name %s"
      (string_of_addr addrs) name
  | Bap_relation.Non_injective_bwd (names,addr) ->
    info "skipping (%s) as they all have the same address %a"
      (String.concat names ~sep:", ") Addr.pp addr

let of_image img =
  let symtab = Image.symbols img in
  let names = Addr.Table.create () in
  let init = Bap_relation.empty Addr.compare String.compare in
  Table.foldi symtab ~init ~f:(fun mem sym rels ->
      let name = Image.Symbol.name sym
      and addr = Memory.min_addr mem in
      if not (Name.is_empty name)
      then Bap_relation.add rels addr name
      else rels) |> fun rels ->
  Bap_relation.matching rels ()
    ~saturated:(fun addr name () -> Hashtbl.add_exn names addr name)
    ~unmatched:(fun reason () -> report_broken reason);
  {find = Hashtbl.find names; path=Image.filename img; biased=true}

let of_blocks seq =
  let names =
    let empty_rel = Bap_relation.empty Addr.compare String.compare in
    Seq.fold seq ~init:String.Map.empty ~f:(fun addrs (name,addr,_) ->
        Map.update addrs name (function
            | Some addr' -> Addr.min addr addr'
            | None -> addr)) |>
    Map.to_sequence |>
    Seq.fold ~init:empty_rel ~f:(fun rels (name,entry) ->
        Bap_relation.add rels entry name) |> fun rels ->
    Bap_relation.matching rels Addr.Map.empty
      ~saturated:(fun addr name names -> Map.add_exn names addr name)
      ~unmatched:(fun reason names -> report_broken reason; names) in
  create @@ Map.find names

module Factory = Factory.Make(struct type nonrec t = t end)

let provide_symbolizer s label =
  let open KB.Syntax in
  KB.collect Theory.Label.addr label >>=? fun addr ->
  Context.for_label label >>| fun ctxt ->
  if Context.is_applicable ctxt s.path
  then
    s.find @@ Context.create_addr ctxt ~unbiased:(not s.biased) addr
  else None


let provide =
  KB.Rule.(declare ~package:"bap" "reflect-symbolizers" |>
           dynamic ["symbolizer"] |>
           require Arch.slot |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require Theory.Unit.path |>
           require Theory.Unit.bias |>
           provide Theory.Label.possible_name |>
           comment "[Symbolizer.provide s] reflects [s] to KB.");
  fun agent s ->
    KB.propose agent Theory.Label.possible_name @@
    provide_symbolizer s

let providing agent s =
  KB.proposing agent Theory.Label.possible_name
    ~propose:(provide_symbolizer s)

let get_name addr =
  Theory.Label.for_addr (Word.to_bitvec addr) >>= fun label ->
  KB.collect Theory.Label.name label >>| function
  | None -> name_of_addr addr
  | Some name -> name

module Toplevel = struct
  let name = Toplevel.var "symbol-name"
  let get_name addr =
    Toplevel.put name (get_name addr);
    Toplevel.get name
end
