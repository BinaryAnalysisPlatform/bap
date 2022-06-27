open Bap_core_theory

open Core_kernel[@@warning "-D"]
open Bap_types.Std
open Bap_image_std

open Bap_disasm_source
module Context = Bap_disasm_source.Context

open KB.Syntax

module Insn = Bap_disasm_insn

type t = {
  path : string option;
  roots : addr seq;
  biased : bool;
}

type rooter = t

let merge_paths = Option.first_some

let create seq = {roots=seq; path=None; biased=false}
let roots {roots=x} = x
let union x y = {
  path = merge_paths x.path y.path;
  roots = Seq.append x.roots y.roots;
  biased = x.biased || y.biased;
}

let set_path r s = {r with path = Some s}
let path {path=x} = x

let empty = create Seq.empty

module Factory = Factory.Make(struct type nonrec t = t end)

let of_image img = {
  path = Image.filename img;
  roots = Image.symbols img |>
          Table.to_sequence |>
          Seq.map ~f:fst    |>
          Seq.map ~f:Memory.min_addr |>
          Seq.cons (Image.entry_point img);
  biased = true;
}

let of_blocks blocks =
  let roots = String.Table.create () in
  Seq.iter blocks ~f:(fun (name,sa,_) ->
      Hashtbl.change roots name (function
          | Some a when Addr.(a < sa) -> Some a
          | _ -> Some sa));
  create (Hashtbl.data roots |> Seq.of_list)

let make_provider rooter =
  let init = Set.empty (module Word) in
  let roots =
    roots rooter |>
    Seq.fold ~init ~f:Set.add in
  fun label ->
    KB.collect Theory.Label.addr label >>=? fun addr ->
    Context.for_label label >>| fun ctxt ->
    if Context.is_applicable ctxt rooter.path
    then
      let addr =
        Context.create_addr ctxt ~unbiased:(not rooter.biased) addr in
      Option.some_if (Set.mem roots addr) true
    else None

let provide =
  KB.Rule.(declare ~package:"bap" "reflect-rooter" |>
           dynamic ["rooter"] |>
           require Theory.Label.addr |>
           require Theory.Label.unit |>
           require Theory.Unit.path |>
           provide Theory.Label.is_subroutine |>
           comment "[Rooter.provide r] provides [r] to KB.");
  fun rooter ->
    let rooter = make_provider rooter in
    KB.promise Theory.Label.is_subroutine @@ rooter

let providing =
  fun rooter ->
  KB.promising Theory.Label.is_subroutine ~promise:(make_provider rooter)
