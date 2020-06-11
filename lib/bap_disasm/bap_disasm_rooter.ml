open Bap_core_theory

open Core_kernel
open Bap_types.Std
open Bap_image_std

open KB.Syntax

module Source = Bap_disasm_source
module Insn = Bap_disasm_insn

type t = {
  path : string option;
  roots : addr seq
}

type rooter = t

let merge_paths = Option.first_some

let create seq = {roots=seq; path=None}
let roots {roots=x} = x
let union x y = {
  path = merge_paths x.path y.path;
  roots = Seq.append x.roots y.roots;
}

let set_path r s = {r with path = Some s}
let path {path=x} = x

let empty = create Seq.empty

module Factory = Source.Factory.Make(struct type nonrec t = t end)

let of_image img = {
  path = Image.filename img;
  roots = Image.symbols img |>
          Table.to_sequence |>
          Seq.map ~f:fst    |>
          Seq.map ~f:Memory.min_addr |>
          Seq.cons (Image.entry_point img)
}

let of_blocks blocks =
  let roots = String.Table.create () in
  Seq.iter blocks ~f:(fun (name,sa,_) ->
      Hashtbl.change roots name (function
          | Some a when Addr.(a < sa) -> Some a
          | _ -> Some sa));
  create (Hashtbl.data roots |> Seq.of_list)

let is_applicable s path = match s.path, path with
  | None,_-> true
  | Some p, Some p' -> String.equal p p'
  | Some _, None -> false

let provide =
  KB.Rule.(declare ~package:"bap" "reflect-rooter" |>
           dynamic ["rooter"] |>
           require Theory.Label.addr |>
           require Theory.Label.path |>
           provide Theory.Label.is_subroutine |>
           comment "[Rooter.provide r] provides [r] to KB.");
  fun rooter ->
    let init = Set.empty (module Bitvec_order) in
    let roots =
      roots rooter |>
      Seq.map ~f:Word.to_bitvec |>
      Seq.fold ~init ~f:Set.add in
    KB.promise Theory.Label.is_subroutine @@ fun label ->
    KB.collect Theory.Label.path label >>= fun path ->
    KB.collect Theory.Label.addr label >>|? fun addr ->
    if is_applicable rooter path
    then Option.some_if (Set.mem roots addr) true
    else None
