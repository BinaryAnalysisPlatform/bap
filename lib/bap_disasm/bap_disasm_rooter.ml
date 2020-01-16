open Bap_core_theory

open Core_kernel
open Bap_types.Std
open Bap_image_std

open KB.Syntax

module Source = Bap_disasm_source
module Insn = Bap_disasm_insn

type t = Rooter of addr seq
type rooter = t

let create seq = Rooter seq
let roots (Rooter seq) = seq
let union (Rooter r1) (Rooter r2) =
  Rooter (Seq.append r1 r2)
let empty = create Seq.empty

module Factory = Source.Factory.Make(struct type nonrec t = t end)

let of_image img =
  Image.symbols img |>
  Table.to_sequence |>
  Seq.map ~f:fst    |>
  Seq.map ~f:Memory.min_addr |>
  Seq.cons (Image.entry_point img) |>
  create

let of_blocks blocks =
  let roots = String.Table.create () in
  Seq.iter blocks ~f:(fun (name,sa,_) ->
      Hashtbl.change roots name (function
          | Some a when Addr.(a < sa) -> Some a
          | _ -> Some sa));
  create (Hashtbl.data roots |> Seq.of_list)

let provide =
  KB.Rule.(declare ~package:"bap.std" "reflect-rooter" |>
           dynamic ["rooter"] |>
           require Theory.Label.addr |>
           provide Theory.Label.is_subroutine |>
           comment "[Rooter.provide r] provides [r] to KB.");
  fun rooter ->
    let init = Set.empty (module Bitvec_order) in
    let roots =
      roots rooter |>
      Seq.map ~f:Word.to_bitvec |>
      Seq.fold ~init ~f:Set.add in
    KB.promise Theory.Label.is_subroutine @@ fun label ->
    KB.collect Theory.Label.addr label >>| function
    | None -> None
    | Some addr ->
      Option.some_if (Set.mem roots addr) true
