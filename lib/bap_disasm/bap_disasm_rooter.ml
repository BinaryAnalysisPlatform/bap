open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

module Source = Bap_disasm_source

type t = Rooter of addr seq
type rooter = t

let create seq = Rooter seq
let roots (Rooter seq) = seq
let union (Rooter r1) (Rooter r2) =
  Rooter (Seq.append r1 r2)
let empty = create Seq.empty

module Factory = Source.Factory(struct type nonrec t = t end)

let of_image img =
  Image.symbols img |>
  Table.to_sequence |>
  Seq.map ~f:fst    |>
  Seq.map ~f:Memory.min_addr |>
  create

let of_blocks blocks =
  let roots = String.Table.create () in
  Seq.iter blocks ~f:(fun (name,sa,_) ->
      Hashtbl.change roots name (function
          | Some a when Addr.(a < sa) -> Some a
          | _ -> Some sa));
  create (Hashtbl.data roots |> Seq.of_list)



let () =
  Factory.register Source.Binary "internal" (fun img -> Some (of_image img))
