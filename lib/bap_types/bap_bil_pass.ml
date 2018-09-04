open Core_kernel
open Regular.Std
open Bap_bil

type t = string
type pass = t

let passes : (bil -> bil) String.Table.t = String.Table.create ()
let selected : t list ref = ref []

let register_pass name f = Hashtbl.set passes name f
let select_passes ps = selected := ps
let selected_passes () =
  List.map !selected ~f:(Hashtbl.find_exn passes)
let passes () = Hashtbl.keys passes

module Printable = Printable.Make(struct
    type nonrec t = t
    let module_name = Some "Bap.Std.Bil.Pass"
    let pp fmt name = Format.fprintf fmt "%s" name
  end)
