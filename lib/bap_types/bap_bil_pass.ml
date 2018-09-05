open Core_kernel
open Regular.Std
open Bap_bil

type t = {
  name : string;
  desc : string;
  body : bil -> bil;
}
type pass = t

let passes : t String.Table.t = String.Table.create ()
let selected : (bil -> bil) list ref = ref []

let pass_name t = t.name

let register_pass name ~desc body =
  Hashtbl.set passes name {name;desc;body}

let select_passes ps = selected := List.map ps ~f:(fun x -> x.body)
let selected_passes () = !selected
let passes () = Hashtbl.data passes

module Pass_pp = Printable.Make(struct
    type nonrec t = t
    let module_name = Some "Bap.Std.Bil.Pass"
    let pp fmt t =
      Format.fprintf fmt "%-24s %s" t.name t.desc
  end)
