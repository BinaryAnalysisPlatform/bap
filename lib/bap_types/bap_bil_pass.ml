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

let register_pass ?(desc="No description provided") name body =
  let pass = {name;desc;body} in
  Hashtbl.set passes name pass;
  pass

let select_passes ps = selected := List.map ps ~f:(fun x -> x.body)
let selected_passes () = !selected
let passes () = Hashtbl.data passes

module Pass_pp = struct
  let name p = p.name
  include Printable.Make(struct
      type nonrec t = t
      let module_name = Some "Bap.Std.Bil.Pass"
      let pp fmt t =
        Format.fprintf fmt "%-24s %s" t.name t.desc
    end)
end
