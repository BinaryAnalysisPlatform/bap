open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source_intf
open Bap_future.Std

module Tab = String.Table

module Factory = struct
  module type S = Factory
  module Make(T : T) = struct
    type t = T.t
    let factory : t source Tab.t = Tab.create ()

    let register name source =
      Hashtbl.set factory ~key:name ~data:source

    let list () = Hashtbl.keys factory
    let find = Hashtbl.find factory
  end
end
