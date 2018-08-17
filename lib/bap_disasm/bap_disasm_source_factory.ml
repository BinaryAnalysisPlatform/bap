open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source_intf
open Bap_future.Std

module Factory = struct
  module type S = Factory
  module Make(T : T) = struct
    type t = T.t

    let factory : t source  String.Table.t = String.Table.create ()

    let find name = Hashtbl.find factory name
    let list () = Hashtbl.keys factory

    let register name source = Hashtbl.set factory ~key:name ~data:source
    let request p = find p
  end
end
