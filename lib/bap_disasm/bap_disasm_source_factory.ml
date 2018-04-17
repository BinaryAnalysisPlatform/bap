open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source_intf
open Bap_future.Std
open Bap_service

module Factory = struct
  module type S = Factory
  module Make(T : T) = struct
    type t = T.t

    let factory : (provider * t source)  String.Table.t = String.Table.create ()

    let register name source =
      match List.hd @@ Provider.select ~by_name:name () with
      | None -> failwith (sprintf "no providers declared under the name %s" name)
      | Some p -> Hashtbl.set factory ~key:name ~data:(p,source)

    let find name =
      match Hashtbl.find factory name with
      | None -> None
      | Some (_,s) -> Some s

    let list () = Hashtbl.keys factory

    let provide provider source =
      Hashtbl.set factory ~key:(Provider.name provider) ~data:(provider,source)

    let request p = find (Provider.name p)
    let providers () = Hashtbl.data factory |> List.map ~f:fst

  end
end
