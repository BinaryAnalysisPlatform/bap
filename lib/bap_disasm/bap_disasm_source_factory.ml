open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source_intf
open Bap_future.Std
open Bap_service

module Tab = Provider.Table

module Factory = struct
  module type S = Factory
  module Make(T : T) = struct
    type t = T.t
    let factory : t source Tab.t = Tab.create ()

    let register name source =
      match List.hd @@ Provider.select ~by_name:name () with
      | None -> failwith (sprintf "no providers declared under the name %s" name)
      | Some p -> Hashtbl.set factory ~key:p ~data:source

    let provide provider source =
      Hashtbl.set factory ~key:provider ~data:source

    let request = Hashtbl.find factory

    let list () = Hashtbl.keys factory |> List.map ~f:Provider.name

    let find name =
      List.find_map (Hashtbl.to_alist factory)
        ~f:(fun (p,s) ->
            Option.some_if (String.equal (Provider.name p) name) s)

    let providers () = Hashtbl.keys factory

  end
end
