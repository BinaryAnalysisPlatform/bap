open Core_kernel.Std
open Regular_data_intf

type 'a t = {
  load : string -> 'a option;
  save : string -> 'a -> unit;
} with fields

let create ~load ~save = {load; save}

type service = {
  create : 'a . 'a reader -> 'a writer -> 'a t
}

module Service = struct
  let oblivion = {
    create = fun _ _ -> {
        load = (fun _ -> None);
        save = (fun _ _ -> ());
      }
  }

  let service = ref oblivion

  let provide new_service = service := new_service
  let request x y = !service.create x y
end
