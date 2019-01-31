open Core_kernel
open Bap_knowledge

module Dict = Univ_map
module Key = Dict.Key
type void
type uuid = (void,void,void) format

type 'a t = {
  key : 'a Key.t;
  init : 'a knowledge;
  uuid : string;
}

type 'a state = 'a t

let string_of_uuid uuid =
  let uuid = string_of_format uuid in
  match Uuidm.of_string uuid with
  | None -> invalid_argf "UUID: '%s' is not RFC4122 compliant" uuid ()
  | _ -> uuid

let declare ?(inspect=sexp_of_opaque) ~uuid ~name init = {
  key = Key.create ~name inspect;
  init;
  uuid = string_of_uuid uuid;
}

let name t = Key.name t.key
let inspect t x = Key.to_sexp t.key x

module Bag = struct
  type t = {
    dict : Dict.t;
    uids : int String.Map.t;
  }

  let empty = {
    dict = Dict.empty;
    uids = String.Map.empty;
  }

  let check_invariant t state =
    match Map.find t.uids state.uuid with
    | None -> ()
    | Some uid when uid = Key.hash state.key -> ()
    | Some _ ->
      invalid_argf "State invariant is broken - \
                    the same key witnesses different types.
            Key %s with uuid %s was created several times.
            The key must be created only once, consider moving the
            creation function from the scope of a functor."
        (name state) state.uuid ()


  let with_state t state ~ready ~create =
    check_invariant t state;
    match Dict.find t.dict state.key with
    | None -> create state.init
    | Some data -> ready data

  let set t state data =
    check_invariant t state;
    {
      uids = Map.set t.uids ~key:state.uuid
          ~data:(Key.hash state.key);
      dict = Dict.set t.dict state.key data;
    }
end
