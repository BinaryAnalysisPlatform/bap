let package = "bap"
open Bap_core_theory
open Core_kernel

type t = Ogre.Doc.t [@@deriving compare]

module Stringable = struct
  type t = Ogre.doc
  let to_string = Ogre.Doc.to_string
  let of_string data = match Ogre.Doc.from_string data with
    | Ok doc -> doc
    | Error err ->
      failwithf "can't deserialize Ogre doc - %s"
        (Error.to_string_hum err) ()
end

let pp = Ogre.Doc.pp
include Sexpable.Of_stringable(Stringable)
include Binable.Of_stringable(Stringable) [@@warning "-D"]

type KB.Conflict.t += Spec_inconsistency of Error.t

let domain = KB.Domain.flat "spec"
    ~empty:Ogre.Doc.empty
    ~inspect:Ogre.Doc.sexp_of_t
    ~join:(fun d1 d2 -> match Ogre.Doc.merge d1 d2 with
        | Ok d -> Ok d
        | Error err ->
          Error (Spec_inconsistency err))
    ~equal:(fun d1 d2 -> Ogre.Doc.compare d1 d2 = 0)

let slot = KB.Class.property Theory.Unit.cls "unit-spec" domain
    ~package
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = Ogre.Doc.t [@@deriving bin_io]
                 end))

let () = KB.Conflict.register_printer @@ function
  | Spec_inconsistency err ->
    Some (Error.to_string_hum err)
  | _ -> None
