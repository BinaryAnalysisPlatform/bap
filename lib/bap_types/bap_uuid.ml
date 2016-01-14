
open Core_kernel.Std

open Bap_common

module Bin = Bin_prot.Utils.Make_binable(struct
    module Binable = String
    type t = Uuidm.t
    let to_binable = Uuidm.to_bytes
    let of_binable s = match Uuidm.of_bytes s with
      | None -> invalid_arg "Bad UUID format"
      | Some uuid -> uuid
  end)

module Stringable = struct
  type t = Uuidm.t
  let of_string s = match Uuidm.of_string s with
    | None -> invalid_arg "Bad UUID format"
    | Some uuid -> uuid
  let to_string s = Uuidm.to_string s
end

module Sexp = Sexpable.Of_stringable(Stringable)

include Uuidm

include Regular.Make(struct
    let compare = Uuidm.compare
    include Bin
    include Sexp
    include Stringable
    let hash = Hashtbl.hash
    let module_name = Some "Bap.Std.Uuid"
    let version = "0.1"

    let pp ppf t = Uuidm.print ppf t
  end)

let of_string = Stringable.of_string
