open Bin_prot.Std
type t = Bitvec.t
module Functions = Bin_prot.Utils.Make_binable(struct
    module Binable = struct
      type t = string [@@deriving bin_io]
    end
    type t = Bitvec.t
    let to_binable = Bitvec.to_binary
    let of_binable = Bitvec.of_binary
  end)
include Functions
