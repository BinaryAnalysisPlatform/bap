
module Std_bytes = Bytes

open Core_kernel.Std

module Binable = Bin_prot.Utils.Make_binable(struct
    module Binable = String
    type t = Std_bytes.t
    let to_binable = Std_bytes.unsafe_to_string
    let of_binable = Std_bytes.of_string 
  end)

module Stringable = struct
  type t = Std_bytes.t
  let of_string = Std_bytes.of_string
  let to_string = Std_bytes.unsafe_to_string
end

module Sexpable = Sexpable.Of_stringable(Stringable)

module T = struct
  include Std_bytes
  include Binable
  include Sexpable
end

module Elt = struct
  type t = char
  let equal t t' = Char.compare t t' = 0
  let of_bool b = if b then 'a' else 'b'
end

module Seq = struct
  type elt = Elt.t
  type t = T.t with sexp

  let create ~len = T.create len
  let length = T.length
  let get = T.get
  let set = T.set
  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len = 
    Std_bytes.blit src src_pos dst dst_pos len
end

module Blit = Blit.Make(Elt)(Seq)

module Identifiable = Identifiable.Make(struct
    type t = T.t with bin_io, compare, sexp
    let of_string = Stringable.of_string
    let to_string = Stringable.to_string
    let hash = Hashtbl.hash
    let module_name = "Bap bytes"
  end)

module C : Container.S0 with type t := T.t and type elt = char = struct

  type t = T.t
  type elt = char

  let convert     = T.unsafe_to_string
  let apply t f   = f (convert t)
  let length      = T.length 
  let fold t      = apply t String.fold
  let find t      = apply t String.find
  let iter t      = apply t String.iter
  let count t     = apply t String.count
  let min_elt t   = apply t String.min_elt
  let max_elt t   = apply t String.max_elt
  let exists t    = apply t String.exists
  let for_all t   = apply t String.for_all
  let to_array t  = apply t String.to_array
  let to_list t   = apply t String.to_list
  let find_map t  = apply t String.find_map
  let sum m t ~f  = apply t (fun s -> String.sum m s ~f)
  let is_empty t  = apply t String.is_empty 
  let mem ?(equal = (=)) t elt = String.mem ~equal (convert t) elt

end

include T
include C
include Blit
include Identifiable
