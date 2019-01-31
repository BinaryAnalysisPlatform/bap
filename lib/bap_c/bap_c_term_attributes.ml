open Core_kernel
open Bap.Std
open Format


module Data = struct
  include Bap_c_data
  let pp_value ppf = function
    | Top -> fprintf ppf "Top"
    | Set xs -> fprintf ppf "%a" (Seq.pp Word.pp) (Seq.of_list xs)
  let rec pp ppf = function
    | Imm (sz,v) -> fprintf ppf "%a:%a" pp_value v Size.pp sz
    | Seq ts -> fprintf ppf "%a" (Seq.pp pp) (Seq.of_list ts)
    | Ptr t  -> fprintf ppf "%a ptr" pp t
end

module Type = struct
  include Bap_c_type
  let pp = Bap_c_type_printer.pp
end


module Proto = struct
  type t = Bap_c_type.proto [@@deriving bin_io, compare, sexp]
  let pp = Bap_c_type_printer.pp_proto
end

let data = Value.Tag.register (module Data)
    ~name:"c.data"
    ~uuid:"e857a310-2cf0-487f-a879-ef5d8e38b3c9"

let t = Value.Tag.register (module Type)
    ~name:"c.type"
    ~uuid:"f668d2ac-874c-4369-acb3-138c041c98c7"

let proto = Value.Tag.register (module Proto)
    ~name:"c.proto"
    ~uuid:"23efab19-4293-4bb7-9c34-0afc63986c2e"
