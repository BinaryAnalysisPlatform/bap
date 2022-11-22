open Core_kernel[@@warning "-D"]
open Bap.Std
open Format


module Data = Bap_c_data


module Type = struct
  include Bap_c_type
  let pp = Bap_c_type_printer.pp
end

module Layout = struct
  open Bap_c_data
  type t = layout [@@deriving bin_io, compare, sexp]
  let pp = pp_layout
end

module Proto = struct
  type t = Bap_c_type.proto [@@deriving bin_io, compare, sexp]
  let pp = Bap_c_type_printer.pp_proto
end

let data = Value.Tag.register (module Data)
    ~name:"c.data"
    ~uuid:"e857a310-2cf0-487f-a879-ef5d8e38b3c9"

let layout = Value.Tag.register (module Layout)
    ~name:"c.layout"
    ~uuid:"e26dbba0-c912-45fb-ac4c-b4a1c242a4f3"

let t = Value.Tag.register (module Type)
    ~name:"c.type"
    ~uuid:"f668d2ac-874c-4369-acb3-138c041c98c7"

let proto = Value.Tag.register (module Proto)
    ~name:"c.proto"
    ~uuid:"23efab19-4293-4bb7-9c34-0afc63986c2e"
