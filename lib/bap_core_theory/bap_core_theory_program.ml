open Core_kernel
open Bitvec_order.Comparators
open Bap_knowledge

let package = "core-theory"
module Effect = Bap_core_theory_effect

type cls = Program
type program = cls
let (cls : (cls,unit) Knowledge.cls) =
  Knowledge.Class.declare ~package "program" ()
    ~public:true
    ~desc:"a class of program instances"

let program = cls

module Label = struct
  let word = Knowledge.Domain.optional "word"
      ~equal:Bitvec.equal
      ~inspect:Bitvec_sexp.sexp_of_t

  let name = Knowledge.Domain.optional "name"
      ~equal:String.equal
      ~inspect:sexp_of_string

  let path = Knowledge.Domain.optional "path"
      ~equal:String.equal
      ~inspect:sexp_of_string

  let names = Knowledge.Domain.powerset (module String) "names"
      ~inspect:sexp_of_string

  let name_choices = Knowledge.Domain.opinions ~empty:None
      ~equal:(Option.equal String.equal)
      ~inspect:(sexp_of_option sexp_of_string)
      "name-choices"


  let int = Knowledge.Domain.optional "ivec"
      ~equal:Int.equal
      ~inspect:sexp_of_int

  let attr name desc =
    let bool_t = Knowledge.Domain.optional
        ~inspect:sexp_of_bool ~equal:Bool.equal "bool" in
    Knowledge.Class.property ~package cls name bool_t
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = bool option [@@deriving bin_io]
                   end))
      ~public:true
      ~desc


  let is_valid = attr "is-valid"
      "is the program valid or not"
  let is_subroutine = attr "is-subroutine"
      "is the program a subroutine entry point"


  let addr = Knowledge.Class.property ~package cls "label-addr" word
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = Bitvec_binprot.t option
                     [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the program virtual address"

  let path = Knowledge.Class.property ~package cls "label-path" path
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = string option [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"a filesytem name of the file that contains the program"

  let name =
    Knowledge.Class.property ~package cls "label-name" name
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = string option [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the program linkage name"


  let possible_name =
    Knowledge.Class.property ~package
      cls "possible-name" name_choices
      ~public:true
      ~desc:"a unique name associated with the program"


  let ivec =
    Knowledge.Class.property ~package cls "label-ivec" int
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = int option [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the program interrupt vector"

  let aliases =
    Knowledge.Class.property ~package cls "label-aliases" names
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = String.Set.t [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the set of known program names"


  open Knowledge.Syntax

  let for_name ?package s =
    Knowledge.Symbol.intern ?package s cls >>= fun obj ->
    Knowledge.provide name obj (Some s) >>| fun () -> obj

  let for_addr ?package x =
    let s = Bitvec.to_string x in
    Knowledge.Symbol.intern ?package s cls >>= fun obj ->
    Knowledge.provide addr obj (Some x) >>| fun () -> obj

  let for_ivec ?package x =
    let s = sprintf "ivec-%x" x in
    Knowledge.Symbol.intern ?package s cls >>= fun obj ->
    Knowledge.provide ivec obj (Some x) >>| fun () -> obj

  let _decide_name_from_possible_name : unit =
    Knowledge.Rule.(declare ~package "name-of-possible-names" |>
                    require possible_name |>
                    provide name |>
                    comment "resolves possible name");
    Knowledge.promise name @@
    Knowledge.resolve possible_name

  let _is_subroutine_implies_is_valid : unit =
    Knowledge.Rule.(declare ~package "subroutine-is-valid" |>
                    require is_subroutine |>
                    provide is_valid |>
                    comment "is-subroutine -> is-valid");
    Knowledge.promise is_valid @@ fun obj ->
    Knowledge.collect is_subroutine obj

  include (val Knowledge.Object.derive cls)
end

module Semantics = struct
  type cls = Effect.cls
  let cls = Knowledge.Class.refine Effect.cls Effect.Sort.top
  module Self = (val Knowledge.Value.derive cls)
  let slot = Knowledge.Class.property ~package program "semantics" Self.domain
      ~persistent:(Knowledge.Persistent.of_binable (module Self))
      ~public:true
      ~desc:"the program semantics"
  include Self
end

include (val Knowledge.Value.derive cls)
