open Core_kernel
open Bitvec_order.Comparators
open Bap_knowledge

let package = "core-theory"
module Effect = Bap_core_theory_effect
module Target = Bap_core_theory_target

type cls = Program
type program = cls
let (cls : (cls,unit) Knowledge.cls) =
  Knowledge.Class.declare ~package "program" ()
    ~public:true
    ~desc:"a class of program instances"

let program = cls

let word = Knowledge.Domain.optional "word"
    ~equal:Bitvec.equal
    ~inspect:Bitvec_sexp.sexp_of_t

let name = Knowledge.Domain.optional "name"
    ~equal:String.equal
    ~inspect:sexp_of_string

let path = Knowledge.Domain.optional "path"
    ~equal:String.equal
    ~inspect:sexp_of_string

let format = Knowledge.Domain.optional "path"
    ~equal:String.equal
    ~inspect:sexp_of_string

let names = Knowledge.Domain.powerset (module String) "names"
    ~inspect:sexp_of_string

let name_choices = Knowledge.Domain.opinions ~empty:None
    ~equal:(Option.equal String.equal)
    ~inspect:(sexp_of_option sexp_of_string)
    "name-choices"

let string_property ?(domain=name) ~desc cls name =
  Knowledge.Class.property ~package cls name domain
    ~persistent:(Knowledge.Persistent.of_binable (module struct
                   type t = string option [@@deriving bin_io]
                 end))
    ~public:true
    ~desc

module Language = Target.Enum.Make()
type language = Language.t

module Source = struct
  type cls = Source
  let cls : (cls,unit) Knowledge.cls =
    Knowledge.Class.declare ~package "unit-source" ()

  let language = Knowledge.Class.property cls
      ~package "source-language" Language.domain
      ~persistent:Language.persistent
      ~public:true
      ~desc:"the language of the unit's source code"

  let code = Knowledge.Class.property cls
      ~package "source-code" Knowledge.Domain.string
      ~persistent:Knowledge.Persistent.string
      ~public:true
      ~desc:"the units source code text"

  let file = string_property ~domain:path cls "source-path"
      ~desc:"a path to the source code file"

  module Value = (val Knowledge.Value.derive cls)

  let persistent = Knowledge.Persistent.of_binable (module Value)

  include Value
end

module Compiler = struct
  type t = {
    name : string;
    version : string list;
    options : string list;
    specs : string String.Map.t;
  } [@@deriving bin_io, compare, equal, fields, sexp]

  let create ?(specs=[]) ?(version=[]) ?(options=[]) name = {
    name; version; options;
    specs = String.Map.of_alist_exn specs;
  }

  let pp ppf x = Sexp.pp_hum ppf (sexp_of_t x)

  let to_string x = Format.asprintf "%a" pp x

  let persistent = Knowledge.Persistent.of_binable (module struct
      type nonrec t = t option [@@deriving bin_io]
    end)

  let domain = Knowledge.Domain.optional
      ~inspect:sexp_of_t
      ~equal
      "compiler"

  include Base.Comparable.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
    end)
end

type compiler = Compiler.t

module Unit = struct
  open Knowledge.Syntax
  type cls = Unit

  let cls : (cls,unit) Knowledge.cls =
    Knowledge.Class.declare ~package "unit" ()
      ~public:true
      ~desc:"a unit of code"

  let path = string_property ~domain:path cls "unit-path"
      ~desc:"a filesystem name of the file that contains the program"

  let bias = Knowledge.Class.property ~package cls "unit-bias" word
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = Bitvec_binprot.t option
                     [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the value by which all addresses of the unit a biased"

  let for_file name =
    Knowledge.Symbol.intern ~package:"file" name cls
      ~public:true >>= fun obj ->
    Knowledge.provide path obj (Some name) >>| fun () ->
    obj


  let for_region ~lower ~upper =
    let to_symbol addr =
      Knowledge.Symbol.intern (Bitvec.to_string addr) program >>=
      Knowledge.Object.repr program in
    to_symbol lower >>= fun lower ->
    to_symbol upper >>= fun upper ->
    let name = Format.asprintf "%s-%s" lower upper in
    Knowledge.Symbol.intern ~package:"region" name cls


  let target =
    Knowledge.Class.property ~package cls "unit-target" Target.domain
      ~persistent:Target.persistent
      ~public:true
      ~desc:"the target system for the unit"


  let source =
    Knowledge.Class.property ~package cls "unit-source" Source.domain
      ~persistent:Source.persistent
      ~public:true
      ~desc:"the source of the unit"

  let compiler = Knowledge.Class.property ~package cls "unit-compiler"
      Compiler.domain
      ~persistent:Compiler.persistent
      ~public:true
      ~desc:"the compiler that compiles/compiled the unit"


  include (val Knowledge.Object.derive cls)

end



module Label = struct

  let int = Knowledge.Domain.optional "ivec"
      ~equal:Int.equal
      ~inspect:sexp_of_int

  let unit = Knowledge.Domain.optional "unit"
      ~equal:Unit.equal
      ~inspect:Unit.sexp_of_t

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

  let encoding = Knowledge.Class.property ~package cls "encoding"
      Language.domain
      ~persistent:Language.persistent
      ~public:true
      ~desc:"the language of the code"

  let unit = Knowledge.Class.property ~package cls "label-unit" unit
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = Unit.t option
                     [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the program unit"

  let addr = Knowledge.Class.property ~package cls "label-addr" word
      ~persistent:(Knowledge.Persistent.of_binable (module struct
                     type t = Bitvec_binprot.t option
                     [@@deriving bin_io]
                   end))
      ~public:true
      ~desc:"the program virtual address"

  let name = string_property cls "label-name"
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

  let target x =
    Knowledge.collect unit x >>= function
    | None -> Knowledge.return Target.unknown
    | Some unit -> Knowledge.collect Unit.target unit

  let _decide_name_from_possible_name : unit =
    Knowledge.Rule.(declare ~package "name-of-possible-names" |>
                    require possible_name |>
                    provide name |>
                    comment "resolves possible name");
    Knowledge.promise name @@
    Knowledge.resolve possible_name

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
