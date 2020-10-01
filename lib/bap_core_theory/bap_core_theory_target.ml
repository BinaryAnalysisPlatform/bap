let package = "core-theory"

open Core_kernel
open Bap_knowledge

module KB = Knowledge
module Var = Bap_core_theory_var
module Val = Bap_core_theory_value
module Mem = Val.Mem
module Bitv = Val.Bitv
module Sort = Val.Sort
module Name = KB.Name

type t = Name.t [@@deriving bin_io, compare, sexp]
type target = t
type endianness = Name.t
type system = Name.t
type abi = Name.t
type filetype = Name.t
type fabi = Name.t
type name = Name.t

module Enum = struct
  module type S = sig
    include Base.Comparable.S
    include Binable.S with type t := t
    include Stringable.S with type t := t
    include Pretty_printer.S with type t := t
    include Sexpable.S with type t := t
    val declare : ?package:string -> string -> t
    val read : ?package:string -> string -> t
    val name : t -> KB.Name.t
    val unknown : t
    val is_unknown : t -> bool
    val domain : t KB.domain
    val persistent : t KB.persistent
    val hash : t -> int
  end

  module Make() = struct
    type t = Name.t [@@deriving bin_io, sexp]

    let elements = Hash_set.create (module Name) ()
    let declare ?package name =
      let name = Name.create ?package name in
      if Hash_set.mem elements name
      then invalid_argf
          "Enum.declare: the element %s is already declared \
           please choose a unique name" (Name.to_string name) ();
      Hash_set.add elements name;
      name

    let read ?package name =
      let name = Name.read ?package name in
      if not (Hash_set.mem elements name)
      then invalid_argf "Enum.read: %s is not a member of the given \
                         enumeration." (Name.to_string name) ();
      name

    let name x = x
    let unknown = Name.of_string ":unknown"
    let is_unknown = Name.equal unknown
    let hash = Name.hash
    include Base.Comparable.Make(Name)
    include (Name : Stringable.S with type t := t)
    include (Name : Pretty_printer.S with type t := t)
    let domain = Knowledge.Domain.flat "enum"
        ~inspect:sexp_of_t
        ~empty:unknown
        ~equal
    let persistent = KB.Persistent.name
  end
end

module Endianness = struct
  include Enum.Make()
  let le = declare ~package "le"
  let eb = declare ~package "eb"
  let bi = declare ~package "bi"
end

module System = Enum.Make()
module Abi = Enum.Make()
module Fabi = Enum.Make()
module Filetype = Enum.Make()

module Options = struct
  type cls = Options
  let cls : (cls,unit) KB.cls = KB.Class.declare ~package "target-options" ()
  let pp ppf x = KB.Value.pp ppf x
  let to_string x = Format.asprintf "%a" pp x
  include (val KB.Value.derive cls)
end

type options = Options.t and options_cls = Options.cls

type mem = Var : ('a,'b) Mem.t Var.t -> mem

type info = {
  parent : target;
  bits : int;
  byte : int;
  data : mem;
  code : mem;
  vars : Set.M(Var.Top).t;
  endianness : Name.t;
  system : Name.t;
  abi : Name.t;
  fabi : Name.t;
  filetype : Name.t;
  options : Options.t;
  names : String.Caseless.Set.t
}

module Self = Enum.Make()
let unknown = Self.unknown

let mem name k v =
  let k = Bitv.(define k)
  and v = Bitv.(define v) in
  Var.define (Mem.define k v) name

let pack v = Var v

let unpack (Var var) =
  let s = Var.sort var in
  let k = Bitv.size@@Mem.keys s
  and v = Bitv.size@@Mem.vals s in
  mem (Var.name var) k v

let unknown = {
  parent = unknown;
  bits = 32;
  byte = 8;
  data = pack@@mem "mem" 32 8;
  code = pack@@mem "mem" 32 8;
  vars = Set.empty (module Var.Top);
  endianness = Endianness.eb;
  system = unknown;
  abi = unknown;
  fabi = unknown;
  filetype = unknown;
  options = Options.empty;
  names = String.Caseless.Set.empty;
}

let targets = Hashtbl.of_alist_exn (module Name) [
    unknown.parent, unknown
  ]

let extend parent
    ?(bits=parent.bits)
    ?(byte=parent.byte)
    ?(data=unpack@@parent.data)
    ?(code=unpack@@parent.code)
    ?vars
    ?(endianness=parent.endianness)
    ?(system=parent.system)
    ?(abi=parent.abi)
    ?(fabi=parent.fabi)
    ?(filetype=parent.filetype)
    ?(options=parent.options)
    ?nicknames name = {
  parent=name; bits; byte; endianness;
  system; abi; fabi; filetype;
  options;
  data = pack data;
  code = pack code;
  vars = Option.value_map vars
      ~default:parent.vars
      ~f:(Set.of_list (module Var.Top));
  names = Option.value_map nicknames
      ~default:parent.names
      ~f:String.Caseless.Set.of_list;
}

let declare
    ?(parent=unknown.parent)
    ?bits ?byte ?data ?code ?vars ?endianness
    ?system ?abi ?fabi ?filetype ?options
    ?nicknames ?package name =
  let name = Name.create ?package name in
  if Hashtbl.mem targets name
  then failwithf "A target with name %s already exists \
                  in the package %s, please choose another \
                  name or package"
      (Name.unqualified name)
      (Name.package name) ();
  let p = Hashtbl.find_exn targets parent in
  let info = extend ?bits ?byte ?data ?code ?vars ?endianness
      ?system ?abi ?fabi ?filetype ?options ?nicknames p parent in
  Hashtbl.add_exn targets name info;
  name

let lookup ?package name =
  let name = Name.read ?package name in
  if Hashtbl.mem targets name then Some name
  else None

let get ?package name =
  let name = Name.read ?package name in
  if not (Hashtbl.mem targets name)
  then invalid_argf "Unknown target %s" (Name.to_string name) ();
  name

let info name = match Hashtbl.find targets name with
  | None -> unknown
  | Some t -> t

let parent t = (info t).parent
let name t = t
let bits t = (info t).bits
let byte t = (info t).byte
let data t = unpack@@(info t).data
let code t = unpack@@(info t).code
let vars t = (info t).vars

let data_addr_size,
    code_addr_size =
  let keys v = Bitv.size @@ Mem.keys @@ Var.sort v in
  (fun t -> keys @@ data t),
  (fun t -> keys @@ code t)

let endianness t = (info t).endianness
let system t = (info t).system
let abi t = (info t).abi
let fabi t = (info t).fabi
let filetype t = (info t).filetype
let options t = (info t).options

let parents target =
  let rec closure ps p =
    if p = unknown.parent
    then List.rev (p::ps)
    else closure (p::ps) (parent p) in
  closure [] (parent target)

let is_unknown c = Name.equal c unknown.parent
let is_known c = not@@is_unknown c

let rec belongs p c =
  Name.equal p c || is_known c && belongs p (parent c)

let rec matches_name t name =
  String.Caseless.equal (Name.unqualified t) name ||
  is_known t && matches_name (parent t) name

let rec matches t name =
  let nicks = (info t).names in
  Set.mem nicks name || matches_name t name

let order t1 t2 : KB.Order.partial =
  if Name.equal t1 t2 then EQ
  else if belongs t1 t2 then LT
  else if belongs t2 t1 then GT
  else NC

let declared () = Hashtbl.keys targets |>
                  List.filter ~f:is_known

let sort_family_by_order =
  List.sort ~compare:(fun t1 t2 -> match order t1 t2 with
      | KB.Order.NC | EQ -> 0
      | LT -> -1
      | GT -> 1)

let sort_by_parent_name =
  List.sort ~compare:(fun f1 f2 -> match f1,f2 with
      | t1::_, t2::_ ->
        String.compare (Name.unqualified t1) (Name.unqualified t2)
      | _ -> 0)

let family t =
  declared () |>
  List.filter ~f:(belongs t) |>
  sort_family_by_order

let partition xs =
  let families = Map.empty (module Name) in
  let universe = Set.of_list (module Name) xs in
  let rec grandest t =
    let p = parent t in
    if is_known p && Set.mem universe p
    then grandest p else t in
  List.fold xs ~init:families ~f:(fun families t ->
      Map.add_multi families (grandest t) t) |>
  Map.data |>
  List.map ~f:sort_family_by_order |>
  sort_by_parent_name

let families () = partition@@declared ()

include (Self : Base.Comparable.S with type t := t)
include (Self : Stringable.S with type t := t)
include (Self : Pretty_printer.S with type t := t)
let domain = Self.domain
let persistent = Self.persistent
let unknown = Self.unknown
