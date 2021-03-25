let package = "core"

open Core_kernel
open Bap_knowledge

module KB = Knowledge
module Var = Bap_core_theory_var
module Val = Bap_core_theory_value
module Mem = Val.Mem
module Bitv = Val.Bitv
module Sort = Val.Sort
module Name = KB.Name

module Endianness = struct
  include KB.Enum.Make()
  let le = declare ~package "le"
  let eb = declare ~package "eb"
  let bi = declare ~package "bi"
end

module System = KB.Enum.Make()
module Abi = KB.Enum.Make()
module Fabi = KB.Enum.Make()
module Filetype = KB.Enum.Make()
module Role = struct
  include KB.Enum.Make()
  module Register = struct
    let general = declare ~package "general"
    let special = declare ~package "special"
    let pseudo = declare ~package "pseudo"
    let integer = declare ~package "integer"
    let floating = declare ~package "floating"
    let vector = declare ~package "vector"
    let stack_pointer = declare ~package "stack-pointer"
    let frame_pointer = declare ~package "frame-pointer"
    let link = declare ~package "link"
    let thread = declare ~package "thread"
    let privileged = declare ~package "privileged"
    let constant = declare ~package "constant"
    let zero = declare ~package "zero"
    let status = declare ~package "status"
    let hardware = declare ~package "hardware"
    let reserved = declare ~package "reserved"
    let zero_flag = declare ~package "zero-flag"
    let carry_flag = declare ~package "carry-flag"
    let sign_flag = declare ~package "sign-flag"
    let overflow_flag = declare ~package "overflow-flag"
    let parity_flag = declare ~package "parity-flag"
    let function_argument = declare ~package "function-argument"
    let function_return = declare ~package "function-return"
    let caller_saved = declare ~package "caller-saved"
    let callee_saved = declare ~package "callee-saved"
  end
end

module Options = struct
  type cls = Options
  let cls : (cls,unit) KB.cls = KB.Class.declare ~package "target-options" ()
  let pp ppf x = KB.Value.pp ppf x
  let to_string x = Format.asprintf "%a" pp x
  include (val KB.Value.derive cls)
end

module Self = KB.Enum.Make()

type t = Self.t [@@deriving bin_io, compare, sexp]
type target = t
type endianness = Endianness.t
type role = Role.t
type system = System.t
type abi = Abi.t
type filetype = Filetype.t
type fabi = Fabi.t
type name = Name.t

type options = Options.t and options_cls = Options.cls

type mem = Var : ('a,'b) Mem.t Var.t -> mem

type info = {
  parent : target;
  bits : int;
  byte : int;
  data : mem;
  code : mem;
  vars : Set.M(Var.Top).t;
  regs : Set.M(Var.Top).t Map.M(Role).t;
  endianness : endianness;
  system : system;
  abi : abi;
  fabi : fabi;
  filetype : filetype;
  options : options;
  names : String.Caseless.Set.t
}

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
  regs = Map.empty (module Role);
  endianness = Endianness.eb;
  system = System.unknown;
  abi = Abi.unknown;
  fabi = Fabi.unknown;
  filetype = Filetype.unknown;
  options = Options.empty;
  names = String.Caseless.Set.empty;
}

let targets = Hashtbl.of_alist_exn (module Self) [
    unknown.parent, unknown
  ]

let make_roles = List.fold
    ~f:(fun spec (roles,vars) ->
        let vars = Set.of_list (module Var.Top) vars in
        List.fold roles ~init:spec ~f:(fun spec role ->
            Map.update spec role ~f:(function
                | None -> vars
                | Some vars' -> Set.union vars vars')))
    ~init:(Map.empty (module Role))

let collect_regs ?pred init roles =
  Map.fold roles ~init ~f:(fun ~key:_ ~data:vars' vars ->
      match pred with
      | None -> Set.union vars vars'
      | Some pred -> Set.union vars (Set.filter vars' pred))

let extend parent
    ?(bits=parent.bits)
    ?(byte=parent.byte)
    ?(data=unpack@@parent.data)
    ?(code=unpack@@parent.code)
    ?vars
    ?regs
    ?(endianness=parent.endianness)
    ?(system=parent.system)
    ?(abi=parent.abi)
    ?(fabi=parent.fabi)
    ?(filetype=parent.filetype)
    ?(options=parent.options)
    ?nicknames name =
  let code = pack code
  and data = pack data
  and vars = Option.value_map vars
      ~default:parent.vars
      ~f:(Set.of_list (module Var.Top));
  and regs = Option.value_map regs
      ~default:parent.regs
      ~f:make_roles in
  let (+) s (Var v) = Set.add s (Var.forget v) in
  {
    parent=name; bits; byte; endianness;
    system; abi; fabi; filetype; data; code; regs;
    vars = collect_regs (vars + code + data) regs;
    options;
    names = Option.value_map nicknames
        ~default:parent.names
        ~f:String.Caseless.Set.of_list;
  }

let declare
    ?(parent=unknown.parent)
    ?bits ?byte ?data ?code ?vars ?regs ?endianness
    ?system ?abi ?fabi ?filetype ?options
    ?nicknames ?package name =
  let t = Self.declare ?package name in
  if Hashtbl.mem targets t
  then failwithf "A target with name %s already exists \
                  in the package %s, please choose another \
                  name or package"
      (Name.unqualified (Self.name t))
      (Name.package (Self.name t)) ();
  let p = Hashtbl.find_exn targets parent in
  let info = extend ?bits ?byte ?data ?code ?vars ?regs ?endianness
      ?system ?abi ?fabi ?filetype ?options ?nicknames p parent in
  Hashtbl.add_exn targets t info;
  t

let lookup ?package name =
  try Some (Self.read ?package name)
  with _exn -> None

let get ?package name =
  match lookup ?package name with
  | None ->
    invalid_argf
      "Unknown target %s. \
       Use `bap list targets' for the list of targets"
      name ();
  | Some t -> t

let read = get

let info name = match Hashtbl.find targets name with
  | None -> unknown
  | Some t -> t

let parent t = (info t).parent
let name t = Self.name t
let bits t = (info t).bits
let byte t = (info t).byte
let data t = unpack@@(info t).data
let code t = unpack@@(info t).code

let has_role roles var role = match Map.find roles role with
  | None -> false
  | Some vars -> Set.mem vars var

let is_excluded exclude info = match exclude with
  | None -> fun _ -> false
  | Some excluded ->
    fun var -> List.exists excluded ~f:(has_role info.regs var)

let is_included roles info = match roles with
  | None -> fun _ -> true
  | Some included ->
    fun var -> List.for_all included ~f:(has_role info.regs var)

let regs ?exclude ?roles t =
  let info = info t in
  let pred = match exclude,roles with
    | None,None -> None
    | _ -> Some (fun v ->
        is_included roles info v &&
        not (is_excluded exclude info v)) in
  collect_regs ?pred (Set.empty (module Var.Top)) info.regs

(* length > 1 *)
let non_unique s = Option.is_some (Set.nth s 1)

let reg ?exclude ?(unique=false) t role =
  let info = info t in
  match Map.find info.regs role with
  | None -> None
  | Some vars ->
    let vars = Set.filter vars ~f:(fun v ->
        not (is_excluded exclude info v)) in
    match Set.choose vars with
    | Some _ when unique && non_unique vars -> None
    | x -> x

let vars t = (info t).vars

let var t name =
  let key = Var.define Sort.Top.t name in
  Set.binary_search (vars t) ~compare:Var.Top.compare `First_equal_to key

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
    if Self.equal unknown.parent p
    then List.rev (p::ps)
    else closure (p::ps) (parent p) in
  closure [] (parent target)

let is_unknown c = Self.equal c unknown.parent
let is_known c = not@@is_unknown c

let rec belongs p c =
  Self.equal p c || is_known c && belongs p (parent c)

let rec matches_name t name =
  String.Caseless.equal (Name.unqualified (Self.name t)) name ||
  is_known t && matches_name (parent t) name

let rec matches t name =
  let nicks = (info t).names in
  Set.mem nicks name || matches_name t name

let order t1 t2 : KB.Order.partial =
  if Self.equal t1 t2 then EQ
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
        let t1 = Self.name t1 and t2 = Self.name t2 in
        String.compare (Name.unqualified t1) (Name.unqualified t2)
      | _ -> 0)

let family t =
  declared () |>
  List.filter ~f:(belongs t) |>
  sort_family_by_order

let partition xs =
  let families = Map.empty (module Self) in
  let universe = Set.of_list (module Self) xs in
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
