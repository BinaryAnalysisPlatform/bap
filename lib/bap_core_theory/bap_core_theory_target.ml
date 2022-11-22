let package = "core"

open Core_kernel[@@warning "-D"]
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

module System = struct
  include KB.Enum.Make()
  let linux = declare ~package "linux"
  let darwin = declare ~package "darwin"
  let vxworks = declare ~package "vxworks"
  let freebsd = declare ~package "freebsd"
  let openbsd = declare ~package "openbsd"
  let windows = declare ~package "windows"
  let msdos = declare ~package "msdos"
  let uefi = declare ~package "uefi"
  let none = declare ~package "none"
end

module Abi = struct
  include KB.Enum.Make()
  let gnu = declare ~package "gnu"
  let eabi = declare ~package "eabi"
  let gnueabi = declare ~package "gnueabi"
  let cdecl = declare ~package "cdecl"
  let stdcall = declare ~package "stdcall"
  let fastcall = declare ~package "fastcall"
  let watcom = declare ~package "watcom"
  let ms = declare ~package "ms"
end

module Fabi = struct
  include KB.Enum.Make()
  let hard = declare ~package "hf"
  let soft = declare ~package "sf"
end

module Filetype = struct
  include KB.Enum.Make()
  let elf = declare ~package "elf"
  let coff = declare ~package "coff"
  let macho = declare ~package "macho"
  let aout = declare ~package "aout"
end

module Role = struct
  include KB.Enum.Make()
  module Register = struct
    let general = declare ~package "general"
    let special = declare ~package "special"
    let alias = declare ~package "alias"
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
type target = t [@@deriving sexp_of]
type endianness = Endianness.t [@@deriving sexp_of]
type role = Role.t
type system = System.t [@@deriving sexp_of]
type abi = Abi.t [@@deriving sexp_of]
type filetype = Filetype.t [@@deriving sexp_of]
type fabi = Fabi.t [@@deriving sexp_of]
type name = Name.t [@@deriving sexp_of]

type options = Options.t [@@deriving sexp_of]
type options_cls = Options.cls

type mem = Var : ('a,'b) Mem.t Var.t -> mem

let sexp_of_mem (Var v) =
  Sexp.Atom (Format.sprintf "%s" (Var.name v))

type alignment = {
  code : int;
  data : int;
} [@@deriving sexp_of]

type aliases = {
  subs : (int * Var.Top.t) Map.M(Var.Top).t;
  sups : Var.Top.t Map.M(Int).t Map.M(Var.Top).t;
}

type info = {
  parent : target;
  bits : int;
  byte : int;
  data : mem;
  code : mem;
  vars : (Set.M(Var.Top).t [@sexp.opaque]);
  regs : (Set.M(Var.Top).t Map.M(Role).t [@sexp.opaque]);
  aliasing : (aliases [@sexp.opaque]);
  endianness : endianness;
  alignment : alignment;
  system : system;
  abi : abi;
  fabi : fabi;
  filetype : filetype;
  options : options;
  names : String.Caseless.Set.t
} [@@deriving sexp_of]

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
  alignment = {code=8; data=8};
  data = pack@@mem "mem" 32 8;
  code = pack@@mem "mem" 32 8;
  vars = Set.empty (module Var.Top);
  regs = Map.empty (module Role);
  aliasing = {
    subs = Map.empty (module Var.Top);
    sups = Map.empty (module Var.Top);
  };
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

module Alias = struct
  type t = unit Var.t * (int * unit Var.t) list
  type 'a part = 'a Bitv.t Var.t option

  let regsize v = match Bitv.refine (Var.sort v) with
    | Some s -> Bitv.size s
    | None -> assert false

  let init = {
    sups = Map.empty (module Var.Top);
    subs = Map.empty (module Var.Top);
  }

  let is_solved {subs; sups} v = (Map.mem subs v || Map.mem sups v)
  let solved_variables {subs; sups} =
    Map.length subs + Map.length sups

  let add_sup sol lhs rhs =
    let rhs = Map.of_alist_exn (module Int) rhs in {
      sol with sups = Map.update sol.sups lhs ~f:(function
        | None -> rhs
        | Some rhs' ->
          Map.merge rhs rhs' ~f:(fun ~key:_ -> function
              | `Left x | `Right x -> Some x
              | `Both (x,y) ->
                if not (Var.Top.equal x y)
                then failwith "invalid equation";
                Some x))
    }

  let add_sub sol lhs rhs = {
    sol with subs = Map.set sol.subs lhs rhs;
  }

  let pp_spec ppf spec =
    List.iter spec ~f:(fun (lhs,rhs) ->
        Format.fprintf ppf "%s = " (Var.name lhs);
        List.iter rhs ~f:(fun (off,var) ->
            Format.fprintf ppf "%d:%s " off (Var.name var));
        Format.fprintf ppf "@\n%!")

  let substitute_one sol lhs (n,r) =
    match Map.find sol.subs r with
    | Some (m,x) -> [n+m,x]
    | None -> match Map.find sol.sups r with
      | None -> [n,r]
      | Some parts ->
        let parts = Map.to_alist parts in
        let partsize =
          regsize @@ snd (List.hd_exn parts) in
        if regsize lhs > partsize
        then List.map parts ~f:(fun (m,r) -> (m+n,r))
        else List.concat_map parts ~f:(fun (m,x) ->
            if n >= m && n + regsize lhs <= m + regsize x
            then [n-m,x]
            else [])

  let pp_aliases ppf {subs; sups} =
    Map.iteri subs ~f:(fun ~key:alias ~data:(off,part) ->
        Format.fprintf ppf "%s = extract:%d:%d[%s]@\n"
          (Var.name alias)
          (regsize alias + off) off
          (Var.name part));
    Map.iteri sups ~f:(fun ~key:alias ~data:parts ->
        let parts = Map.data parts |> List.rev_map ~f:Var.name |>
                    String.concat ~sep:"." in
        Format.fprintf ppf "%s = %s@\n"
          (Var.name alias) parts)

  let pp_lhs () spec =
    List.map ~f:fst spec |>
    List.map ~f:Var.name |>
    String.concat ~sep:" "

  let report_unsolved spec =
    invalid_argf "Failed to solve register aliasing. \
                  Unable to resolve the following registers: (%a). \
                  Please, provide extra constraints or mark them as \
                  non-aliases."
      pp_lhs spec ()


  type equation = Var.Top.t * (int * Var.Top.t) list
  [@@deriving equal]
  type system = equation list
  [@@deriving equal]

  let no_progress = equal_system
  let has_progress x y = not (no_progress x y)

  let string_of_vars vars =
    Set.to_list vars |>
    List.map ~f:Var.name |>
    String.concat ~sep:" "


  let solve regs spec =
    let aliases = match Map.find regs Role.Register.alias with
      | None -> Set.empty (module Var.Top)
      | Some vars -> vars in
    let is_alias = Set.mem aliases in
    let is_base = Fn.non is_alias in
    let all_solved = List.for_all ~f:(fun (_,r) -> is_base r) in
    let substitute spec sol =
      List.map spec ~f:(function
          | lhs,[rhs] -> lhs,substitute_one sol lhs rhs
          | other -> other) in
    let invert sol spec =
      List.concat_map spec ~f:(fun (lhs,rhs) ->
          match rhs with
          | [off,rhs] when is_base lhs || is_solved sol lhs -> [rhs,[off,lhs]]
          | [_] -> [lhs,rhs]
          | rhs -> List.map rhs ~f:(fun (off,sub) -> sub,[off,lhs])) in
    let reduce spec sol =
      List.fold spec ~init:([],sol) ~f:(fun (spec,sol) (lhs,rhs) ->
          if is_alias lhs && all_solved rhs then
            spec, match rhs with
            | [res] -> add_sub sol lhs res
            | sum -> add_sup sol lhs sum
          else (lhs,rhs)::spec,sol) in
    let rec loop input sol steps =
      let solved = solved_variables sol in
      let spec,sol = reduce input sol in
      let spec = invert sol spec in
      let spec = substitute spec sol in
      if solved_variables sol = solved then match spec with
        | [] -> sol
        | spec when steps > 0 -> loop spec sol (steps-1)
        | spec -> report_unsolved spec
      else loop spec sol (steps-1) in
    loop spec init 10000

  let error def =
    Format.kasprintf @@ fun details ->
    invalid_argf "%s: bad aliasing defintion of %s - %s"
      "Theory.Alias.def" (Var.name def) details ()

  let check var = function
    | [] -> error var "the parts list is empty"
    | parts ->
      let total = Bitv.size (Var.sort var) in
      let width = total / List.length parts in
      List.iter parts ~f:(Option.iter ~f:(fun v ->
          if Bitv.size (Var.sort v) <> width
          then error var "the size of %s must be %d divided by %d"
              (Var.name v) total (List.length parts)))

  let def var parts : t =
    check var parts;
    let var = Var.forget var in
    let total = regsize var in
    let width = total / List.length parts in
    var,
    List.filter_mapi parts ~f:(fun i -> function
        | None -> None
        | Some reg ->
          Some (total - i * width - width, Var.forget reg))

  let reg x = Some x
  let unk = None
end

module Origin = struct
  type sup = Sup
  type sub = Sub
  type syn = Syn

  type ('a,'k) t =
    | Any : ('a,'k) t -> ('a,unit) t
    | Sup : 'a Bitv.t Var.t list -> ('a,sup) t
    | Sub : {hi : int; lo : int; base : 'a Bitv.t Var.t} -> ('a,sub) t

  let forget : type k. ('a,k) t -> ('a,unit) t = function
    | Any _ as x -> x
    | x -> Any x

  let cast_sub = function
    | Any (Sub _ as x) -> Some x
    | Any _ -> None

  let cast_sup = function
    | Any (Sup _ as x) -> Some x
    | Any _ -> None

  let reg (Sub {base=v}) = v
  let hi (Sub {hi}) = hi
  let lo (Sub {lo}) = lo
  let is_alias (Sub {hi;lo;base}) =
    hi - lo + 1 = Bitv.size (Var.sort base)

  let regs (Sup regs) = regs
end

type ('a,'k) origin = ('a,'k) Origin.t

let extend parent
    ?(bits=parent.bits)
    ?(byte=parent.byte)
    ?(data=unpack@@parent.data)
    ?(code=unpack@@parent.code)
    ?(data_alignment=parent.alignment.data)
    ?(code_alignment=parent.alignment.code)
    ?vars
    ?regs
    ?aliasing
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
  let aliasing = Option.value_map aliasing
      ~default:parent.aliasing
      ~f:(Alias.solve regs) in
  let (+) s (Var v) = Set.add s (Var.forget v) in
  {
    parent=name; bits; byte; endianness;
    system; abi; fabi; filetype; data; code; regs;
    vars = collect_regs (vars + code + data) regs;
    aliasing;
    options;
    alignment = {
      code=code_alignment;
      data=data_alignment;
    };
    names = Option.value_map nicknames
        ~default:parent.names
        ~f:String.Caseless.Set.of_list;
  }

let declare
    ?(parent=unknown.parent)
    ?bits ?byte ?data ?code
    ?data_alignment ?code_alignment
    ?vars ?regs ?aliasing ?endianness
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
  let info = extend ?bits ?byte ?data ?code
      ?data_alignment ?code_alignment ?vars ?regs ?aliasing ?endianness
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

let has_roles t roles var =
  let {regs} = info t and var = Var.forget var in
  List.for_all roles ~f:(has_role regs var)

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

let unalias t reg : ('a,unit) origin option =
  let {sups; subs} = (info t).aliasing in
  let reg = Var.forget reg in
  let refine v =
    match Bitv.refine (Var.sort v) with
    | None ->
      failwithf "broken invariant: non-register in a file: %s"
        (Var.name v) ()
    | Some s -> Var.resort v s in
  match Map.find sups reg with
  | Some parts ->
    let parts =
      Map.to_alist parts ~key_order:`Decreasing |>
      List.map ~f:(fun (_,v) -> refine v) in
    Some Origin.(forget (Sup parts))
  | None -> match Map.find subs reg with
    | None -> None
    | Some (lo,v) ->
      let hi = Alias.regsize reg - 1 in
      let origin = Origin.Sub {hi; lo; base=refine v} in
      Some Origin.(forget origin)

let data_addr_size,
    code_addr_size =
  let keys v = Bitv.size @@ Mem.keys @@ Var.sort v in
  (fun t -> keys @@ data t),
  (fun t -> keys @@ code t)

let data_alignment t = (info t).alignment.data
let code_alignment t = (info t).alignment.code

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

let matches_name t name =
  String.Caseless.equal (Name.unqualified (Self.name t)) name

let rec matching t name =
  if matches_name t name || Set.mem (info t).names name
  then Some t
  else if is_known t then matching (parent t) name
  else None


let matches t name =
  Option.is_some (matching t name)

let nicknames t = (info t).names

let is_greater_or_equal : KB.Order.partial -> bool = function
  | EQ | GT -> true
  | LT | NC -> false


let pp_option pp ppf = function
  | None -> Format.pp_print_string ppf ":unknown"
  | Some x -> pp ppf x


let print_constraints system abi fabi filetype options parent ppf =
  Format.fprintf ppf "@[<v2>Constraints:@;\
                      Family:   %a@;\
                      System:   %a@;\
                      Abi:      %a@;\
                      Fabi:     %a@;\
                      Filetype: %a@;\
                      Options:  %a@;@]"
    (pp_option Self.pp) parent
    (pp_option System.pp) system
    (pp_option Abi.pp) abi
    (pp_option Fabi.pp) fabi
    (pp_option Filetype.pp) filetype
    (pp_option Options.pp) options

let report_no_matches system abi fabi filetype options parent =
  invalid_arg @@ Format.asprintf "Target.select - no matching target@\n%t"
    (print_constraints system abi fabi filetype options parent)


let report_non_unique system abi fabi filetype options parent targets =
  let targets = String.concat ~sep:", " @@
    List.map targets ~f:Self.to_string in
  invalid_arg @@ Format.asprintf
    "Target.select - more than one target matches the \
     constraints: {%s}@\n%t" targets
    (print_constraints system abi fabi filetype options parent)

module type Orderable = sig
  type t
  val domain : t KB.Domain.t
  val equal : t -> t -> bool
end

let filter
    ?(strict=false)
    ?(parent=Self.unknown)
    ?(system=System.unknown)
    ?(abi=Abi.unknown)
    ?(fabi=Fabi.unknown)
    ?(filetype=Filetype.unknown)
    ?(options=Options.empty) () =
  let order (type t) (module E : Orderable with type t = t)  x y =
    if strict then E.equal x y
    else is_greater_or_equal @@
      KB.Domain.order E.domain x y in
  let result =
    family parent |> List.filter ~f:(fun t ->
        let i = info t in
        List.for_all ~f:Fn.id [
          order (module System) i.system system;
          order (module Abi) i.abi abi;
          order (module Fabi) i.fabi fabi;
          order (module Filetype) i.filetype filetype;
          order (module Options) i.options options;
        ]) in
  result

let select
    ?(unique=false)
    ?strict
    ?parent
    ?system
    ?abi
    ?fabi
    ?filetype
    ?options () =
  filter ?strict ?parent ?system ?abi ?fabi ?filetype ?options () |> function
  | [] -> report_no_matches system abi fabi filetype options parent
  | [t] -> t
  | ts when unique -> report_non_unique system abi fabi filetype options parent ts
  | t :: _ -> t


let list_options options =
  match KB.Domain.inspect Options.domain options with
  | List options -> options
  | _ -> []

let option_to_string = function
  | Sexp.List [] -> ":unknown"
  | Sexp.List [Atom slot; Atom value] -> "+"^slot^"="^value
  | Sexp.List [Atom slot; List []] -> "+"^slot
  | other -> string_of_sexp other

let options_to_string options =
  String.concat (List.map (list_options options) ~f:option_to_string)

type component = F of string | S of string

let is_unknown_field = function
  | F s -> String.equal ":unknown" s
  | S _ -> false

let are_separators x y = match x,y with
  | S _, S _ -> true
  | _ -> false

let string_of_component = function S s | F s -> s

let generate_name components =
  List.filter components ~f:(Fn.non is_unknown_field) |>
  List.remove_consecutive_duplicates ~equal:are_separators |>
  List.map ~f:string_of_component |>
  String.concat

let field name x =
  F (KB.Name.unqualified (name x))

let register
    ?(systems=[System.unknown])
    ?(abis=[Abi.unknown])
    ?(fabis=[Fabi.unknown])
    ?(filetypes=[Filetype.unknown])
    ?(options=[Options.empty])
    ?package
    parent =
  ignore @@
  let (let*) = List.Monad_infix.(>>=) in
  let* system = systems in
  let* filetype = filetypes in
  let* abi = abis in
  let* fabi = fabis in
  let* options = options in
  if List.for_all ~f:Fn.id [
      System.is_unknown system;
      Abi.is_unknown abi;
      Fabi.is_unknown fabi;
      Filetype.is_unknown filetype;
      KB.Value.is_empty options;
    ]
  then []
  else
    let package = Option.value package
        ~default:(KB.Name.package (name parent)) in
    let base = (KB.Name.unqualified (name parent)) in
    let name = generate_name [
        F base; S "-";
        field System.name system; S "-";
        field Abi.name abi; S "";
        field Fabi.name fabi; S "-";
        field Filetype.name filetype; S "";
        F (options_to_string options);
      ] in [
      declare ~parent ~package name
        ~system
        ~filetype
        ~abi
        ~fabi
        ~options
    ]

let hash = Self.hash

type alias = Alias.t

include (Self : Base.Comparable.S with type t := t)
include (Self : Stringable.S with type t := t)
include (Self : Pretty_printer.S with type t := t)
let domain = Self.domain
let persistent = Self.persistent
let unknown = Self.unknown
