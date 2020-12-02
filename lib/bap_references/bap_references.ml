open Bap_knowledge
open Core_kernel
open Bap.Std
open Bap_core_theory


open Image.Scheme
open Ogre.Syntax

module Bitvec = struct
  include Bitvec
  include Bitvec_sexp.Functions
end


module Refs = Map.Make(Bitvec)

type ref =
  | Addr of Bitvec.t
  | Name of string
[@@deriving compare, sexp, equal]

type value = Ref of ref | Bad [@@deriving compare, sexp, equal]
type t = value Refs.t [@@deriving sexp_of, equal]


let empty = Refs.empty

let slot = KB.Class.property Theory.Unit.cls "refs"
    ~package:"bap"
    ~public:true
    ~desc:"external references" @@
  KB.Domain.flat ~empty "refs"
    ~inspect:sexp_of_t
    ~equal

let chop_version s =
  match String.lfindi s ~f:(fun _ -> Char.equal '@') with
  | None | Some 0 -> s
  | Some len -> String.subo ~len s


let arch =
  let open Ogre.Syntax in
  Ogre.request Image.Scheme.arch >>| function
  | None -> `unknown
  | Some arch -> match Arch.of_string arch with
    | None -> `unknown
    | Some arch -> arch

let width = Ogre.(arch >>| Arch.addr_size >>| Size.in_bits)

let collect init merge map src =
  width >>| Bitvec.modulus >>= fun m ->
  Ogre.collect Ogre.Query.(select (from src)) >>|
  Seq.fold ~init ~f:(fun exts (addr, value) ->
      Map.update exts Bitvec.(int64 addr mod m) ~f:(function
          | None -> map m value
          | Some value' -> merge m value' value))

let name _ x = Ref (Name (chop_version x))
and addr m x = Ref (Addr Bitvec.(int64 x mod m))

let merge_name m x y =
  let y = name m y in
  match x with
  | Bad -> Bad
  | Ref (Addr _) as y -> y
  | Ref (Name _) as x ->
    if compare_value x y = 0 then y else Bad

let merge_addr m x y =
  let y = addr m y in
  match x with
  | Bad -> Bad
  | Ref (Addr _) as x when compare_value x y <> 0 -> Bad
  | _ -> y

let extract =
  collect empty merge_name name external_reference >>= fun names ->
  collect names merge_addr addr relocation

let create doc = match Ogre.eval extract doc with
  | Ok exts -> exts
  | Error _ -> empty

let lookup exts addr = match Map.find exts addr with
  | Some Bad -> None
  | Some Ref x -> Some x
  | None -> None

let provide_from_spec () =
  let open KB.Rule in
  declare ~package:"bap" "refs-of-spec" |>
  require Image.Spec.slot |>
  provide slot |>
  comment "extracts external references from the specification";
  let open KB.Syntax in
  KB.promise slot @@ fun unit ->
  KB.collect Image.Spec.slot unit >>| create
