open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_common


type var = Var : 'a Theory.Var.t -> var
type t = var

let reify v = Var v
let sort (Var v) =
  Theory.Sort.forget (Theory.Var.sort v)

let ident (Var v) = Theory.Var.ident v
let name (Var v) = Theory.Var.name v
let with_index (Var v) ver =
  Var (Theory.Var.versioned v ver)
let index (Var v) = Theory.Var.version v
let base v = with_index v 0


let typ v =
  let s = sort v in
  Theory.Bool.refine s |> function
  | Some _ -> Type.Imm 1
  | None -> Theory.Bitv.refine s |> function
    | Some bits -> Type.Imm (Theory.Bitv.size bits)
    | None -> Theory.Mem.refine s |> function
      | None -> Type.Unk
      | Some mems ->
        let ks, vs = Theory.Mem.(keys mems, vals mems) in
        let ks, vs = Theory.Bitv.(size ks, size vs) in
        match Bap_size.addr_of_int_opt ks, Bap_size.of_int_opt vs with
        | Some ks, Some vs -> Type.Mem (ks,vs)
        | _ -> Type.Unk



let is_virtual (Var v) = Theory.Var.is_virtual v
let is_physical v = not (is_virtual v)

let unknown =
  let unknown = Theory.Sort.Name.declare ~package:"bap-std" "Unknown" in
  Theory.Sort.sym unknown |>
  KB.Class.refine Theory.Sort.t

let sort_of_typ t =
  let ret = Theory.Sort.forget in
  match t with
  | Type.Imm 1 -> ret Theory.Bool.t
  | Type.Imm m -> ret @@ Theory.Bitv.define m
  | Type.Mem (ks,vs) ->
    let ks,vs = Bap_size.(in_bits ks, in_bits vs) in
    let ks,vs = Theory.Bitv.(define ks, define vs) in
    ret @@ Theory.Mem.define ks vs
  | Type.Unk -> ret @@ unknown

module Generator = struct
  let empty = Theory.Var.Ident.of_string "nil"
  let ident_t = KB.Domain.flat ~empty
      ~inspect:Theory.Var.Ident.sexp_of_t
      ~equal:Theory.Var.Ident.equal
      "ident"
  type generator = Gen
  let generator =
    KB.Class.declare ~package:"bap.std.internal" "var-generator" Gen

  let ident = KB.Class.property ~package:"bap.std.internal"
      generator "ident" ident_t

  let fresh s =
    KB.Value.get ident @@ Bap_state.run_or_fail generator @@
    KB.Syntax.(begin
        KB.Object.create generator >>= fun g ->
        Theory.Var.fresh s >>= fun v ->
        KB.provide ident g (Theory.Var.ident v) >>| fun () ->
        g
      end)
end

let create ?(is_virtual=false) ?(fresh=false) name typ =
  let sort = sort_of_typ typ in
  if is_virtual || fresh
  then
    let iden = Generator.fresh sort in
    Var (Theory.Var.create sort iden)
  else
    Var (Theory.Var.define sort name)

let same x y = base x = base y


module T = struct
  type t = var

  module Repr = struct
    type t = {name : Theory.Var.ident; sort : Theory.Sort.Top.t}
    [@@deriving bin_io, compare, sexp]

    let of_var v = {
      name = ident v;
      sort = sort v
    }
    let to_var {name;sort} =
      Var (Theory.Var.create sort name)
  end

  include Sexpable.Of_sexpable(Repr)(struct
      type t = var
      let to_sexpable = Repr.of_var
      let of_sexpable = Repr.to_var
    end)

  include Binable.Of_binable(Repr)(struct
      type t = var
      let to_binable = Repr.of_var
      let of_binable = Repr.to_var
    end)

  let compare x y =
    Theory.Var.Ident.compare (ident x) (ident y)

  let hash x = Hashtbl.hash (ident x)

  let version = "2.0.0"

  let module_name = Some "Bap.Std.Var"

  let pp ppf x =
    Format.fprintf ppf "%s" (Theory.Var.Ident.to_string (ident x))
end

include Regular.Make(T)
