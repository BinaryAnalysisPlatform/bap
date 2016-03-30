open Core_kernel.Std
open Bap_types.Std
open Regular.Std
open Bap_ir

module SM = Monad.State
open SM.Monad_infix

module Taint = Tid
module Taints = Taint.Set
module Values = Bil.Result.Id.Map

type t = Tid.t
type set = Tid.Set.t [@@deriving bin_io, compare, sexp]
type map = set Var.Map.t [@@deriving bin_io, compare, sexp]
type 'a values = 'a Values.t

let reg = Value.Tag.register
    ~name:"tainted_reg"
    ~uuid:"1ab9a363-db8f-4ab4-9fb4-5ff54de97c5c"
    (module Tid)

let ptr = Value.Tag.register
    ~name:"tainted_ptr"
    ~uuid:"ef2d20e5-b04d-41da-ab20-5d98ddc2f78e"
    (module Tid)

module Taint_map = struct
  type t = map [@@deriving bin_io, compare, sexp]
  include Regular.Make(struct
      open Format

      type t = map [@@deriving bin_io, compare, sexp]
      let version = "0.1"
      let module_name = None

      let pp_list pp ppf xs =
        let rec pp_rest ppf = function
          | [] -> ()
          | [x] -> pp ppf x
          | x :: xs -> fprintf ppf "%a,@;%a" pp x pp_rest xs in
        pp_rest ppf xs

      let pp_taints = pp_list Taint.pp

      let pp_taint_set ppf t =
        fprintf ppf "@[<1>[%a@]]" pp_taints (Set.to_list t)

      let pp_binding ppf (v,ts) =
        fprintf ppf "%a => %a" Var.pp v pp_taint_set ts

      let pp_vars ppf t =
        pp_list pp_binding ppf (Map.to_alist t)

      let pp ppf t =
        fprintf ppf "@[<1>{%a@]}" pp_vars t
      let hash = Hashtbl.hash
    end)
end

let regs : map tag = Value.Tag.register
    ~name:"tainted_regs"
    ~uuid:"03c90a60-e19f-43cc-8049-fdeb23973396"
    (module Taint_map)

let ptrs : map tag = Value.Tag.register
    ~name:"tainted_ptrs"
    ~uuid:"ecf96df5-f706-4f95-a421-3fa9b91ad8bd"
    (module Taint_map)

let create = ident

let get_taints from key = match Map.find from key with
  | None -> Taints.empty
  | Some ts -> ts

let collect_taints =
  Map.fold ~init:Taints.empty ~f:(fun ~key:_ ~data:ts ts' ->
      Set.union ts ts')

class context = object(self)
  val tvs : set values = Values.empty
  val tas : set Addr.Map.t = Addr.Map.empty

  method taint_reg r ts =
    {< tvs = Values.add tvs ~key:(Bil.Result.id r) ~data:ts >}

  method taint_ptr a (s : size) ts =
    let addrs = Seq.init (Size.in_bytes s) ~f:(fun n -> Addr.(a++n)) in
    let tas' = Seq.fold addrs ~init:tas ~f:(fun tas a ->
        Map.add tas ~key:a ~data:ts) in
    {< tas = tas' >}

  (** T(r) = { t : t |-> v}  *)
  method reg_taints r = get_taints tvs (Bil.Result.id r)
  method ptr_taints r = get_taints tas r
  method all_taints =
    Set.union (collect_taints tvs) (collect_taints tas)
end

let union_map m1 m2 ~f =
  Map.merge m1 m2 ~f:(fun ~key -> function
      | `Both (v1,v2) -> Some (f v1 v2)
      | `Left v | `Right v -> Some v)

let merge : map -> map -> map =
  union_map ~f:Set.union


let pp_set ppf (set : set) =
  let rec pp ppf = function
    | [] -> ()
    | [x] -> Taint.pp ppf x
    | x :: xs -> Format.fprintf ppf "%a;@ %a" Taint.pp x pp xs in
  Format.fprintf ppf "{@,%a}" pp (Set.to_list set)

let pp_map ppf (map : map) =
  Format.fprintf ppf "@[{@;";
  Map.iteri map ~f:(fun ~key ~data ->
      Format.fprintf ppf "@[<2>%a => %a@]}"
        Var.pp key pp_set data)


class ['a] propagator = object(self)
  constraint 'a = #context
  inherit ['a] expi as super

  method! eval_binop op e1 e2 =
    super#eval_binop op e1 e2 >>= self#eval2 e1 e2
  method! eval_unop op e =
    super#eval_unop op e >>= self#eval e
  method! eval_cast ct n e =
    super#eval_cast ct n e >>= self#eval e
  method! eval_concat e1 e2 =
    super#eval_concat e1 e2 >>= self#eval2 e1 e2
  method! eval_extract n1 n2 e =
    super#eval_extract n1 n2 e >>= self#eval e

  method! eval_store ~mem ~addr v e s =
    self#eval_exp v >>= fun rv ->
    super#eval_store ~mem ~addr v e s >>= fun rr ->
    self#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return rr
    | Bil.Imm a ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_ptr a s (ctxt#reg_taints rv)) >>= fun () ->
      SM.return rr

  method! eval_load ~mem ~addr e s =
    super#eval_load ~mem ~addr e s >>= fun r ->
    super#eval_exp addr >>| Bil.Result.value >>= function
    | Bil.Bot | Bil.Mem _ -> SM.return r
    | Bil.Imm a ->
      SM.get () >>= fun ctxt ->
      SM.put (ctxt#taint_reg r (ctxt#ptr_taints a)) >>= fun () ->
      SM.return r

  method private eval2 e1 e2 r3 =
    self#eval_exp e1 >>= fun r1 ->
    self#eval_exp e2 >>= fun r2 ->
    self#propagate [r1;r2] r3 >>= fun () ->
    SM.return r3

  method private eval e rr =
    self#eval_exp e >>= fun re ->
    self#propagate [re] rr >>= fun () ->
    SM.return rr

  method private propagate args rr =
    SM.get () >>= fun ctxt ->
    let taints = Taints.union_list @@
      List.map args ~f:(fun a -> ctxt#reg_taints a) in
    SM.put (ctxt#taint_reg rr taints)
end

module Map = Taint_map
