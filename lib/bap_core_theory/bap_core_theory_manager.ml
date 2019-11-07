open Core_kernel
open Bap_knowledge

open Bap_core_theory_definition
open Bap_core_theory_value
open Knowledge.Syntax

let user_package = "user"

module Value = Knowledge.Value
module Name = Knowledge.Name

type 'a theory = {
  name : Set.M(Name).t;
  desc : string;
  requires : Set.M(String).t;
  provides : Set.M(String).t;
  structure : 'a;
  is_empty : Base.bool;
  id : Set.M(Name).t;
}

type core = (module Core)
let known_theories : (Name.t, core theory) Hashtbl.t =
  Hashtbl.create (module Name)

let features = Set.of_list (module String)
let names = Set.of_list (module Name)

module Empty = struct
  let name = Name.create ~package:"core-theory" "empty"
  let requires = ["empty"]
  let provides = [Name.show name]
  module Self = Bap_core_theory_empty.Core
  let theory = {
    is_empty = true;
    name = names [name];
    desc = "The empty theory.";
    requires = Set.of_list (module String) requires;
    provides = Set.of_list (module String) provides;
    structure = (module Self : Core);
    id = names [name];
  }
end

let check_uniqueness name =
  match Hashtbl.find known_theories name with
  | None -> ()
  | Some {desc} ->
    invalid_argf "Theory.declare: Name exists. \
                  A theory with the name `%a' is already declared \
                  by some other component. \
                  The other component provided %s."
      Name.str name
      (if desc = ""
       then "no description to its theory"
       else sprintf "the following description to its theory: %S" desc)
      ()

let declare
    ?(desc="")
    ?(extends=[])
    ?(context=[])
    ?(provides=[])
    ?package ~name structure =

  let name = Name.create ?package name in
  let extends = List.map extends ~f:Name.read in
  check_uniqueness name;
  Hashtbl.add_exn known_theories name {
    is_empty = false;
    name = names (name :: Empty.name :: extends);
    desc; structure;
    requires = features context;
    provides = Set.add (features provides) (Name.show name);
    id = names [name];
  }

let (++) x y =
  x >>= fun x ->
  y >>| fun y ->
  Value.merge x y
[@@inline]

let join1 p q x =
  x >>= fun x ->
  p !!x ++ q !!x
[@@inline]

let join1s p q s x =
  x >>= fun x ->
  p s !!x ++ q s !!x
[@@inline]

let join2 p q x y =
  x >>= fun x ->
  y >>= fun y ->
  p !!x !!y ++ q !!x !!y
[@@inline]

let join2s p q s x y =
  x >>= fun x ->
  y >>= fun y ->
  p s !!x !!y ++ q s !!x !!y
[@@inline]

let join3 p q x y z =
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  p !!x !!y !!z ++ q !!x !!y !!z
[@@inline]

let join3s p q s x y z =
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  p s !!x !!y !!z ++ q s !!x !!y !!z
[@@inline]

let join4 p q r x y z =
  r >>= fun r ->
  x >>= fun x ->
  y >>= fun y ->
  z >>= fun z ->
  p !!r !!x !!y !!z ++ q !!r !!x !!y !!z
[@@inline]

let joinN p q xs =
  KB.List.map ~f:KB.return xs >>= fun xs ->
  p xs ++ q xs

module Join(P : Core)(Q : Core) : Core = struct
  let var v             = P.var v ++ Q.var v
  let int s x           = P.int s x ++ Q.int s x
  let unk s             = P.unk s ++ Q.unk s
  let b0                = P.b0 ++ Q.b0
  let b1                = P.b1 ++ Q.b1
  let inv x             = join1 P.inv Q.inv x
  let and_ x y          = join2 P.and_ Q.and_ x y
  let or_ x y           = join2 P.or_ Q.or_ x y
  let msb x             = join1 P.msb Q.msb x
  let lsb x             = join1 P.lsb Q.lsb x
  let neg x             = join1 P.neg Q.neg x
  let not x               = join1 P.not Q.not x
  let add x y           = join2 P.add Q.add x y
  let sub x y           = join2 P.sub Q.add x y
  let mul x y           = join2 P.mul Q.mul x y
  let div x y           = join2 P.div Q.div x y
  let sdiv x y          = join2 P.sdiv Q.sdiv x y
  let modulo x y        = join2 P.modulo Q.modulo x y
  let smodulo x y       = join2 P.smodulo Q.smodulo x y
  let logand x y        = join2 P.logand Q.logand x y
  let logor x y         = join2 P.logor Q.logor x y
  let logxor x y        = join2 P.logxor Q.logxor x y
  let shiftr b x y      = join3 P.shiftr Q.shiftr b x y
  let shiftl b x y      = join3 P.shiftl Q.shiftl b x y
  let ite b x y         = join3 P.ite Q.ite b x y
  let sle x y           = join2 P.sle Q.sle x y
  let ule x y           = join2 P.ule Q.ule x y
  let cast s x y        = join2s P.cast Q.cast s x y
  let concat s xs       = joinN (P.concat s) (Q.concat s) xs
  let append s x y      = join2s P.append Q.append s x y
  let load m k          = join2 P.load Q.load m k
  let store m k v       = join3 P.store Q.store m k v
  let perform s         = P.perform s ++ Q.perform s
  let set v x           = join1s P.set Q.set v x
  let let_ v x b        = join2s P.let_ Q.let_ v x b
  let jmp d             = join1 P.jmp Q.jmp d
  let goto d            = P.goto d ++ Q.goto d
  let seq x y           = join2 P.seq Q.seq x y
  let blk l x y         = join2s P.blk Q.blk l x y
  let repeat b x        = join2 P.repeat Q.repeat b x
  let branch b x y      = join3 P.branch Q.branch b x y
  let zero s            = P.zero s ++ Q.zero s
  let is_zero x         = join1 P.is_zero Q.is_zero x
  let non_zero x        = join1 P.non_zero Q.non_zero x
  let succ x            = join1 P.succ Q.succ x
  let pred x            = join1 P.pred Q.pred x
  let nsucc x n         = x >>= fun x -> P.nsucc !!x n ++ Q.nsucc !!x n
  let npred x n         = x >>= fun x -> P.npred !!x n ++ Q.npred !!x n
  let high s x          = join1s P.high Q.high s x
  let low s x           = join1s P.low Q.low s x
  let signed s x        = join1s P.signed Q.signed s x
  let unsigned s x      = join1s P.unsigned Q.unsigned s x
  let extract s x y z   = join3s P.extract Q.extract s x y z
  let loadw s d m k     = join3s P.loadw Q.loadw s d m k
  let storew d m k x    = join4 P.storew Q.storew d m k x
  let arshift x y       = join2 P.arshift Q.arshift x y
  let rshift x y        = join2 P.rshift Q.rshift x y
  let lshift x y        = join2 P.lshift Q.lshift x y
  let eq x y            = join2 P.eq Q.eq x y
  let neq x y           = join2 P.neq Q.neq x y
  let slt x y           = join2 P.slt Q.slt x y
  let ult x y           = join2 P.ult Q.ult x y
  let sgt x y           = join2 P.sgt Q.sgt x y
  let ugt x y           = join2 P.ugt Q.ugt x y
  let sge x y           = join2 P.sge Q.sge x y
  let uge x y           = join2 P.uge Q.uge x y
  let rne               = P.rne ++ Q.rne
  let rna               = P.rna ++ Q.rna
  let rtp               = P.rtp ++ Q.rtp
  let rtn               = P.rtn ++ Q.rtn
  let rtz               = P.rtz ++ Q.rtz
  let requal x y        = join2 P.requal Q.requal x y
  let float s x         = join1s P.float Q.float s x
  let fbits x           = join1 P.fbits Q.fbits x
  let is_finite x       = join1 P.is_finite Q.is_finite x
  let is_fzero x        = join1 P.is_fzero Q.is_fzero x
  let is_fneg x         = join1 P.is_fneg Q.is_fneg x
  let is_fpos x         = join1 P.is_fpos Q.is_fpos x
  let is_nan x          = join1 P.is_nan Q.is_nan x
  let is_inf x          = join1 P.is_inf Q.is_inf x
  let cast_float s m x  = join2s P.cast_float Q.cast_float s m x
  let cast_sfloat s m x = join2s P.cast_sfloat Q.cast_sfloat s m x
  let cast_int s m x    = join2s P.cast_int Q.cast_int s m x
  let cast_sint s m x   = join2s P.cast_sint Q.cast_sint s m x
  let fneg x            = join1 P.fneg Q.fneg x
  let fabs x            = join1 P.fabs Q.fabs x
  let fadd m x y        = join3 P.fadd Q.fadd m x y
  let fsub m x y        = join3 P.fsub Q.fsub m x y
  let fmul m x y        = join3 P.fmul Q.fmul m x y
  let fdiv m x y        = join3 P.fdiv Q.fdiv m x y
  let fmodulo m x y     = join3 P.fmodulo Q.fmodulo m x y
  let fmad m x y z      = join4 P.fmad Q.fmad m x y z
  let fround m x        = join2 P.fround Q.fround m x
  let fconvert s x y    = join2s P.fconvert Q.fconvert s x y
  let fsucc x           = join1 P.fsucc Q.fsucc x
  let fpred x           = join1 P.fpred Q.fpred x
  let forder x y        = join2 P.forder Q.forder x y
  let pow m x y         = join3 P.pow Q.pow m x y
  let compound m x y    = join3 P.compound Q.compound m x y
  let rootn m x y       = join3 P.rootn Q.rootn m x y
  let pown m x y        = join3 P.pown Q.pown m x y
  let hypot m x y       = join3 P.hypot Q.hypot m x y
  let fsqrt m x         = join2 P.fsqrt Q.fsqrt m x
  let rsqrt m x         = join2 P.rsqrt Q.rsqrt m x
  let exp m x           = join2 P.exp Q.exp m x
  let expm1 m x         = join2 P.expm1 Q.expm1 m x
  let exp2 m x          = join2 P.exp2 Q.exp2 m x
  let exp2m1 m x        = join2 P.exp2m1 Q.exp2m1 m x
  let exp10 m x         = join2 P.exp10 Q.exp10 m x
  let exp10m1 m x       = join2 P.exp10m1 Q.exp10m1 m x
  let log m x           = join2 P.log Q.log m x
  let log2 m x          = join2 P.log2 Q.log2 m x
  let log10 m x         = join2 P.log10 Q.log10 m x
  let logp1 m x         = join2 P.logp1 Q.logp1 m x
  let log2p1 m x        = join2 P.log2p1 Q.log2p1 m x
  let log10p1 m x       = join2 P.log10p1 Q.log10p1 m x
  let sin m x           = join2 P.sin Q.sin m x
  let cos m x           = join2 P.cos Q.cos m x
  let tan m x           = join2 P.tan Q.tan m x
  let sinpi m x         = join2 P.sinpi Q.sinpi m x
  let cospi m x         = join2 P.cospi Q.cospi m x
  let atanpi m x        = join2 P.atanpi Q.atanpi m x
  let atan2pi m x y     = join3 P.atan2pi Q.atan2pi m x y
  let asin m x          = join2 P.asin Q.asin m x
  let acos m x          = join2 P.acos Q.acos m x
  let atan m x          = join2 P.atan Q.atan m x
  let atan2 m x y       = join3 P.atan2 Q.atan2 m x y
  let sinh m x          = join2 P.sinh Q.sinh m x
  let cosh m x          = join2 P.cosh Q.cosh m x
  let tanh m x          = join2 P.tanh Q.tanh m x
  let asinh m x         = join2 P.asinh Q.asinh m x
  let acosh m x         = join2 P.acosh Q.acosh m x
  let atanh m x         = join2 P.atanh Q.atanh m x
end

let join_cores (module P : Core) (module Q : Core) : (module Core) =
  (module Join(P)(Q))

let equal x y = Set.equal x.name y.name

let set_inclusion_order x y : Knowledge.Order.partial =
  if Set.equal x y then EQ else
  if Set.is_subset x y then LT else
  if Set.is_subset y x then GT else NC

let order t1 t2 = set_inclusion_order t1.name t2.name

let merge t1 t2 = match order t1 t2 with
  | EQ -> t1
  | LT -> t2
  | GT -> t1
  | NC -> {
      name = Set.union t1.name t2.name;
      desc = ""; is_empty = false;
      requires = Set.union t1.requires t2.requires;
      provides = Set.union t1.provides t2.provides;
      structure = join_cores t1.structure t2.structure;
      id = Set.union t1.id t2.id;
    }

let join t1 t2 = Ok (merge t1 t2)

let sexp_of_name name =
  Sexp.List (Set.to_list name |>
             List.map ~f:(fun n -> Sexp.Atom (Name.show n)))

let inspect {name; desc} =
  Sexp.List (sexp_of_name name ::
             if desc = "" then [] else [Atom desc])

let domain = Knowledge.Domain.define "theory"
    ~inspect
    ~join
    ~empty:Empty.theory
    ~order

let slot = Knowledge.Class.property theory "instance" domain
    ~package:"core-theory"
    ~desc:"The theory structure"


let str_ctxt () ctxt =
  List.to_string (Set.to_list ctxt) ~f:ident

module Theory = (val Knowledge.Object.derive theory)

let theories () =
  let init = Map.empty (module Theory) in
  Hashtbl.to_alist known_theories |>
  Knowledge.List.fold ~init ~f:(fun theories (name,structure) ->
      Knowledge.Symbol.intern (Name.unqualified name) theory
        ~package:(Name.package name) >>| fun name ->
      Map.add_exn theories name structure)


let is_applicable ~provided ~requires =
  Set.for_all requires ~f:(Set.mem provided)

let is_required ~required ~provides =
  match required with
  | None -> true
  | Some required -> Set.exists provides ~f:(Set.mem required)

let without_subsumptions theories =
  List.filter theories ~f:(fun t1 ->
      not @@ List.exists theories ~f:(fun t2 ->
          match order t1 t2 with
          | LT -> true
          | _ -> false))

let refine ?(context=[]) ?requires theories =
  let provided = features context
  and required = Option.map ~f:features requires in
  without_subsumptions @@
  List.filter theories ~f:(fun {requires; provides} ->
      is_applicable ~provided ~requires &&
      is_required ~required ~provides)

let new_theory ?context ?requires () =
  theories () >>| Map.data >>| refine ?context ?requires >>= function
  | [] -> Knowledge.return Empty.theory
  | [t] -> Knowledge.return t
  | theories ->
    Knowledge.return @@
    (List.reduce_balanced_exn theories ~f:merge)

let theory_for_id id =
  let sym = sprintf "'%s" @@
    List.to_string (Set.to_list id) ~f:Name.show in
  Knowledge.Symbol.intern sym theory
    ~package:"core-theory-internal.mananager"

let instance ?context ?requires () =
  theories () >>| Map.data >>| refine ?context ?requires >>= function
  | [] -> theory_for_id Empty.theory.id
  | [t] -> theory_for_id t.id
  | theories ->
    List.fold theories ~init:(Set.empty (module Name)) ~f:(fun names t ->
        Set.union names t.id) |>
    theory_for_id >>= fun id ->
    let theory = List.reduce_balanced_exn theories ~f:merge in
    Knowledge.provide slot id theory >>| fun () ->
    id

let require name =
  let open Knowledge.Syntax in
  theories () >>= fun theories ->
  match Map.find theories name with
  | Some t -> Knowledge.return t.structure
  | None -> Knowledge.collect slot name >>| fun t ->
    t.structure
