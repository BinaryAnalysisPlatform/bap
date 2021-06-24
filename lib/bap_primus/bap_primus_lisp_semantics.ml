open Core_kernel
open Bap.Std
open Bap_core_theory
open Monads.Std

open Bap_primus_lisp_types
open Bap_primus_lisp_attributes

module Attribute = Bap_primus_lisp_attribute
module Program = Bap_primus_lisp_program
module Source = Bap_primus_lisp_source
module Resolve = Bap_primus_lisp_resolve
module Def = Bap_primus_lisp_def
module Type = Bap_primus_lisp_type
module Key = Bap_primus_lisp_program.Items

module Meta = struct
  module State = struct
    type t = {
      binds : unit Theory.Value.t Map.M(Theory.Var.Top).t;
      arith : (module Bitvec.S);
      scope : unit Theory.var list Map.M(Theory.Var.Top).t;
    }
  end
  include Monad.State.T1(State)(KB)
  include Monad.State.Make(State)(KB)
end

open Meta.Syntax
open Meta.Let

type value = unit Theory.Value.t
type effect = unit Theory.Effect.t

type KB.Conflict.t +=
  | Unresolved_definition of string
  | User_error of string

let package = "bap"
let language = Theory.Language.declare ~package "primus-lisp"

let program =
  KB.Class.property Theory.Source.cls "primus-lisp-program"
    ~public:true
    ~package @@
  KB.Domain.flat "lisp-program"
    ~empty:Program.empty
    ~equal:Program.equal
    ~join:(fun x y -> Ok (Program.merge x y))

type program = {
  prog : Program.t;
  places : unit Theory.var Map.M(KB.Name).t;
}

let typed = KB.Class.property Theory.Source.cls "typed-program"
    ~package @@
  KB.Domain.optional "typed-lisp-program"
    ~equal:(fun x y ->
        Program.equal x.prog y.prog)

let fail s = Meta.lift (KB.fail s)

let unresolved name problem =
  let msg =
    Format.asprintf "Failed to find a definition for %a. %a"
      KB.Name.pp name
      Resolve.pp_resolution problem in
  fail (Unresolved_definition msg)

let resolve prog item name =
  match Resolve.semantics prog item name () with
  | None -> !!None
  | Some (Error problem) -> unresolved name problem
  | Some (Ok (fn,_)) -> !!(Some fn)

let check_arg _ _ = true

let is_external def =
  not @@ Set.is_empty (Attribute.Set.get External.t (Def.attributes def))

type info = {
  types : (Theory.Target.t -> Type.signature);
  docs : string;
}

let library = Hashtbl.create (module KB.Name)

module Property = struct
  let name = KB.Class.property Theory.Program.cls ~package "lisp-name" @@
    KB.Domain.optional "lisp-name"
      ~equal:KB.Name.equal
      ~inspect:KB.Name.sexp_of_t

  type args = Theory.Value.Top.t list [@@deriving equal, sexp]


  type KB.conflict += Unequal_arity

  let args = KB.Class.property Theory.Program.cls ~package "lisp-args" @@
    KB.Domain.optional "lisp-args"
      ~equal:equal_args
      ~inspect:sexp_of_args
      ~join:(fun xs ys ->
          List.map2 xs ys ~f:(KB.Domain.join Theory.Value.Top.domain) |>
          function
          | Ok rs -> Result.all rs
          | Unequal_lengths -> Error Unequal_arity)
end

let definition =
  KB.Class.property Theory.Program.cls "lisp-definition"
    ~package
    ~public:true
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = Theory.Label.t option [@@deriving bin_io]
                 end)) @@
  KB.Domain.optional "label"
    ~equal:Theory.Label.equal
    ~inspect:Theory.Label.sexp_of_t

let declare
    ?(types=fun _ -> Type.{
        args = [];
        rest = Some any;
        ret = any;
      })
    ?(docs="undocumented") ?package name =
  let name = KB.Name.create ?package name in
  if Hashtbl.mem library name
  then invalid_argf "A primitive `%s' already exists, please \
                     choose a different name for your primitive"
      (KB.Name.show name) ();
  Hashtbl.add_exn library name {
    docs;
    types
  }


let sort = Theory.Value.sort
let size x = Theory.Bitv.size (sort x)
let lisp_machine =
  Theory.Effect.Sort.(join [data "unrepresented-lisp-machine"] [top])

let forget = Theory.Value.forget
let create eff res =
  KB.Value.put Theory.Semantics.value eff (forget res)


let symbol =
  KB.Class.property Theory.Value.cls "lisp-symbol" @@
  KB.Domain.optional "symbol"
    ~equal:String.equal
    ~inspect:(fun x -> Sexp.Atom x)

let static_slot =
  KB.Class.property Theory.Value.cls "static-value"
    ~package
    ~public:true
    ~persistent:(KB.Persistent.of_binable (module struct
                   type t = Bitvec_binprot.t option [@@deriving bin_io]
                 end)) @@
  KB.Domain.optional "bitvec"
    ~equal:Bitvec.equal
    ~inspect:(fun x -> Sexp.Atom (Bitvec.to_string x))

let update_value r f =
  let v = KB.Value.get Theory.Semantics.value r in
  KB.Value.put Theory.Semantics.value r (f v)

let set_static r x = update_value r @@ fun v ->
  KB.Value.put static_slot v (Some x)

let symsort = Bap_primus_value.Index.key_width
let res = KB.Value.get Theory.Semantics.value
let bits = Theory.Bitv.define

let (!) = KB.(!!)

let empty = Theory.Effect.empty Theory.Effect.Sort.bot

let intern name =
  let open KB.Syntax in
  let name = KB.Name.read name in
  KB.Symbol.intern (KB.Name.unqualified name) Theory.Value.cls >>|
  KB.Object.id >>| Int63.to_int64 >>|
  Bitvec.M64.int64

let make_reg var =
  let open KB.Syntax in
  let empty = Theory.Value.empty (Theory.Var.sort var) in
  let name = Theory.Var.name var in
  intern name >>| fun value ->
  let res = KB.Value.put symbol empty (Some name) in
  KB.Value.put static_slot res (Some value)

let sym str =
  let v = update_value empty @@ fun v ->
    KB.Value.put symbol v (Some str) in
  match str with
  | "nil" -> Meta.return@@set_static v Bitvec.zero
  | name ->
    Meta.lift @@
    intern name >>|
    set_static v


let static x =
  KB.Value.get static_slot (res x)

let reify_sym x = match static x with
  | Some _ -> Meta.return x
  | None -> match KB.Value.get symbol (res x) with
    | None -> Meta.return x
    | Some name ->
      Meta.lift @@
      intern name >>|
      set_static x

let is_machine_var t v =
  Set.mem (Theory.Target.vars t) (Theory.Var.forget v)

let machine_var_by_name t name =
  Set.find (Theory.Target.vars t) ~f:(fun v ->
      String.equal (Theory.Var.name v) name)

let make_var ?t:constr places target name  =
  let word = Theory.Target.bits target in
  match Map.find places name with
  | Some v -> v
  | None ->
    let t = Option.value constr ~default:word in
    Theory.Var.forget@@Theory.Var.define (bits t) (KB.Name.to_string name)

let lookup_parameter prog v =
  let v = KB.Name.read @@ Theory.Var.name v in
  let name = KB.Name.unqualified v in
  Program.in_package (KB.Name.package v) prog @@ fun prog ->
  Program.get prog Key.para |>
  List.find ~f:(fun p ->
      String.equal (Def.name p) name) |> function
  | None -> None
  | Some p -> Some (Def.Para.default p)

let is_parameter prog v = Option.is_some (lookup_parameter prog v)

module Env = struct
  open Meta.State

  let lookup v =
    let v = Theory.Var.forget v in
    Meta.get () >>| fun {binds} ->
    Map.find binds v


  let set v x =
    Meta.update @@ fun s -> {
      s with binds = Map.set s.binds
                 (Theory.Var.forget v) x
    }

  let del v =
    Meta.update @@ fun s -> {
      s with binds = Map.remove s.binds (Theory.Var.forget v)
    }

  let is_bound v = lookup v >>| Option.is_some

  let var word {data={exp=n; typ=t}} =
    let s = match t with
      | Any | Name _ -> word
      | Symbol -> symsort
      | Type m -> m in
    Theory.Var.forget@@Theory.Var.define (bits s) (KB.Name.to_string n)

  let set_args ws bs =
    Meta.get () >>= fun s ->
    let binds,old =
      List.fold bs ~init:(s.binds,[]) ~f:(fun (s,old) (v,x) ->
          let v = var ws v in
          Map.set s v x,(v,Map.find s v) :: old) in
    Meta.put {s with binds} >>| fun () ->
    List.rev old

  let del_args bs = Meta.update @@ fun s -> {
      s with binds = List.fold bs ~init:s.binds ~f:(fun s (v,x) ->
      match x with
      | None -> Map.remove s v
      | Some x -> Map.set s v x)
    }
end

module Scope = struct
  let forget = Theory.Var.forget

  let push orig =
    let orig = forget orig in
    Meta.lift@@Theory.Var.fresh (Theory.Var.sort orig) >>= fun v ->
    Meta.update  (fun s -> {
          s with scope = Map.update s.scope orig ~f:(function
        | None -> [v]
        | Some vs -> v::vs)
        }) >>| fun () ->
    v

  let lookup orig =
    let+ {scope} = Meta.get () in
    match Map.find scope orig with
    | None | Some [] -> None
    | Some (x :: _) -> Some x

  let pop orig =
    Meta.update @@ fun s -> {
      s with scope = Map.change s.scope (forget orig) ~f:(function
        | None | Some [] | Some [_] -> None
        | Some (_::xs) -> Some xs)
    }

  let clear =
    let* s = Meta.get () in
    let+ () = Meta.put {
        s with scope = Map.empty (module Theory.Var.Top)
      } in
    s.scope

  let restore scope = Meta.update @@ fun s -> {
      s with scope
    }

end

module Prelude(CT : Theory.Core) = struct
  let null = KB.Object.null Theory.Program.cls
  let fresh = Meta.lift (KB.Object.create Theory.Program.cls)

  let rec seq = function
    | [] -> Meta.lift@@CT.perform Theory.Effect.Sort.bot
    | [x] -> x
    | x :: xs ->
      let* xs = seq xs in
      let* x = x in
      Meta.lift@@CT.seq (KB.return x) (KB.return xs)

  let skip = CT.perform Theory.Effect.Sort.bot
  let pass = CT.perform Theory.Effect.Sort.bot

  let pure res =
    res >>| fun res ->
    create empty res

  let bigint x m =
    let s = bits m in
    let m = Bitvec.modulus m in
    let x = Bitvec.(bigint x mod m) in
    pure @@ Meta.lift (CT.int s x) >>| fun r ->
    set_static r x

  let (:=) v x = Meta.lift@@CT.set v x

  let full eff res =
    seq eff >>= fun eff ->
    res >>| fun res ->
    create eff res

  let data xs =
    let* data = seq xs in
    Meta.lift@@CT.blk null !data skip

  let ctrl xs =
    let* ctrl = seq xs in
    Meta.lift@@CT.blk null pass !ctrl

  let blk lbl xs = seq [
      Meta.lift@@CT.blk lbl pass skip;
      seq xs;
    ]

  let cast s x =
    Meta.lift@@CT.cast (bits s) CT.b0 !x

  let nil = !!(Theory.Value.empty Theory.Bool.t)
  let undefined = full [] nil
  let purify eff =
    full [] !!(res eff)

  let unified x y f =
    Theory.Value.Match.(begin
        let|() = both
            Theory.Bitv.refine x
            Theory.Bitv.refine y @@ fun x y ->
          let s = Int.max (size x) (size y) in
          cast s x >>= fun x ->
          cast s y >>= fun y ->
          f x y in
        undefined
      end)

  let coerce_bits s x f =
    let open Theory.Value.Match in
    let| () = can Theory.Bitv.refine x @@ fun x ->
      Meta.lift@@CT.cast s CT.b0 !x >>= f in
    let| () = can Theory.Bool.refine x @@ fun cnd ->
      Meta.lift@@CT.ite !cnd
        (CT.int s Bitvec.one)
        (CT.int s Bitvec.zero) >>= fun x ->
      f x in
    undefined

  let coerce_bool x f =
    let open Theory.Value.Match in
    let| () = can Theory.Bool.refine x f in
    let| () = can Theory.Bitv.refine x @@ fun x ->
      Meta.lift@@CT.non_zero !x >>= fun x -> f x in
    undefined

  let is_static eff = Option.is_some (static eff)

  let assign ?(local=false) target v eff =
    let v = Theory.Var.forget v in
    match static eff with
    | Some _ when local || not (is_machine_var target v) ->
      Env.set v (res eff) >>= fun () ->
      purify eff
    | _ ->
      Env.del v >>= fun () ->
      full [!!eff; data [v := !(res eff)]] !!(res eff)

  let reify ppf program defn target name args =
    let word = Theory.Target.bits target in
    let {prog; places} = program in
    let var ?t n = make_var ?t places target n in
    let rec eval : ast -> unit Theory.Effect.t Meta.t = function
      | {data=Int {data={exp=x; typ=Type m}}} -> bigint x m
      | {data=Int {data={exp=x}}} -> bigint x word
      | {data=Var {data={exp=n; typ=Type t}}} -> lookup@@var ~t n
      | {data=Var {data={exp=n}}} -> lookup@@var n
      | {data=Sym {data=s}} -> sym (KB.Name.unqualified s)
      | {data=Ite (cnd,yes,nay)} -> ite cnd yes nay
      | {data=Let ({data={exp=n; typ=Type t}},x,y)} -> let_ ~t n x y
      | {data=Let ({data={exp=n}},x,y)} -> let_ n x y
      | {data=App (Dynamic name,args)} -> app name args
      | {data=Seq xs} -> seq_ xs
      | {data=Set ({data={exp=n; typ=Type t}},x)} -> set_ (var ~t n) x
      | {data=Set ({data={exp=n}},x)} -> set_ (var n) x
      | {data=Rep (cnd,body)} -> rep cnd body
      | {data=Msg (fmt,args)} -> msg fmt args
      | {data=Err msg} -> err msg
      | _ -> undefined
    and ite cnd yes nay =
      let* cnd = eval cnd in
      match static cnd with
      | Some cnd ->
        if Bitvec.(equal cnd zero)
        then eval nay
        else eval yes
      | None ->
        coerce_bool (res cnd) @@ fun cres ->
        Meta.lift@@Theory.Var.fresh Theory.Bool.t >>= fun c ->
        let* yes = eval yes in
        let* nay = eval nay in
        full [
          !!cnd;
          data [c := !cres];
          Meta.lift@@CT.branch (CT.var c) !yes !nay;
        ] @@
        Meta.lift@@CT.ite (CT.var c) !(res yes) !(res nay)
    and rep cnd body =
      let* r = eval cnd in
      match static r with
      | Some x ->
        if Bitvec.(equal x zero)
        then !!r
        else
          eval body >>= fun _ ->
          rep cnd body
      | None ->
        let* body = eval body in
        let* head = fresh and* loop = fresh and* tail = fresh in
        coerce_bool (res r) @@ fun cres ->
        full [
          blk head [ctrl [Meta.lift@@CT.goto tail]];
          blk loop [!!body];
          blk tail [!!r; ctrl [
              Meta.lift@@CT.branch !cres (CT.goto head) skip
            ]]
        ] !!cres
    and call ?(toplevel=false) name xs =
      match Resolve.defun check_arg prog Key.func name xs with
      | Some (Ok (fn,_)) when is_external fn ->
        sym (Def.name fn) >>= fun dst ->
        prim "invoke-subroutine" (res dst::xs)
      | Some (Ok (fn,bs)) ->
        Env.set_args word bs >>= fun bs ->
        Scope.clear >>= fun scope ->
        eval (Def.Func.body fn) >>= fun eff ->
        Scope.restore scope >>= fun () ->
        Env.del_args bs >>= fun () ->
        !!eff
      | Some (Error problem) ->
        unresolved name problem
      | None when toplevel -> !!Insn.empty
      | None ->
        match Resolve.semantics prog Key.semantics name () with
        | Some Ok (sema,()) ->
          Meta.lift@@
          Def.Sema.apply sema defn xs
        | Some (Error problem) -> unresolved name problem
        | None ->
          let msg = Format.asprintf "No definition is found for %a"
              KB.Name.pp name in
          fail (Unresolved_definition msg)
    and app name xs =
      map xs >>= fun (aeff,xs) ->
      call name xs >>= fun peff ->
      full [!!aeff; !!peff] !!(res peff)
    and map args =
      seq [] >>= fun eff ->
      Meta.List.fold args ~init:(eff,[]) ~f:(fun (eff,args) arg ->
          let* eff' = eval arg in
          let+ eff = seq [!!eff; !!eff'] in
          (eff,forget (res eff')::args)) >>| fun (eff,args) ->
      eff, List.rev args
    and seq_ xs =
      pure nil >>= fun init ->
      Meta.List.fold ~init xs ~f:(fun eff x  ->
          let* eff' = eval x in
          full [!!eff; !!eff'] !!(res eff'))
    and msg fmt args =
      map args >>= fun (aeff,args) ->
      List.iter fmt ~f:(function
          | Lit s -> Format.printf "%s" s
          | Pos n -> match List.nth args n with
            | None -> failwithf "bad positional %d" n ()
            | Some v -> match KB.Value.get static_slot v with
              | Some v -> Format.printf "%a" Bitvec.pp v
              | None -> Format.printf "@[<hv>%a@]" KB.Value.pp v);
      Format.fprintf ppf "@\n";
      !!aeff
    and err msg = fail (User_error msg)
    and lookup v =
      Scope.lookup v >>= function
      | Some v -> lookup v
      | None ->
        Env.lookup v >>= function
        | Some v -> pure !!v
        | None -> match lookup_parameter prog v with
          | Some def -> eval def >>= assign target v
          | None -> pure@@Meta.lift@@CT.var v
    and set_ v x =
      Scope.lookup v >>= function
      | Some v -> eval x >>= assign target ~local:true v
      | None -> eval x >>= assign target v
    and let_ ?(t=word) v x b =
      let* xeff = eval x in
      let orig = Theory.Var.define (bits t) (KB.Name.to_string v) in
      if is_parameter prog orig
      then
        Env.set orig (res xeff) >>= fun () ->
        let* beff = eval b in
        Env.del orig >>= fun () ->
        full [!!beff] !!(res beff)
      else
        Scope.push orig >>= fun v ->
        let* aeff = assign ~local:true target v xeff in
        let* beff = eval b in
        Scope.pop orig >>= fun () ->
        full [
          !!aeff;
          !!beff;
        ] !!(res beff)
    and prim ?(package="core") name args =
      call (KB.Name.read ~package name) args >>= reify_sym in
    match args with
    | Some args ->
      call ~toplevel:true name args
    | None ->
      resolve prog Key.func name >>= function
      | Some fn ->
        eval (Def.Func.body fn)
      | None -> !!Insn.empty
end

module Unit = struct
  open KB.Syntax
  open KB.Let

  let slot = KB.Class.property Theory.Unit.cls "lisp-unit"
      ~package
      ~public:true @@ KB.Domain.optional "unit-name"
      ~inspect:sexp_of_string
      ~equal:equal_string


  let create ?(name="core") target : Theory.Unit.t KB.t =
    let* unit = KB.Symbol.intern ~package:"lisp" name Theory.Unit.cls in
    KB.sequence [
      KB.provide slot unit (Some name);
      KB.provide Theory.Unit.target unit target
    ] >>| fun () ->
    unit

  let is_lisp obj =
    KB.collect slot obj >>| Option.is_some

  let language = language
end

type KB.conflict += Illtyped_program of Program.Type.error list

let primitive name defn args =
  let open KB.Syntax in
  KB.Object.scoped Theory.Program.cls @@ fun obj ->
  KB.sequence [
    KB.provide Property.name obj (Some name);
    KB.provide definition obj (Some defn);
    KB.provide Property.args obj (Some args);
  ] >>= fun () ->
  KB.collect Theory.Semantics.slot obj

let link_library target prog =
  Hashtbl.fold library ~f:(fun ~key:name ~data:{types; docs} prog ->
      let types = types target in
      Program.add prog Program.Items.semantics @@
      Def.Sema.create ~docs ~types name (primitive name))
    ~init:prog

let obtain_typed_program unit =
  let open KB.Syntax in
  KB.collect Theory.Unit.source unit >>= fun src ->
  KB.collect Theory.Unit.target unit >>= fun target ->
  match KB.Value.get typed src with
  | Some prog -> !!prog
  | None ->
    let prog =
      link_library target @@
      Program.with_places (KB.Value.get program src) target in
    let tprog = Program.Type.infer prog in
    let prog = Program.Type.program tprog in
    let places = Program.fold prog Key.place
        ~f:(fun ~package place places ->
            let name = KB.Name.create ~package (Def.name place) in
            Map.set places name (Def.Place.location place))
        ~init:(Map.empty (module KB.Name)) in
    let program = {prog; places} in
    match Program.Type.errors tprog with
    | [] ->
      let src = KB.Value.put typed src (Some program) in
      KB.provide Theory.Unit.source unit src >>| fun () ->
      program
    | errs -> KB.fail (Illtyped_program errs)


let typed_program unit =
  let open KB.Syntax in
  obtain_typed_program unit >>| fun {prog} -> prog


let provide_semantics ?(stdout=Format.std_formatter) () =
  let open KB.Syntax in
  KB.Rule.(begin
      declare "primus-lisp-semantics" |>
      require Property.name |>
      require Property.args |>
      require Theory.Label.unit |>
      require Theory.Unit.source |>
      require Theory.Unit.target |>
      require program |>
      provide Theory.Semantics.slot |>
      comment "reifies Primus Lisp definitions"
    end);
  let (>>=?) x f = x >>= function
    | None -> !!Insn.empty
    | Some x -> f x in
  KB.promise Theory.Semantics.slot @@ fun obj ->
  KB.collect Theory.Label.unit obj >>=? fun unit ->
  KB.collect Property.name obj >>=? fun name ->
  KB.collect Property.args obj >>= fun args ->
  obtain_typed_program unit >>= fun prog ->
  KB.collect Theory.Unit.target unit >>= fun target ->
  let bits = Theory.Target.bits target in
  let module Arith = Bitvec.Make(struct
      let modulus = Bitvec.modulus bits
    end) in
  let meta = Meta.State.{
      binds = Map.empty (module Theory.Var.Top);
      scope = Map.empty (module Theory.Var.Top);
      arith = (module Arith);
    } in
  Theory.instance () >>= Theory.require >>= fun (module Core) ->
  let open Prelude(Core) in
  Meta.run (reify stdout prog obj target name args) meta >>= fun (res,_) ->
  KB.collect Disasm_expert.Basic.Insn.slot obj >>| function
  | Some basic when Insn.(res <> empty) ->
    Insn.with_basic res basic
  | _ -> res

let provide_attributes () =
  let open KB.Syntax in
  let empty = Attribute.Set.empty in
  let (>>=?) x f = x >>= function
    | None -> !!empty
    | Some x -> f x in
  KB.promise Attribute.Set.slot @@ fun this ->
  KB.collect Theory.Label.unit this >>=? fun unit ->
  KB.collect Property.name this >>=? fun name ->
  obtain_typed_program unit >>= fun {prog} ->
  let package = KB.Name.package name
  and name = KB.Name.unqualified name in
  Program.in_package package prog @@ fun prog ->
  Program.get prog Key.func |>
  List.fold ~init:(Ok empty) ~f:(fun attrs fn ->
      if String.equal (Def.name fn) name
      then match attrs with
        | Error c -> Error c
        | Ok attrs ->
          KB.Value.join attrs (Def.attributes fn)
      else attrs) |> function
  | Ok attrs -> !!attrs
  | Error conflict -> KB.fail conflict

let enable ?stdout () =
  provide_semantics ?stdout ();
  provide_attributes ()

let static = static_slot

let () = KB.Conflict.register_printer @@ function
  | Unresolved_definition msg -> Option.some @@ sprintf "%s" msg
  | Property.Unequal_arity ->
    Some "The number of arguments is different"
  | User_error msg -> Some ("error: " ^ msg)
  | Illtyped_program errs ->
    let open Format in
    let msg = asprintf "%a"
        (pp_print_list
           ~pp_sep:pp_print_newline
           Program.Type.pp_error) errs in
    Some msg
  | _ -> None

include Property
