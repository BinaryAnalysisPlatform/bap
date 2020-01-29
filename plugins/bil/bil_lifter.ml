open Core_kernel
open Bap.Std
open Bap_future.Std
open Bap_knowledge
open Bap_core_theory
open Monads.Std

open Knowledge.Syntax

open Theory.Parser
include Self()

module Call = struct
  let prefix = "bil-fixup:"
  let extern name =
    let dst = sprintf "%s%s" prefix name in
    Bil.special dst

  let is_extern name =
    String.is_prefix name ~prefix

  let dst =
    String.chop_prefix_exn ~prefix
end

module BilParser = struct
  type context = [`Bitv | `Bool | `Mem ] [@@deriving sexp]
  let fail exp ctx =
    error "ill-formed expression in %a ctxt: %a"
      Sexp.pp (sexp_of_context ctx) Exp.pp exp

  type exp = Bil.exp
  module Var = Bap.Std.Var
  let rec uncat acc : exp -> exp list = function
    | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
    | Concat (x,y) -> x::y::acc
    | x -> x::acc

  let bits_of_var v = match Var.typ v with
    | Imm x -> x
    | _ -> failwith "not a bitv var"

  let byte x = Bil.int (Word.of_int ~width:8 x)
  let is_big e =
    Bil.int @@
    if [%compare.equal : endian] e BigEndian then Word.b1 else Word.b0

  let is_reg v = match Var.typ v with
    | Type.Imm 1 | Type.Mem _ -> false
    | _ -> true

  let is_bit v = match Var.typ v with
    | Type.Imm 1 -> true
    | _ -> false

  let is_mem v = match Var.typ v with
    | Type.Mem _ -> true
    | _ -> false

  let bitv : type t r. (t,exp,r) bitv_parser =
    fun (module S) -> function
      | Cast (HIGH,n,x) -> S.high n x
      | Cast (LOW,n,x) -> S.low n x
      | Cast (UNSIGNED,n,x) -> S.unsigned n x
      | Cast (SIGNED,n,x) -> S.signed n x
      | BinOp(PLUS,x,y) -> S.add x y
      | BinOp(MINUS,x,y) -> S.sub x y
      | BinOp(TIMES,x,y) -> S.mul x y
      | BinOp(DIVIDE,x,y) -> S.div x y
      | BinOp(SDIVIDE,x,y) -> S.sdiv x y
      | BinOp(MOD,x,y) -> S.modulo x y
      | BinOp(SMOD,x,y) -> S.smodulo x y
      | BinOp(LSHIFT,x,y) -> S.lshift x y
      | BinOp(RSHIFT,x,y) -> S.rshift x y
      | BinOp(ARSHIFT,x,y) -> S.arshift x y
      | BinOp(AND,x,y) -> S.logand x y
      | BinOp(OR,x,y) -> S.logor x y
      | BinOp(XOR,x,y) -> S.logxor x y
      | UnOp(NEG,x) -> S.neg x
      | UnOp(NOT,x) -> S.not x
      | Load(m,k,e,s) ->
        S.load_word (Size.in_bits s) (is_big e) m k
      | Var v -> S.var (Var.name v) (bits_of_var v)
      | Int x -> S.int (Word.to_bitvec x) (Word.bitwidth x)
      | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
      | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
      | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
      | Ite (x,y,z) -> S.ite x y z
      | Extract (hi,lo,x) ->
        let s = max 0 (hi-lo+1) in
        S.extract s (byte hi) (byte lo) x
      | Concat (_,_) as cat -> S.concat (uncat [] cat)
      | Unknown (_, Imm s) -> S.unknown s
      | BinOp ((EQ|NEQ|LT|LE|SLT|SLE), _, _) as op ->
        S.ite op (Int Word.b1) (Int Word.b0)

      (* ill-formed expressions *)
      | Let _
      | Store (_, _, _, _, _)
      | Unknown (_, (Mem _|Unk)) as exp -> fail exp `Bitv; S.error



  let mem : type t. (t,exp) mem_parser =
    fun (module S) -> function
      | Unknown (_,Mem (k,v)) ->
        S.unknown (Size.in_bits k) (Size.in_bits v)
      | Store (m,k,v,e,_) ->
        S.store_word (is_big e) m k v
      | Var v  ->
        let with_mem_types v f = match Var.typ v with
          | Mem (ks,vs) -> f (Size.in_bits ks) (Size.in_bits vs)
          | _ -> fail (Var v) `Mem; S.error in
        with_mem_types v (S.var (Var.name v))
      | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
      | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
      | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
      | Ite (c,x,y) ->  S.ite c x y
      (* the rest is ill-formed *)
      | Let _
      | Unknown (_,_)
      | Load (_,_,_,_)
      | BinOp (_,_,_)
      | UnOp (_,_)
      | Int _
      | Cast (_,_,_)
      | Extract (_,_,_)
      | Concat (_,_) as exp -> fail exp `Mem; S.error

  let float _ _ = assert false
  let rmode _ _ = assert false

  let bool : type t r. (t,exp,r) bool_parser =
    fun (module S) -> function
      | Var x -> S.var (Var.name x)
      | Int x -> S.int (Word.to_bitvec x)
      | Cast (HIGH,1,x) -> S.high x
      | Cast (LOW,1,x) -> S.low x
      | BinOp (EQ,x,y) -> S.eq x y
      | BinOp (NEQ,x,y) -> S.neq x y
      | BinOp (LT,x,y) -> S.lt x y
      | BinOp (LE,x,y) -> S.le x y
      | BinOp (SLT,x,y) -> S.slt x y
      | BinOp (SLE,x,y) -> S.sle x y
      | BinOp (OR,x,y) -> S.logor x y
      | BinOp (AND,x,y) -> S.logand x y
      | BinOp (XOR,x,y) -> S.logxor x y
      | UnOp (NOT,x) -> S.not x
      | Let (v,y,z) when is_bit v -> S.let_bit (Var.name v) y z
      | Let (v,y,z) when is_reg v -> S.let_reg (Var.name v) y z
      | Let (v,y,z) when is_mem v -> S.let_mem (Var.name v) y z
      | Ite (x,y,z) -> S.ite x y z
      | Extract (hi,lo,x) when hi = lo -> S.extract hi x
      | Unknown (_,_) -> S.unknown ()
      | Let _
      | Extract _
      | UnOp (NEG,_)
      | Cast (_,_,_)
      | Load (_,_,_,_)
      | Store (_,_,_,_,_)
      | Concat (_,_)
      | BinOp ((PLUS|MINUS|TIMES|DIVIDE|SDIVIDE|
                MOD|SMOD|LSHIFT|RSHIFT|ARSHIFT),_,_) as exp
        -> fail exp `Bool; S.error


  let stmt : type t r. (t,exp,r,stmt) stmt_parser =
    fun (module S) ->
    let set v x =
      let n = Var.name v in
      match Var.typ v with
      | Unk ->
         error "can't reify the variable %s: unknown type" (Var.name v);
         S.error
      | Imm 1 -> S.set_bit n x
      | Imm m -> S.set_reg n m x
      | Mem (ks,vs) ->
        S.set_mem n (Size.in_bits ks) (Size.in_bits vs) x in
    function
    | Move (v,x) -> set v x
    | Jmp (Int x) -> S.goto (Word.to_bitvec x)
    | Jmp x -> S.jmp x
    | Special s when Call.is_extern s ->
      S.call (Call.dst s)
    | Special s -> S.special s
    | While (c,xs) -> S.while_ c xs
    | If (c,xs,ys) -> S.if_ c xs ys
    | CpuExn n -> S.cpuexn n


  let t = {bitv; mem; stmt; bool; float; rmode}
end

module Optimizer = Theory.Parser.Make(Bil_semantics.Core)
[@@inlined]



let provide_bir () =
  Knowledge.promise Theory.Program.Semantics.slot @@ fun obj ->
  KB.collect Theory.Program.Semantics.slot obj >>| fun sema ->
  let bir = Bil_ir.reify @@  KB.Value.get Bil_ir.slot sema in
  KB.Value.put Term.slot sema bir


module Relocations = struct

  type t = {
    rels : addr Addr.Map.t;
    exts : string Addr.Map.t;
  }

  module Fact = Ogre.Make(Monad.Ident)

  module Request = struct
    open Image.Scheme
    open Fact.Syntax

    let of_aseq s =
      Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
          Map.set m ~key ~data)

    let arch =
      Fact.collect Ogre.Query.(select (from arch)) >>= fun s ->
      Fact.Seq.reduce ~f:(fun a1 a2 ->
          if Arch.equal a1 a2 then Fact.return a1
          else Fact.failf "arch is ambiguous: %a <> %a"
              Arch.pp a1 Arch.pp a2 ())
        (Seq.filter_map ~f:Arch.of_string s) >>= fun a ->
      match a with
      | Some a -> Fact.return a
      | None -> Fact.failf "unknown/unsupported architecture" ()


    let arch_width =
      arch >>| fun arch -> Arch.addr_size arch |> Size.in_bits

    let relocations =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(select (from relocation)) >>= fun s ->
      Fact.return
        (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
             Addr.of_int64 ~width addr, Addr.of_int64 ~width data))

    let external_symbols  =
      arch_width >>= fun width ->
      Fact.collect Ogre.Query.(select (from external_reference)) >>| fun s ->
      Seq.fold s ~init:Addr.Map.empty ~f:(fun addrs (addr, name) ->
          Map.set addrs
            ~key:(Addr.of_int64 ~width addr)
            ~data:name)
  end

  let relocations = Fact.eval Request.relocations
  let external_symbols = Fact.eval Request.external_symbols
  let empty = {rels = Addr.Map.empty; exts = Addr.Map.empty}

  let of_spec spec =
    match relocations spec, external_symbols spec with
    | Ok rels, Ok exts -> {rels; exts}
    | Error e, _  | _, Error e -> Error.raise e

  let span mem =
    let start = Memory.min_addr mem in
    let len = Memory.length mem in
    Seq.init len ~f:(Addr.nsucc start)

  let find_external {exts} mem =
    Seq.find_map ~f:(Map.find exts) (span mem)

  let find_internal {rels} mem =
    Seq.find_map ~f:(Map.find rels) (span mem)

  let subscribe () =
    let open Future.Syntax in
    Stream.hd Project.Info.spec >>|
    of_spec


  let override_internal dst =
    Stmt.map (object inherit Stmt.mapper
      method! map_jmp _ = [Bil.Jmp (Int dst)]
    end)

  let override_external name =
    Stmt.map (object inherit Stmt.mapper
      method! map_jmp _ = [Call.extern name]
    end)


  let fixup info mem bil =
    match Future.peek info with
    | None -> bil
    | Some info ->
      match find_internal info mem with
      | Some dst ->
        override_internal dst bil
      | None ->
        match find_external info mem with
        | Some name ->
          override_external name bil
        | None -> bil

end

module Brancher = struct
  include Theory.Empty

  let pack kind dsts =
    KB.Value.put Insn.Slot.dests (Theory.Effect.empty kind) dsts

  let get x = KB.Value.get Insn.Slot.dests x

  let union k e1 e2 =
    pack k @@ match get e1, get e2 with
    | None,s|s,None -> s
    | Some e1, Some e2 -> Some (Set.union e1 e2)

  let ret kind dst =
    let dsts = Set.singleton (module Theory.Label) dst in
    KB.return @@ pack kind (Some dsts)

  let goto dst = ret Theory.Effect.Sort.jump dst

  let jmp _ =
    KB.Object.create Theory.Program.cls >>= fun dst ->
    ret Theory.Effect.Sort.jump dst

  let seq x y =
    x >>= fun x ->
    y >>= fun y ->
    let k = Theory.Effect.sort x in
    KB.return (union k x y)

  let blk _ data ctrl =
    data >>= fun data ->
    ctrl >>= fun ctrl ->
    let k = Theory.Effect.Sort.join
        [Theory.Effect.sort data]
        [Theory.Effect.sort ctrl] in
    KB.return (union k data ctrl)

  let branch _cnd yes nay =
    yes >>= fun yes ->
    nay >>= fun nay ->
    let k = Theory.Effect.sort yes in
    KB.return (union k yes nay)
end

let base_context = [
  "bil-lifter";
]

let provide_lifter ~with_fp () =
  info "providing a lifter for all BIL lifters";
  let relocations = Relocations.subscribe () in
  let unknown = Theory.Program.Semantics.empty in
  let context arch =
    sprintf "arch-%a" Arch.str arch ::
    if with_fp
    then "floating-point" :: base_context
    else base_context in
  let (>>?) x f = x >>= function
    | None -> KB.return unknown
    | Some x -> f x in
  let lifter obj =
    Knowledge.collect Arch.slot obj >>= fun arch ->
    Theory.instance ~context:(context arch) () >>=
    Theory.require >>= fun (module Core) ->
    Knowledge.collect Memory.slot obj >>? fun mem ->
    Knowledge.collect Disasm_expert.Basic.Insn.slot obj >>? fun insn ->
    let module Target = (val target_of_arch arch) in
    match Target.lift mem insn with
    | Error _ ->
      Knowledge.return (Insn.of_basic insn)
    | Ok bil ->
      Bil_semantics.context >>= fun ctxt ->
      Knowledge.provide Bil_semantics.arch ctxt arch >>= fun () ->
      let module Lifter = Theory.Parser.Make(Core) in
      Optimizer.run BilParser.t bil >>= fun sema ->
      let bil = Insn.bil sema in
      let bil = Relocations.fixup relocations mem bil in
      Lifter.run BilParser.t bil >>| fun sema ->
      let bil = Insn.bil sema in
      KB.Value.merge ~on_conflict:`drop_left
        sema (Insn.of_basic ~bil insn) in
  Knowledge.promise Theory.Program.Semantics.slot lifter


let init ~with_fp () =
  provide_lifter ~with_fp ();
  provide_bir ();
  Theory.declare !!(module Brancher : Theory.Core)
    ~package:"bap.std" ~name:"jump-dests"
    ~desc:"an approximation of jump destinations"
    ~provides:[
      "brancher";
      "branch-destinations"
    ]
