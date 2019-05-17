open Core_kernel
open Regular.Std
open Bap_types.Std
open Bap_core_theory
open Or_error

module Kind = Bap_insn_kind
module Mem = Bap_memory
module C = Bap_disasm_prim

type empty
type asm
type kinds

type mem = Mem.t [@@deriving sexp_of]
type kind = Kind.t [@@deriving compare, sexp]

type pred = [
  | `Valid
  |  Kind.t
] [@@deriving sexp, compare]

type 'a oper = {
  oper : int;
  insn : int;
  data : 'a;
} [@@deriving bin_io, compare, sexp]

type reg_info = {
  reg_code : int;
  reg_name : string;
} [@@deriving bin_io, sexp]

let compare_reg_info {reg_code=x} {reg_code=y} = Int.compare x y

type imm_info = {
  imm_small : int;
  imm_large : int64 sexp_option;
} [@@deriving bin_io, compare, sexp]

type reg = reg_info oper [@@deriving bin_io, compare, sexp]
type imm = imm_info oper [@@deriving bin_io, compare, sexp]
type fmm = float    oper [@@deriving bin_io, compare, sexp]

module Table = struct
  (* Bigstring.length is very slow... we should report a bug to the
     mantis. They need to add "noalloc" to it, otherwise on each call
     the whole GC machinery is triggered. For now we will store the
     size, so that later we can check it for free. *)
  type t = {
    data : Bigstring.t;
    size : int;
    cache : string Int.Table.t;
  }

  let create data = {
    data;
    size = Bigstring.length data;
    cache = Int.Table.create ();
  }

  let lookup t pos =
    Int.Table.find_or_add t.cache pos ~default:(fun () ->
        if pos < 0 || pos >= t.size then "ERROR"
        else match Bigstring.find ~pos '\x00' t.data with
          | None -> "ERROR"
          | Some fin ->
            let len = fin - pos in
            let dst = Bytes.create len in
            Bigstring.To_bytes.blit
              ~src:t.data ~src_pos:pos ~dst ~dst_pos:0 ~len;
            Bytes.to_string dst)
end

type disassembler = {
  dd : int;
  insn_table : Table.t;
  reg_table  : Table.t;
  mutable users : int;
}

let last_id = ref 0
let disassemblers = Hashtbl.create (module String)



type dis = {
  name : string;
  asm : bool;
  kinds : bool;
}

let get {name} = match Hashtbl.find disassemblers name with
  | None ->
    failwith "Trying to access a closed disassembler"
  | Some d -> d

let (!!) h = (get h).dd

module Reg = struct

  let create dis ~insn ~oper : reg =
    let data =
      let reg_code = C.insn_op_reg_code !!dis ~insn ~oper in
      let reg_name =
        if reg_code = 0 then "Nil"
        else
          let off = C.insn_op_reg_name !!dis ~insn ~oper in
          (Table.lookup (get dis).reg_table off) in
      {reg_code; reg_name} in
    {insn; oper; data}

  let code op : int = op.data.reg_code
  let name {data = {reg_name = x}} : string = x

  module T = struct
    type t = reg
    [@@deriving bin_io, sexp, compare]

    let module_name = Some "Bap.Std.Reg"
    let version = "1.0.0"

    let pp fmt t =
      Format.fprintf fmt "%s" @@ name t

    let hash = code
  end
  include T
  include Regular.Make(T)
end

module Imm = struct

  let fits x =
    not(x = Int.max_value || x = Int.min_value)

  let create dis ~insn ~oper =
    let data =
      let imm_small = C.insn_op_imm_small_value !!dis ~insn ~oper in
      let imm_large = if fits imm_small then None else
          Some (C.insn_op_imm_value !!dis ~insn ~oper) in
      {imm_small; imm_large} in
    {insn; oper; data}

  let to_int {data=n} : int option =
    if n.imm_large = None then Some n.imm_small else None

  let to_int64 {data = n} =
    match n.imm_large with
    | None -> Int64.of_int n.imm_small
    | Some x -> x

  let to_word t ~width =
    let n = to_int64 t in
    match Word.extract ~hi:(width-1) (Word.of_int64 n) with
    | Ok word -> Some word
    | Error _ -> None

  module T = struct
    type t = imm
    [@@deriving bin_io, sexp, compare]
    let module_name = Some "Bap.Std.Imm"
    let version = "1.0.0"
    let pp fmt t =
      let x = to_int64 t in
      if Int64.is_negative x then
        Format.fprintf fmt "-0x%Lx" (Int64.abs x)
      else
        Format.fprintf fmt "0x%Lx" x

    let hash {data = n} =
      if fits n.imm_small
      then n.imm_small
      else Int64.hash (uw n.imm_large)

  end
  include T
  include Regular.Make(T)
end

module Fmm = struct

  let create dis ~insn ~oper = {
    insn; oper;
    data = C.insn_op_fmm_value !!dis ~insn ~oper
  }
  let to_float x = x.data

  module T = struct
    type t = fmm
    [@@deriving bin_io, sexp, compare]

    let module_name = Some "Bap.Std.Fmm"
    let version = "1.0.0"
    let hash t = Float.hash (to_float t)
    let pp fmt t =
      Format.fprintf fmt "%a" Float.pp (to_float t)
  end
  include T
  include Regular.Make(T)
end

module Op = struct
  module T = struct
    type t =
      | Reg of reg
      | Imm of imm
      | Fmm of fmm
    [@@deriving bin_io, compare, sexp]

    let pr fmt = Format.fprintf fmt
    let pp fmt = function
      | Reg reg -> pr fmt "%a" Reg.pp reg
      | Imm imm -> pr fmt "%a" Imm.pp imm
      | Fmm fmm -> pr fmt "%a" Fmm.pp fmm

    let pp_adt ch = function
      | Imm imm -> pr ch "Imm(0x%Lx)" (Imm.to_int64 imm)
      | Fmm fmm -> pr ch "Fmm(%g)" (Fmm.to_float fmm)
      | Reg reg -> pr ch "Reg(\"%a\")" Reg.pp reg


    let module_name = Some "Bap.Std.Op"
    let version = "1.0.0"

    let hash = function
      | Reg r -> Reg.hash r
      | Imm n -> Imm.hash n
      | Fmm n -> Fmm.hash n

    (** Normalized comparison *)
    module Normalized = struct
      (** immediates are all equal  *)
      let compare x y = match x, y with
        | Imm _, Imm _ -> 0
        | Fmm _, Fmm _ -> 0
        | _ -> compare x y

      (** compares equal if one of arrays is a prefix of another, e.g,
          [| x; y; z |] is equal to [| x; y |] and vice verse *)
      let compare_ops xs ys =
        let len = min (Array.length xs) (Array.length ys) in
        let rec loop = function
          | 0 -> 0
          | n ->
            let r = compare xs.(len - n) ys.(len - n) in
            if r = 0 then loop (n-1) else r in
        loop len

      let hash = function
        | Imm _ | Fmm _ -> 0
        | Reg x -> Reg.hash x
    end
  end
  include T
  include Regular.Make(T)
end

type op = Op.t
[@@deriving bin_io, compare, sexp]

let cpred_of_pred : pred -> C.pred = function
  | `Valid -> C.Is_true
  | `Conditional_branch -> C.Is_conditional_branch
  | `Unconditional_branch -> C.Is_unconditional_branch
  | `Indirect_branch -> C.Is_indirect_branch
  | `Return -> C.Is_return
  | `Call -> C.Is_call
  | `Barrier -> C.Is_barrier
  | `Terminator -> C.Is_terminator
  | `May_affect_control_flow -> C.May_affect_control_flow
  | `May_store  -> C.May_store
  | `May_load -> C.May_load

module Insn = struct
  type ins_info = {
    code : int;
    name : string;
    asm  : string;
    kinds: kind list;
    opers: Op.t array;
  }
  type ('a,'k) t = ins_info

  let sexp_of_t ins =
    let name = ins.name in
    let ops = Array.to_list ins.opers in
    Sexp.List (Sexp.Atom name :: List.map ops ~f:(fun op ->
        Sexp.Atom (Op.to_string op)))

  let compare {code=x} {code=y} = Int.compare x y

  let name {name = x} = x
  let code op = op.code
  let asm  x = x.asm
  let ops  x = x.opers
  let kinds x = x.kinds
  let is op x =
    let equal x y = Kind.compare x y = 0 in
    List.mem ~equal op.kinds x


  let create ~asm ~kinds dis ~insn =
    let code = C.insn_code !!dis ~insn in
    let name =
      let off = C.insn_name !!dis ~insn in
      Table.lookup (get dis).insn_table off in
    let asm =
      if asm then
        let data = Bytes.create (C.insn_asm_size !!dis ~insn) in
        C.insn_asm_copy !!dis ~insn data;
        Bytes.to_string data
      else "" in
    let kinds =
      if kinds then
        List.filter_map Kind.all ~f:(fun k ->
            let p = cpred_of_pred (k :> pred) in
            if C.is_supported !!dis p
            then Option.some_if (C.insn_satisfies !!dis ~insn p) k
            else None)
      else [] in
    let opers =
      Array.init (C.insn_ops_size !!dis ~insn) ~f:(fun oper ->
          match C.insn_op_type !!dis ~insn ~oper with
          | C.Reg -> Op.Reg Reg.(create dis ~insn ~oper)
          | C.Imm -> Op.Imm Imm.(create dis ~insn ~oper)
          | C.Fmm -> Op.Fmm Fmm.(create dis ~insn ~oper)
          | C.Insn -> assert false) in
    {code; name; asm; kinds; opers }


  let domain =
    KB.Domain.optional ~inspect:sexp_of_t "insn"
      ~equal:(fun x y -> Int.equal x.code y.code)
  let slot = KB.Class.property ~package:"bap.std"
      Theory.Program.cls "insn" domain
end

type ('a,'k) insn = ('a,'k) Insn.t

let compare_insn (i1 : ('a,'b) insn) (i2 : ('a,'b) insn) =
  Insn.compare i1 i2
let sexp_of_insn : ('a,'b) insn -> Sexp.t = Insn.sexp_of_t


type full_insn = (asm,kinds) insn

let sexp_of_full_insn = sexp_of_insn
let compare_full_insn i1 i2 =
  let open Insn in
  let r1 = Int.compare i1.code i2.code in
  if r1 <> 0 then String.compare i1.asm i2.asm
  else r1



type (+'a,+'k) insns = (mem * ('a,'k) insn option) list

module Pred = Comparable.Make(struct
    type t = pred [@@deriving compare, sexp]
  end)

module Preds = Pred.Set
type preds = Preds.t [@@deriving compare, sexp]

type step = {
  mem : mem;
  off : int;
  preds : preds;
} [@@deriving sexp_of]

type ('a,'k) maybe_insn = mem * ('a,'k) insn option

let sexp_of_maybe_ins (_,insn) =
  [%sexp_of:insn option] insn


type (+'a,+'k,'s,'r) state = {
  backlog : int;
  dis : dis sexp_opaque;
  current : step;
  history : step list;
  insns : ('a,'k) maybe_insn array sexp_opaque;
  return : ('s -> 'r) sexp_opaque;
  stopped : (('a,'k,'s,'r) state -> 's -> 'r) option sexp_opaque;
  invalid : (('a,'k,'s,'r) state -> mem -> 's -> 'r) option sexp_opaque;
  hit : (('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r)
      option sexp_opaque;
} [@@deriving sexp_of]

let create_state ?(backlog=8) ?(stop_on=[]) ?stopped ?invalid ?hit dis
    mem ~return  = {
  backlog;
  dis; return; hit;
  current = {mem; off=0; preds = Preds.of_list stop_on};
  stopped;
  invalid;
  history = [];
  insns = [| |] ;
}

let insn_mem s ~insn : mem =
  let off = C.insn_offset !!(s.dis) ~insn in
  let words = C.insn_size !!(s.dis) ~insn in
  let from = Addr.(Mem.min_addr s.current.mem ++ off) in
  ok_exn (Mem.view s.current.mem ~from ~words)


let kinds s ~insn:_ : kind list = []

let set_memory dis p : unit =
  let open Bigsubstring in
  let buf = Mem.to_buffer p.mem in
  let addr = Mem.min_addr p.mem in
  let addr = ok_exn (Addr.to_int64 addr) in
  C.set_memory !!dis addr (base buf) ~off:(pos buf) ~len:(length buf)

let update_state s current = {
  s with
  history = List.take (s.current :: s.history) s.backlog;
  current;
}

let memory s = s.current.mem

let with_preds s (ps : pred list) =
  let add p =
    let p = cpred_of_pred p in
    if C.is_supported !!(s.dis) p then
      C.predicates_push !!(s.dis) p in
  let ps = Preds.of_list ps in
  let drop = Preds.diff s.current.preds ps in
  if not(Preds.is_empty drop) then
    Preds.iter (Preds.diff ps s.current.preds) ~f:add
  else begin
    C.predicates_clear !!(s.dis);
    Preds.iter ps ~f:(add);
    C.predicates_push !!(s.dis) C.Is_invalid;
  end;
  {s with current = {s.current with preds = ps}}

let insns s =
  List.init Array.(length s.insns) ~f:(fun i -> s.insns.(i))

let last s n =
  let m = Array.length s.insns in
  let n = min n m in
  List.init n ~f:(fun i -> s.insns.(m - i - 1))

let preds s = Preds.to_list s.current.preds

let addr s = Addr.(Mem.min_addr s.current.mem ++ s.current.off)

let step s data =
  C.insns_clear !!(s.dis);
  let rec loop s data =
    C.run !!(s.dis);
    let off = C.offset !!(s.dis) in
    let s = update_state s {s.current with off} in
    let n = C.insns_size !!(s.dis) in
    assert (n > 0);
    let insn = n - 1 in
    let stop = C.insn_size !!(s.dis) ~insn = 0 in
    let n = if stop then max 0 (n - 1) else n in
    let {asm; kinds} = s.dis in
    let insns = Array.init n ~f:(fun insn -> begin
          let is_valid =
            not(C.insn_satisfies !!(s.dis) ~insn C.Is_invalid) in
          insn_mem s ~insn,
          Option.some_if is_valid
            (Insn.create ~asm ~kinds s.dis ~insn)
        end) in
    let s = {s with insns} in
    if stop then match s.stopped with
      | Some f -> f s data
      | None -> s.return data
    else if C.insn_satisfies !!(s.dis) ~insn C.Is_invalid
    then match s.invalid with
      | Some f -> f s (insn_mem s ~insn) data
      | None -> loop s data
    else match s.hit with
      | Some f -> f s
                    (insn_mem s ~insn)
                    (Insn.create ~asm:true ~kinds:true s.dis ~insn)
                    data
      | None -> s.return data in
  loop s data

let jump s mem data : 'r =
  let current = { s.current with mem } in
  let s = { s with current } in
  set_memory s.dis s.current;
  step s data

let stop s data = s.return data

let back s data =
  let current,history = match s.history with
    | [] -> s.current, s.history
    | x :: xs -> x,xs in
  step { s with current; history} data

let create ?(debug_level=0) ?(cpu="") ?(backend="llvm") triple =
  let name = sprintf "%s:%s%s" backend triple cpu in
  match Hashtbl.find disassemblers name with
  | Some d ->
    d.users <- d.users + 1;
    Ok {name; asm=false; kinds=false}
  | None ->
    let dd = match C.create ~backend ~triple ~cpu ~debug_level with
      | n when n >= 0 -> Ok n
      | -2 -> errorf "Unknown backend: %s" backend
      | -3 -> errorf "Unsupported target: %s %s" triple cpu
      |  n -> errorf "Disasm.Basic: Unknown error %d" n in
    dd >>= fun dd ->
    let disassembler = {
      dd;
      insn_table = Table.create (C.insn_table dd);
      reg_table = Table.create (C.reg_table dd);
      users = 1;
    } in
    Hashtbl.add_exn disassemblers name disassembler;
    Ok {name; asm = false; kinds = false}


let close dis =
  let disassembler = get dis in
  disassembler.users <- disassembler.users - 1;
  if disassembler.users = 0
  then begin
    Hashtbl.remove disassemblers dis.name;
    C.delete disassembler.dd;
  end


let with_disasm ?debug_level ?cpu ?backend triple ~f =
  create ?debug_level ?cpu ?backend triple >>= fun dis ->
  f dis >>| fun res -> close dis; res

type ('a,'k) t = dis

let run ?backlog ?(stop_on=[]) ?invalid ?stopped ?hit dis ~return ~init mem =
  let state =
    create_state ?backlog ?invalid ?stopped ?hit ~return
      dis mem in
  let state = with_preds state stop_on in
  C.store_asm_string !!dis dis.asm;
  C.store_predicates !!dis dis.kinds;
  jump state (memory state) init

let store_kinds d =
  {d with kinds = true}

let store_asm d =
  {d with asm = true}

let insn_of_mem dis mem =
  let init = mem,None,`left mem in
  let split mem' =
    if Mem.(max_addr mem' = max_addr mem) then Ok `finished
    else Mem.view mem ~from:Addr.(Mem.max_addr mem' ++ 1)
      >>| fun r -> `left r in
  run ~stop_on:[`Valid] dis mem ~return ~init
    ~hit:(fun s mem' insn _ ->
        split mem' >>= fun r -> stop s (mem',Some insn,r))
    ~invalid:(fun s mem' _ ->
        split mem' >>= fun r -> stop s (mem',None,r))


let available_backends () =
  C.backends_size () |> List.init ~f:C.backend_name

module Trie = struct

  type s = State : (_,_,_,_) state -> s
  type key = int * s

  let key_of_first_insns s ~len:n =
    Option.some_if (Array.length s.insns <= n) (n, State s)

  module Key = struct
    type t = key
    type token = int * Op.t array [@@deriving bin_io, compare, sexp]
    let length = fst
    let nth_token (_, State s) i =
      match s.insns.(i) with
      | (_, None) -> 0, [| |]
      | (_, Some insn) -> Insn.(insn.code, insn.opers)

    let token_hash = Hashtbl.hash
  end

  module Normalized_key = struct
    include Key

    let compare_token (x,xs) (y,ys) =
      let r = compare_int x y in
      if r <> 0 then r
      else Op.Normalized.compare_ops xs ys

    let ops_hash xs =
      Array.fold ~init:0 ~f:(fun h x -> Op.Normalized.hash x lxor h) xs

    let token_hash (x,xs) =
      x lxor ops_hash xs
  end

  module Normalized = Trie.Make(Normalized_key)
  include (Trie.Make(Key) : Trie with type key := key)
end
