open Core_kernel.Std
open Bap_types.Std
open Or_error

module Kind = Bap_insn_kind
module Mem = Bap_memory
module C = Bap_disasm_prim

type empty
type asm
type kinds

type mem = Mem.t with sexp_of
type kind = Kind.t with compare, sexp

type pred = [
  | `valid
  |  kind
] with sexp,compare

type 'a oper = {
  oper : int;
  insn : int;
  data : 'a;
} with bin_io, compare, sexp


type reg_info = {
  reg_code : int;
  reg_name : string Lazy.t;
} with bin_io, sexp

let compare_reg_info {reg_code=x} {reg_code=y} = compare x y

type imm_info = {
  imm_small : int;
  imm_large : int64 sexp_option;
} with bin_io, compare, sexp

type reg = reg_info oper with bin_io, compare, sexp
type imm = imm_info oper with bin_io, compare, sexp
type fmm = float    oper with bin_io, compare, sexp


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
            let dst = String.create len in
            Bigstring.To_string.blit
              ~src:t.data ~src_pos:pos ~dst ~dst_pos:0 ~len;
            dst)
end


type dis = {
  id : int;
  insn_table : Table.t;
  reg_table  : Table.t;
  asm : bool;
  kinds : bool;
}

module Reg = struct

  let create dis ~insn ~oper : reg =
    let data =
      let reg_code = C.insn_op_reg_code dis.id ~insn ~oper in
      let reg_name =
        if reg_code = 0 then lazy "nil"
        else
          let off = C.insn_op_reg_name dis.id ~insn ~oper in
          lazy (Table.lookup dis.reg_table off) in
      {reg_code; reg_name} in
    {insn; oper; data}

  let code op : int = op.data.reg_code
  let name {data = {reg_name = lazy x}} : string = x

  module T = struct
    type t = reg
    with bin_io, sexp, compare

    let module_name = "Bap_disasm_basic.Reg"

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
      let imm_small = C.insn_op_imm_small_value dis.id ~insn ~oper in
      let imm_large = if fits imm_small then None else
          Some (C.insn_op_imm_value dis.id ~insn ~oper) in
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
    match Word.bitsub ~hi:(width-1) (Word.of_int64 n) with
    | Ok word -> Some word
    | Error _ -> None


  module T = struct
    type t = imm
    with bin_io, sexp, compare
    let module_name = "Bap_disasm_basic.Imm"
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
    data = C.insn_op_fmm_value dis.id ~insn ~oper
  }
  let to_float x = x.data

  module T = struct
    type t = fmm
    with bin_io, sexp, compare

    let module_name = "Bap_disasm_basic.Fmm"
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
    with bin_io, compare, sexp

    let pp fmt = function
      | Reg reg -> Format.fprintf fmt "%a" Reg.pp reg
      | Imm imm -> Format.fprintf fmt "%a" Imm.pp imm
      | Fmm fmm -> Format.fprintf fmt "%a" Fmm.pp fmm

    let module_name = "Bap_disasm_basic.Op"

    let hash = function
      | Reg r -> Reg.hash r
      | Imm n -> Imm.hash n
      | Fmm n -> Fmm.hash n
  end
  include T
  include Regular.Make(T)

end

type op = Op.t
with bin_io, compare, sexp



let cpred_of_pred : pred -> C.pred = function
  | `valid -> C.Is_true
  | `conditional_branch -> C.Is_conditional_branch
  | `unconditional_branch -> C.Is_unconditional_branch
  | `indirect_branch -> C.Is_indirect_branch
  | `return -> C.Is_return
  | `call -> C.Is_call
  | `barrier -> C.Is_barrier
  | `terminator -> C.Is_terminator
  | `may_affect_control_flow -> C.May_affect_control_flow
  | `may_store | `may_load -> C.Is_true (* TODO add *)

module Insn = struct
  type ins_info = {
    code : int;
    name : string Lazy.t;
    asm  : string;
    kinds: kind list;
    opers: Op.t array;
  }
  type ('a,'k) t = ins_info

  let sexp_of_t ins =
    let lazy name = ins.name in
    let ops = Array.to_list ins.opers in
    Sexp.List (Sexp.Atom name :: List.map ops ~f:(fun op ->
        Sexp.Atom (Op.to_string op)))

  let compare {code=x} {code=y} = compare x y

  let name {name = lazy x} = x
  let code op = op.code
  let asm  x = x.asm
  let ops  x = x.opers
  let kinds x = x.kinds
  let is op x =
    let equal x y = Kind.compare x y = 0 in
    List.mem ~equal op.kinds x


  let create ~asm ~kinds dis ~insn =
    let code = C.insn_code dis.id ~insn in
    let name =
      let off = C.insn_name dis.id ~insn in
      lazy (Table.lookup dis.insn_table off) in
    let asm =
      if asm then
        let data = String.create (C.insn_asm_size dis.id ~insn) in
        C.insn_asm_copy dis.id ~insn data;
        data
      else "" in
    let kinds =
      if kinds then
        List.filter_map Kind.all ~f:(fun k ->
            let p = cpred_of_pred (k :> pred) in
            if C.is_supported dis.id p
            then Option.some_if (C.insn_satisfies dis.id ~insn p) k
            else None)
      else [] in
    let opers =
      Array.init (C.insn_ops_size dis.id ~insn) ~f:(fun oper ->
          match C.insn_op_type dis.id ~insn ~oper with
          | C.Reg -> Op.Reg Reg.(create dis ~insn ~oper)
          | C.Imm -> Op.Imm Imm.(create dis ~insn ~oper)
          | C.Fmm -> Op.Fmm Fmm.(create dis ~insn ~oper)
          | C.Insn -> assert false) in
    {code; name; asm; kinds; opers }

end

type ('a,'k) insn = ('a,'k) Insn.t

let compare_insn (i1 : ('a,'b) insn) (i2 : ('a,'b) insn) =
  Insn.compare i1 i2
let sexp_of_insn : ('a,'b) insn -> Sexp.t = Insn.sexp_of_t

type (+'a,+'k) insns = (mem * ('a,'k) insn option) list

module Pred = struct
  include Comparable.Make(struct
      type t = pred with compare, sexp
    end)
end

module Preds = Pred.Set
type preds = Preds.t with compare, sexp

type step = {
  mem : mem;
  off : int;
  preds : preds;
} with sexp_of

type ('a,'k) maybe_insn = mem * ('a,'k) insn option

let sexp_of_maybe_ins (_,insn) =
  <:sexp_of<insn option>> insn


type (+'a,+'k,'s,'r) state = {
  dis : dis sexp_opaque;
  current : step;
  history : step list;
  insns : ('a,'k) maybe_insn Lazy.t array sexp_opaque;
  return : ('s -> 'r) sexp_opaque;
  stopped : (('a,'k,'s,'r) state -> 's -> 'r) option sexp_opaque;
  invalid : (('a,'k,'s,'r) state -> mem -> 's -> 'r) option sexp_opaque;
  hit : (('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r)
      option sexp_opaque;
} with sexp_of

let create_state ?(stop_on=[]) ?stopped ?invalid ?hit dis mem ~return  = {
  dis; return; hit;
  current = {mem; off=0; preds = Preds.of_list stop_on};
  stopped;
  invalid;
  history = [];
  insns = [| |] ;
}

let insn_mem s ~insn : mem =
  let off = C.insn_offset s.dis.id ~insn in
  let words = C.insn_size s.dis.id ~insn in
  let from = Addr.(Mem.min_addr s.current.mem ++ off) in
  ok_exn (Mem.view s.current.mem ~from ~words)


let kinds s ~insn : kind list = []

let set_memory dis p : unit =
  let open Bigsubstring in
  let buf = Mem.to_buffer p.mem in
  let addr = Mem.min_addr p.mem in
  let addr = ok_exn (Addr.to_int64 addr) in
  C.set_memory dis.id addr (base buf) ~off:(pos buf) ~len:(length buf)

let update_state s current = {
  s with
  history = s.current :: s.history;
  current;
}

let memory s = s.current.mem

let with_preds s (ps : pred list) =
  let add p =
    let p = cpred_of_pred p in
    if C.is_supported s.dis.id p then
      C.predicates_push s.dis.id p in
  let ps = Preds.of_list ps in
  let drop = Preds.diff s.current.preds ps in
  if not(Preds.is_empty drop) then
    Preds.iter (Preds.diff ps s.current.preds) ~f:add
  else begin
    C.predicates_clear s.dis.id;
    Preds.iter ps ~f:(add);
  end;
  {s with current = {s.current with preds = ps}}

let insns s =
  List.init Array.(length s.insns) ~f:(fun i -> Lazy.force s.insns.(i))

let last s n =
  let m = Array.length s.insns in
  let n = min n m in
  List.init n ~f:(fun i -> Lazy.force s.insns.(m - i - 1))

let preds s = Preds.to_list s.current.preds

let addr s = Addr.(Mem.min_addr s.current.mem ++ s.current.off)

let step s data =
  C.insns_clear s.dis.id;
  let rec loop s data =
    C.run s.dis.id;
    let off = C.offset s.dis.id in
    let s = update_state s {s.current with off} in
    let n = C.insns_size s.dis.id in
    assert (n > 0);
    let insn = n - 1 in
    let stop = C.insn_size s.dis.id ~insn = 0 in
    let n = if stop then max 0 (n - 1) else n in
    let {asm; kinds} = s.dis in
    let insns = Array.init n ~f:(fun insn -> lazy begin
        let is_valid =
          not(C.insn_satisfies s.dis.id ~insn C.Is_invalid) in
        insn_mem s ~insn,
        Option.some_if is_valid
          (Insn.create ~asm ~kinds s.dis ~insn)
      end) in
    let s = {s with insns} in
    if stop then match s.stopped with
      | Some f -> f s data
      | None -> s.return data
    else if C.insn_satisfies s.dis.id ~insn C.Is_invalid
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

let create ?(debug_level=0) ?(cpu="") ~backend triple =
  let id = match C.create ~backend ~triple ~cpu ~debug_level with
    | n when n >= 0 -> Ok n
    | -2 -> errorf "Unknown backend: %s" backend
    | -3 -> errorf "Unsupported target: %s %s" triple cpu
    |  n -> errorf "Disasm.Basic: Unknown error %d" n in
  id >>= fun id -> return {
    id;
    insn_table = Table.create (C.insn_table id);
    reg_table = Table.create (C.reg_table id);
    asm = false;
    kinds = false;
  }

type ('a,'k) t = dis

let run ?(stop_on=[]) ?invalid ?stopped ?hit dis ~return ~init mem =
  let state =
    create_state  ?invalid ?stopped ?hit ~return
      dis mem in
  let state = with_preds state stop_on in
  jump state (memory state) init

let drop_kinds d =
  C.store_predicates d.id false;
  {d with kinds = false}

let drop_asm d =
  C.store_asm_string d.id false;
  {d with asm = false}

let store_kinds d =
  C.store_predicates d.id true;
  {d with kinds = true}


let store_asm d =
  C.store_asm_string d.id true;
  {d with asm = true}

let insn_of_mem dis mem =
  let init = mem, None, `left mem in
  let split mem' =
    Mem.view mem ~from:Addr.(Mem.max_addr mem' ++ 1) in
  run ~stop_on:[ `valid ] dis mem ~return ~init
    ~hit:(fun s mem insn _ ->
        split mem >>= fun r -> stop s (mem, Some insn, `left r))
    ~invalid:(fun s mem _ ->
        split mem >>= fun r -> stop s (mem, None,      `left r))

(* TEST_MODULE = struct *)
(*   (\* bap_disasm_insn_ops_size *\) *)
(*   let data = String.concat [ *)
(*       "\x48\x83\xec\x08";         (\* sub $0x8,%rsp       *\) *)
(*       "\xe8\x47\xee\xff\xff";     (\* callq 942040        *\) *)
(*       "\x8b\x40\x10";             (\* mov 0x10(%rax),%eax *\) *)
(*       "\x48\x83\xc4\x08";         (\* add $0x8, %rsp *\) *)
(*       "\xc3";                     (\* retq *\) *)
(*     ] *)

(*   let disasm = [ *)
(*     ["SUB64ri8"; "RSP"; "RSP"; "8"]; *)
(*     ["CALL64pcrel32"; "-4537"]; *)
(*     ["MOV32rm"; "EAX"; "RAX"; "1"; "nil"; "16"; "nil"]; *)
(*     ["ADD64ri8"; "RSP"; "RSP"; "8"]; *)
(*     ["RET"] *)
(*   ] *)

(*   let string_of_disasm d = *)
(*     let string_of_insn insn = *)
(*       "\t" ^ Sexp.to_string_hum (<:sexp_of<(string list)>> insn) in *)
(*     List.map d ~f:string_of_insn |> String.concat ~sep:"\n" *)


(*   let memory addr s = *)
(*     Mem.create LittleEndian Addr.(of_int64 addr) @@ *)
(*     Bigstring.of_string s |> ok_exn *)

(*   let hit state mem insn disasm = *)
(*     let ops = Insn.ops insn |> *)
(*               Array.map ~f:(Op.to_string) |> *)
(*               Array.to_list in *)
(*     step state ((Insn.name insn :: ops) :: disasm) *)

(*   let get_all state = *)
(*     List.map ~f:snd (insns state) |> *)
(*     List.filter_opt |> *)
(*     List.map ~f:(fun insn -> Insn.name insn :: *)
(*                              (Insn.ops insn |> *)
(*                               Array.map ~f:Op.to_string |> *)
(*                               Array.to_list)) *)

(*   let stopped s = function *)
(*     | [] -> stop s (get_all s) *)
(*     | xs -> stop s (List.rev xs) *)

(*   let invalid state disasm = assert false *)


(*   let state, mem = *)
(*     let mem = memory 0x9431f0L data in *)
(*     let dis = ok_exn (create ~backend:"llvm" "x86_64") in *)
(*     create_state dis mem ~return:ident ~invalid ~stopped ~hit, mem *)

(*   TEST = *)
(*     let r = jump state mem [] in *)
(*     printf "Expect:\n%s\n" (string_of_disasm disasm); *)
(*     printf "Return:\n%s\n" (string_of_disasm r); *)
(*     r = disasm *)
(* end *)
