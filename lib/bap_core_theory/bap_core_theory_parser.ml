open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory_definition
open Bap_core_theory_sort

module Value = Bap_core_theory_value
module Grammar = Bap_core_theory_grammar_definition
module Link = Bap_core_theory_link

open Knowledge.Syntax
open Link.Syntax

type ('a,'e) bitv_parser =
  (module Grammar.Bitv with type t = 'a
                        and type exp = 'e) ->
  'e -> 'a

type ('a,'e) bool_parser =
  (module Grammar.Bool with type t = 'a
                        and type exp = 'e) ->
  'e -> 'a

type ('a,'e) mem_parser =
  (module Grammar.Mem with type t = 'a
                       and type exp = 'e) ->
  'e -> 'a

type ('a,'e,'s) stmt_parser =
  (module Grammar.Stmt with type t = 'a
                        and type exp = 'e
                        and type stmt = 's) ->
  's -> 'a

type ('a,'e) float_parser =
  (module Grammar.Float with type t = 'a
                         and type exp = 'e) ->
  'e -> 'a

type ('a,'e) rmode_parser =
  (module Grammar.Rmode with type t = 'a
                         and type exp = 'e) ->
  'e -> 'a

type ('e,'r,'s) t = {
  bitv : 'a. ('a,'e) bitv_parser;
  bool : 'a. ('a,'e) bool_parser;
  mem  : 'a. ('a,'e) mem_parser;
  stmt : 'a. ('a,'e,'s) stmt_parser;
  float : 'a . ('a,'e) float_parser;
  rmode : 'a . ('a,'r) rmode_parser;
}

type ('e,'r,'s) parser = ('e,'r,'s) t

let bits = Bits.define
let sort x = x >>| Value.sort
let bool = Bool.t

module Make(S : Core) = struct
  open S
  open Knowledge.Syntax

  type 'a t = 'a knowledge

  let of_word w = int (bits (Word.bitwidth w)) w
  let of_int s x = int s (Word.of_int x ~width:(Bits.size s))
  let join s1 s2 = bits (Bits.size s1 + Bits.size s2)
  let is_big e = if e = BigEndian then b1 else b0


  let rename ctxt v =
    match List.Assoc.find ~equal:String.equal ctxt v with
    | None -> v
    | Some r -> r

  let rec expw : type s b e r.
    (string * string) list ->
    (e,r,b) parser -> e -> s bitv value t =
    fun ctxt self -> self.bitv (module struct
        type nonrec t = s bitv value t
        type exp = e
        type rmode = r

        let run = expw
        let expw s = run ctxt self s
        let expm s = expm ctxt self s
        let expb s = expb ctxt self s
        let expr s = expr ctxt self s
        let expf s = expf ctxt self s

        let load_word sz dir mem key =
          loadw (bits sz) (expb dir) (expm mem) (expw key)

        let load mem key = load (expm mem) (expw key)

        let add x y = add (expw x) (expw y)
        let sub x y = sub (expw x) (expw y)
        let mul x y = mul (expw x) (expw y)
        let div x y = div (expw x) (expw y)
        let sdiv x y = sdiv (expw x) (expw y)
        let modulo x y = modulo (expw x) (expw y)
        let smodulo x y = smodulo (expw x) (expw y)
        let lshift x y = lshift (expw x) (expw y)
        let rshift x y = rshift (expw x) (expw y)
        let arshift x y = arshift (expw x) (expw y)
        let logand x y = logand (expw x) (expw y)
        let logor x y = logor (expw x) (expw y)
        let logxor x y = logxor (expw x) (expw y)
        let var n sz = var (Var.create (bits sz) (rename ctxt n))

        let int x = of_word x
        let ite c x y = ite (expb c) (expw x) (expw y)
        let signed w x =
          let x = expw x in cast (bits w) (msb x) x
        let unsigned w x = cast (bits w) b0 (expw x)
        let high w x = high (bits w) (expw x)
        let low w x = low (bits w) (expw x)
        let append x y =
          let x = expw x and y = expw y in
          sort x >>= fun sx ->
          sort y >>= fun sy ->
          append (join sx sy) x y

        let cast rs bit x =
          cast (bits rs) (expb bit) (expw x)

        let concat xs =
          let xs = List.map ~f:expw xs in
          Knowledge.List.fold ~init:0 xs ~f:(fun s x ->
              sort x >>| fun sx ->
              s + Bits.size sx) >>= fun sz ->
          concat (bits sz) xs

        let let_ v x y =
          let x = expw x in
          sort x >>= fun s ->
          Var.Generator.fresh s >>= fun r ->
          let_ r x (run ((v,Var.name r)::ctxt) self y)

        let unknown w = unk (bits w)
        let extract sz hi lo x =
          extract (bits sz) (expw hi) (expw lo) (expw x)
        let neg x = neg (expw x)
        let not x = not (expw x)

        let cast_int s m w = cast_int (bits s) (expr m) (expf w)
        let cast_sint s m w = cast_sint (bits s) (expr m) (expf w)
      end)
  and expm : type k x b e r.
    (string * string) list ->
    (e,r,b) parser -> e -> (k,x) mem value t =
    fun ctxt self -> self.mem (module struct
        open Knowledge.Syntax
        type nonrec t = (k, x) mem value t
        type exp = e

        let run = expm
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expb s = expb ctxt self s

        let store m k x = store (expm m) (expw k) (expw x)
        let store_word d m k x =
          storew (expb d) (expm m) (expw k) (expw x)
        let var v ks vs =
          let s = Mems.define (bits ks) (bits vs) in
          var (Var.create s (rename ctxt v))
        let ite c x y = ite (expb c) (expm x) (expm y)
        let let_ v x y =
          let x = expm x in
          sort x >>= fun s ->
          Var.Generator.fresh s >>= fun r ->
          let_ r x @@ run ((v,Var.name r)::ctxt) self y
        let unknown ks vs = unk (Mems.define (bits ks) (bits vs))
      end)

  and expb : type s b e r.
    (string * string) list ->
    (e,r,b) parser -> e -> bit value t =
    fun ctxt self -> self.bool (module struct
        open Knowledge.Syntax
        type nonrec t = bit value t
        type exp = e

        let run = expb
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expf s = expf ctxt self s
        let expb s = run ctxt self s

        let var v = var (Var.create bool (rename ctxt v))
        let ite c x y = ite (expb c) (expb x) (expb y)
        let le x y =  ule (expw x) (expw y)
        let sle x y =  sle (expw x) (expw y)
        let lt x y = ult (expw x) (expw y)
        let slt x y =  slt (expw x) (expw y)
        let logor x y = or_ (expb x) (expb y)
        let logand x y = and_ (expb x) (expb y)
        let logxor x y =
          let x = expb x and y = expb y in
          and_ (or_ x y) (inv (and_ x y))
        let eq x y = eq (expw x) (expw y)
        let neq x y = neq (expw x) (expw y)
        let let_ v x y =
          let x = expb x in
          sort x >>= fun s ->
          Var.Generator.fresh s >>= fun r ->
          let_ r x @@ run ((v,Var.name r)::ctxt) self y
        let not x = inv (expb x)
        let unknown _ = (unk bool)

        let int x = if Word.is_zero x then b0 else b1

        let high x = lsb (high (bits 1) (expw x))
        let low x = lsb (low (bits 1) (expw x))

        let extract n x =
          let x = expw x in
          sort x >>= fun xs ->
          lsb (extract (bits 1) (of_int xs n) (of_int xs n) x)

        let is_snan x = is_snan (expf x)
        let is_qnan x = is_qnan (expf x)
        let is_pinf x = is_pinf (expf x)
        let is_ninf x = is_ninf (expf x)

        let fless = forder
        let feq x y = and_ (inv (fless x y)) (inv (fless y x))

        let fle x y =
          let x = expf x and y = expf y in
          or_ (fless x y) (feq x y)


        let flt x y = fless (expf x) (expf y)
        let feq x y = feq (expf x) (expf y)

      end)
  and expf : type a k b e r.
    (string * string) list ->
    (e,r,b) parser -> e -> (a,k) float value t =
    fun ctxt self -> self.float (module struct
        type nonrec t = (a,k) float value t
        type exp = e
        type rmode = r

        let run = expf
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expr s = expr ctxt self s
        let expb s = expb ctxt self s
        let expf s = run ctxt self s

        let floats es ks  = Floats.define (bits es) (bits ks)

        let var es ks name = var (Var.create (floats es ks) name)
        let unk es ks = unk (floats es ks)

        let finite es ks s e k =
          finite (floats es ks) (expb s) (expw e) (expw k)

        let pinf es ks = pinf (floats es ks)
        let ninf es ks = ninf (floats es ks)
        let snan es ks p = snan (floats es ks) (expw p)
        let qnan es ks p = qnan (floats es ks) (expw p)

        let fadd m x y = fadd (expr m) (expf x) (expf y)
        let fsub m x y = fsub (expr m) (expf x) (expf y)
        let fmul m x y = fmul (expr m) (expf x) (expf y)
        let fdiv m x y = fdiv (expr m) (expf x) (expf y)
        let frem m x y = fmodulo (expr m) (expf x) (expf y)
        let fmin x y =
          let x = expf x and y = expf y in
          ite (forder x y) x y
        let fmax x y =
          let x = expf x and y = expf y in
          ite (forder x y) y x

        let fabs x = fabs (expf x)
        let fneg x = fneg (expf x)
        let fsqrt m x = fsqrt (expr m) (expf x)
        let fround m x = fround (expr m) (expf x)

        let cast_float es ks m x =
          cast_float (floats es ks) (expr m) (expw x)

        let cast_sfloat es ks m x =
          cast_sfloat (floats es ks) (expr m) (expw x)

        let convert es ks m x =
          fconvert (floats es ks) (expr m) (expf x)
      end)
  and expr : type b e r.
    (string * string) list ->
    (e,r,b) parser -> r -> rmode value t =
    fun _ctxt self -> self.rmode (module struct
        type nonrec t = rmode value t
        type exp = r
        let rne = rne
        let rtz = rtz
        let rtp = rtp
        let rtn = rtn
        let rtz = rtz
        let rna = rna
      end)

  let rec run : type e s r. (e,r,s) parser -> s list -> unit eff t =
    fun parser code -> bil [] parser code

  and bil : type e s r. (string * string) list -> (e,r,s) parser -> s list -> unit eff t =
    fun ctxt parser xs -> stmts ctxt parser xs

  and stmts : type e s r.
    (string * string) list ->
    (e,r,s) parser -> s list -> unit eff t = fun ctxt self -> function
    | [] -> Knowledge.return Label.root >>= fun lbl -> blk lbl pass skip
    | x :: xs ->
      self.stmt (module struct
        type nonrec t = unit eff t
        type exp = e
        type stmt = s
        type rmode = r

        let next = stmts ctxt self

        let unlabeled = Label.root

        let bind exp body =
          sort exp >>= fun s ->
          Var.Generator.fresh s >>= fun v ->
          let b1 = (blk unlabeled (set v exp) skip) in
          seq b1 (body v)

        let special _ =
          seq (blk unlabeled pass skip) (next xs)

        let cpuexn n =
          link_ivec n >>= fun lbl ->
          seq (blk unlabeled pass (goto lbl)) (next xs)

        let while_ cnd ys =
          seq
            (blk unlabeled (repeat (expb ctxt self cnd) (stmtd ctxt self ys)) skip)
            (next xs)

        let if_ cnd yes nay =
          seq
            (branch (expb ctxt self cnd)
               (bil ctxt self yes)
               (bil ctxt self nay))
            (next xs)


        let goto addr =
          let addr = int (bits (Word.bitwidth addr)) addr in
          seq (blk unlabeled pass (jmp addr)) (next xs)

        let jmp exp =
          seq (blk unlabeled pass (jmp (expw ctxt self exp))) (next xs)

        (* let goto addr =
         *   link_addr addr >>= fun lbl ->
         *   seq (blk unlabeled pass (goto lbl)) (next xs) *)

        let move eff = seq (blk unlabeled eff skip) (next xs)
        let set_bit var exp = move (set_bit ctxt self var exp)
        let set_reg var sz exp = move (set_reg ctxt self var sz exp)
        let set_mem var ks vs exp = move (set_mem ctxt self var ks vs exp)
        let set_float var ks vs exp = move (set_float ctxt self var ks vs exp)
        let set_rmode var exp = move (set_rmode ctxt self var exp)
        let push var r = stmts ((var, Var.name r) :: ctxt) self xs
        let tmp_bit var exp = bind (expb ctxt self exp) (push var)
        let tmp_reg var exp = bind (expw ctxt self exp) (push var)
        let tmp_mem var exp = bind (expm ctxt self exp) (push var)
        let tmp_float var exp = bind (expf ctxt self exp) (push var)
        let tmp_rmode var exp = bind (expr ctxt self exp) (push var)
        let let_gen t var exp body =
          seq (bind (t ctxt self exp)
                 (fun r -> stmts ((var, Var.name r) :: ctxt) self [body]))
            (next xs)

        let let_bit = let_gen expb
        let let_reg = let_gen expw
        let let_mem = let_gen expm
        let let_float = let_gen expf
        let let_rmode = let_gen expr

        let seq ys = seq (next ys) (next xs)
      end) x

  and set_bit : type e s r.
    (string * string) list ->
    (e,r,s) parser -> string -> e -> data eff t =
    fun ctxt self v x -> set (Var.create bool v) (expb ctxt self x)

  and set_reg : type e s r.
    (string * string) list ->
    (e,r,s) parser -> string -> int -> e -> data eff t =
    fun ctxt self v s x ->
      set (Var.create (bits s) v) (expw ctxt self x)

  and set_mem : type e s r.
    (string * string) list ->
    (e,r,s) parser -> string -> int -> int -> e -> data eff t =
    fun ctxt self v ks vs x ->
      set (Var.create (Mems.define (bits ks) (bits vs)) v) (expm ctxt self x)

  and set_float : type e s r.
    (string * string) list ->
    (e,r,s) parser -> string -> int -> int -> e -> data eff t =
    fun ctxt self v es ks x ->
      set (Var.create (Floats.define (bits es) (bits ks)) v) (expf ctxt self x)

  and set_rmode : type e s r.
    (string * string) list ->
    (e,r,s) parser -> string -> r -> data eff t =
    fun ctxt self v x -> set (Var.create Rmode.t v) (expr ctxt self x)

  and stmtd : type e s r.
    (string * string) list ->
    (e,r,s) parser -> s list -> data eff t = fun ctxt self -> function
    | [] -> pass
    | x :: xs ->
      self.stmt (module struct
        type nonrec t = data eff t
        type exp = e
        type stmt = s
        type rmode = r

        let next = stmtd ctxt self

        let bind exp body =
          sort exp >>= fun s ->
          Var.Generator.fresh s >>= fun v ->
          seq (set v exp) (body v)

        let special _ = seq pass (next xs)
        let cpuexn _ = assert false
        let while_ cnd ys =
          seq
            (repeat (expb ctxt self cnd) (next ys))
            (next xs)

        let if_ cnd yes nay =
          seq
            (branch (expb ctxt self cnd)
               (stmtd ctxt self yes)
               (stmtd ctxt self nay))
            (next xs)


        let jmp _ = assert false
        let goto _ = assert false


        let move eff = seq eff (next xs)
        let set_bit var exp = move (set_bit ctxt self var exp)
        let set_reg var sz exp = move (set_reg ctxt self var sz exp)
        let set_mem var ks vs exp = move (set_mem ctxt self var ks vs exp)
        let set_float var ks vs exp = move (set_float ctxt self var ks vs exp)
        let set_rmode var exp = move (set_rmode ctxt self var exp)

        let push var r = stmtd ((var, Var.name r) :: ctxt) self xs
        let tmp_bit var exp = bind (expb ctxt self exp) (push var)
        let tmp_reg var exp = bind (expw ctxt self exp) (push var)
        let tmp_mem var exp = bind (expm ctxt self exp) (push var)
        let tmp_float var exp = bind (expf ctxt self exp) (push var)
        let tmp_rmode var exp = bind (expr ctxt self exp) (push var)

        let let_gen t var exp body =
          seq (bind (t ctxt self exp)
                 (fun r -> stmtd ((var, Var.name r) :: ctxt) self [body]))
            (next xs)

        let let_bit = let_gen expb
        let let_reg = let_gen expw
        let let_mem = let_gen expm
        let let_float = let_gen expf
        let let_rmode = let_gen expr

        let seq ys = seq (next ys) (next xs)
      end) x
end
