open Core_kernel
open Bap_knowledge
open Bap_core_theory_definition
open Bap_core_theory_value

module Value = Knowledge.Value
module Grammar = Bap_core_theory_grammar_definition
module Program = Bap_core_theory_program
module Label = Program.Label
module IEEE754 = Bap_core_theory_IEEE754

open Knowledge.Syntax

type ('a,'e,'r) bitv_parser =
  (module Grammar.Bitv with type t = 'a
                        and type exp = 'e
                        and type rmode = 'r) ->
  'e -> 'a

type ('a,'e,'r) bool_parser =
  (module Grammar.Bool with type t = 'a
                        and type exp = 'e) ->
  'e -> 'a

type ('a,'e) mem_parser =
  (module Grammar.Mem with type t = 'a
                       and type exp = 'e) ->
  'e -> 'a

type ('a,'e,'r,'s) stmt_parser =
  (module Grammar.Stmt with type t = 'a
                        and type exp = 'e
                        and type stmt = 's
                        and type rmode = 'r) ->
  's -> 'a

type ('a,'e,'r) float_parser =
  (module Grammar.Float with type t = 'a
                         and type exp = 'e
                         and type rmode = 'r) ->
  'e -> 'a

type ('a,'e) rmode_parser =
  (module Grammar.Rmode with type t = 'a
                         and type exp = 'e) ->
  'e -> 'a

type ('e,'r,'s) t = {
  bitv : 'a. ('a,'e,'r) bitv_parser;
  bool : 'a. ('a,'e,'r) bool_parser;
  mem  : 'a. ('a,'e) mem_parser;
  stmt : 'a. ('a,'e,'r,'s) stmt_parser;
  float : 'a . ('a,'e,'r) float_parser;
  rmode : 'a . ('a,'r) rmode_parser;
}

type ('e,'r,'s) parser = ('e,'r,'s) t

let bits = Bitv.define
let bool = Bool.t

type Knowledge.conflict += Error

let (>>->) x f =
  x >>= fun x -> f (KB.Class.sort (Value.cls x)) x

module Make(S : Core) = struct
  open S
  open Knowledge.Syntax

  type 'a t = 'a knowledge

  let of_word w s = int (bits s) w
  let of_int s x =
    let m = Bitvec.modulus (Bitv.size s) in
    int s Bitvec.(int x mod m)
  let join s1 s2 = bits (Bitv.size s1 + Bitv.size s2)

  let mkvar sort name =
    Var.create sort (Var.Ident.of_string name)

  type context = (string * Var.ident) list
  let rename (ctxt : context) v =
    match List.Assoc.find ~equal:String.equal ctxt v with
    | None -> Var.Ident.of_string v
    | Some r -> r

  let pass = perform Effect.Sort.bot
  let skip = perform Effect.Sort.bot
  let null = Program.Label.null

  let rec expw : type s b e r.
    context ->
    (e,r,b) parser -> e -> s bitv =
    fun ctxt self -> self.bitv (module struct
        type nonrec t = s bitv
        type exp = e
        type rmode = r

        let run = expw
        let expw s = run ctxt self s
        let expm s = expm ctxt self s
        let expb s = expb ctxt self s
        let expr s = expr ctxt self s
        let expf s = expf ctxt self s

        let error = Knowledge.fail Error

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

        let int x s = of_word x s
        let ite c x y = ite (expb c) (expw x) (expw y)
        let signed w x = signed (bits w) (expw x)
        let unsigned w x = unsigned (bits w) (expw x)
        let high w x = high (bits w) (expw x)
        let low w x = low (bits w) (expw x)
        let append x y =
          let x = expw x and y = expw y in
          x >>-> fun sx x ->
          y >>-> fun sy y ->
          append (join sx sy) !!x !!y

        let cast rs bit x =
          cast (bits rs) (expb bit) (expw x)

        let concat xs =
          Knowledge.List.fold ~init:([],0) xs ~f:(fun (xs,s) x ->
              expw x >>| fun x ->
              !!x::xs, s + Bitv.size (KB.Class.sort (Value.cls x)))
          >>= fun (xs,sz) ->
          concat (bits sz) (List.rev xs)

        let let_bit v x y =
          expb x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_reg v x y =
          expw x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_mem v x y =
          expm x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_float v x y =
          expf x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let unknown w = unk (bits w)
        let extract sz hi lo x =
          extract (bits sz) (expw hi) (expw lo) (expw x)
        let neg x = neg (expw x)
        let not x = not (expw x)

        let cast_int s m w = cast_int (bits s) (expr m) (expf w)
        let cast_sint s m w = cast_sint (bits s) (expr m) (expf w)

        let fbits x = fbits (expf x)
      end)
  and expm : type k x b e r.
    context ->
    (e,r,b) parser -> e -> (k,x) mem =
    fun ctxt self -> self.mem (module struct
        open Knowledge.Syntax
        type nonrec t = (k, x) mem
        type exp = e

        let run = expm
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expb s = expb ctxt self s
        let expf s = expf ctxt self s

        let error = Knowledge.fail Error

        let store m k x = store (expm m) (expw k) (expw x)
        let store_word d m k x =
          storew (expb d) (expm m) (expw k) (expw x)
        let var v ks vs =
          let s = Mem.define (bits ks) (bits vs) in
          var (Var.create s (rename ctxt v))
        let ite c x y = ite (expb c) (expm x) (expm y)

        let let_bit v x y =
          expb x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_reg v x y =
          expw x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_mem v x y =
          expm x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_float v x y =
          expm x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let unknown ks vs = unk (Mem.define (bits ks) (bits vs))
      end)

  and expb : type s b e r.
    context ->
    (e,r,b) parser -> e -> bool =
    fun ctxt self -> self.bool (module struct
        open Knowledge.Syntax
        type nonrec t = bool
        type exp = e

        let run = expb
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expf s = expf ctxt self s
        let expb s = run ctxt self s

        let error = Knowledge.fail Error

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

        let let_bit v x y =
          expb x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_reg v x y =
          expw x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_mem v x y =
          expm x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_float v x y =
          expf x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let not x = inv (expb x)
        let unknown _ = (unk bool)

        let int x = if Bitvec.(x = zero) then b0 else b1

        let high x = lsb (high (bits 1) (expw x))
        let low x = lsb (low (bits 1) (expw x))

        let extract n x =
          expw x >>-> fun xs x ->
          lsb (extract (bits 1) (of_int xs n) (of_int xs n) !!x)

        let fless = forder
        let feq x y = and_ (inv (fless x y)) (inv (fless y x))

        let fle x y =
          let x = expf x and y = expf y in
          or_ (fless x y) (feq x y)

        let flt x y = fless (expf x) (expf y)
        let feq x y = feq (expf x) (expf y)

        let is_fneg x = is_fneg (expf x)
        let is_fpos x = is_fpos (expf x)
        let is_fzero x = is_fzero (expf x)
        let is_nan x = is_nan (expf x)
        let is_inf x = is_inf (expf x)
      end)
  and expf : type s b e r k n i g a.
    context ->
    (e,r,b) parser -> e -> ((i, g, a) IEEE754.t, s) format float =
    fun ctxt self -> self.float (module struct
        type nonrec t = ((i, g, a) IEEE754.t, s) format float
        type exp = e
        type rmode = r

        let run = expf
        let expw s = expw ctxt self s
        let expm s = expm ctxt self s
        let expr s = expr ctxt self s
        let expb s = expb ctxt self s
        let expf s = run ctxt self s

        let error = Knowledge.fail Error

        let floats s = IEEE754.Sort.define s
        let ieee754 s x : t  = float (floats s) (expw x)
        let ieee754_var s name : t = var (mkvar (floats s) name)
        let ieee754_unk s = unk (floats s)

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

        let ite c x y = ite (expb c) (expf x) (expf y)
        let fabs x = fabs (expf x)
        let fneg x = fneg (expf x)
        let fsqrt m x = fsqrt (expr m) (expf x)
        let fround m x = fround (expr m) (expf x)

        let ieee754_cast s m x =
          cast_float (floats s) (expr m) (expw x)

        let ieee754_cast_signed s m x =
          cast_sfloat (floats s) (expr m) (expw x)

        let ieee754_convert s m x =
          fconvert (floats s) (expr m) (expf x)

        let let_bit v x y =
          expb x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_reg v x y =
          expw x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_mem v x y =
          expm x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)

        let let_float v x y =
          expf x >>-> fun s x ->
          Var.scoped s @@ fun r ->
          let_ r !!x (run ((v,Var.ident r)::ctxt) self y)
      end)
  and expr : type b e r.
    context ->
    (e,r,b) parser -> r -> rmode =
    fun _ctxt self -> self.rmode (module struct
        type nonrec t = rmode
        type exp = r
        let error = Knowledge.fail Error
        let rne = rne
        let rtz = rtz
        let rtp = rtp
        let rtn = rtn
        let rtz = rtz
        let rna = rna
      end)

  let rec run : type e s r. (e,r,s) parser -> s list -> unit eff =
    fun parser code -> bil [] parser code

  and bil : type e s r. context -> (e,r,s) parser -> s list -> unit eff =
    fun ctxt parser xs -> stmts ctxt parser xs

  and stmts : type e s r.
    context ->
    (e,r,s) parser -> s list -> unit eff = fun ctxt self -> function
    | [] -> blk null pass skip
    | x :: xs ->
      self.stmt (module struct
        type nonrec t = unit eff
        type exp = e
        type stmt = s
        type rmode = r

        let next = stmts ctxt self

        let bind exp body =
          exp >>-> fun s exp ->
          Var.fresh s >>= fun v ->
          let b1 = blk null (set v !!exp) skip in
          seq b1 (body v)

        let error = Knowledge.fail Error

        let special _ =
          seq (blk null pass skip) (next xs)

        let cpuexn n =
          Label.for_ivec n >>= fun dst ->
          seq (blk null pass (goto dst)) (next xs)

        let while_ cnd ys =
          seq
            (blk null (repeat (expb ctxt self cnd) (stmtd ctxt self ys)) skip)
            (next xs)

        let if_ cnd yes nay =
          seq
            (branch (expb ctxt self cnd)
               (bil ctxt self yes)
               (bil ctxt self nay))
            (next xs)

        let jmp exp =
          seq (blk null pass (jmp (expw ctxt self exp))) (next xs)

        let call name =
          Label.for_name name >>= fun dst ->
          Knowledge.provide Label.is_subroutine dst (Some true) >>= fun () ->
          seq (blk null pass (goto dst)) (next xs)

        let goto addr =
          Label.for_addr addr >>= fun dst ->
          seq (blk null pass (goto dst)) (next xs)

        let move eff =
          seq (blk null eff skip) (next xs)
        let set_bit var exp = move (set_bit ctxt self var exp)
        let set_reg var sz exp = move (set_reg ctxt self var sz exp)
        let set_mem var ks vs exp = move (set_mem ctxt self var ks vs exp)
        let set_ieee754 var s exp = move (set_ieee754 ctxt self var s exp)
        let set_rmode var exp = move (set_rmode ctxt self var exp)
        let push var r = stmts ((var, Var.ident r) :: ctxt) self xs
        let tmp_bit var exp = bind (expb ctxt self exp) (push var)
        let tmp_reg var exp = bind (expw ctxt self exp) (push var)
        let tmp_mem var exp = bind (expm ctxt self exp) (push var)
        let tmp_float var exp = bind (expf ctxt self exp) (push var)
        let tmp_rmode var exp = bind (expr ctxt self exp) (push var)
        let let_gen t var exp body =
          seq (bind (t ctxt self exp)
                 (fun r -> stmts ((var, Var.ident r) :: ctxt) self [body]))
            (next xs)

        let let_bit = let_gen expb
        let let_reg = let_gen expw
        let let_mem = let_gen expm
        let let_float = let_gen expf
        let let_rmode = let_gen expr

        let seq ys = seq (next ys) (next xs)
      end) x

  and set_bit : type e s r.
    context ->
    (e,r,s) parser -> string -> e -> data eff =
    fun ctxt self v x -> set (mkvar bool v) (expb ctxt self x)

  and set_reg : type e s r.
    context ->
    (e,r,s) parser -> string -> int -> e -> data eff =
    fun ctxt self v s x ->
    set (mkvar (bits s) v) (expw ctxt self x)

  and set_mem : type e s r.
    context ->
    (e,r,s) parser -> string -> int -> int -> e -> data eff =
    fun ctxt self v ks vs x ->
    set (mkvar (Mem.define (bits ks) (bits vs)) v) (expm ctxt self x)

  and set_ieee754 : type e s r.
    context ->
    (e,r,s) parser -> string -> IEEE754.parameters -> e -> data eff =
    fun ctxt self v fs x -> set (mkvar (IEEE754.Sort.define fs) v) (expf ctxt self x)

  and set_rmode : type e s r.
    context ->
    (e,r,s) parser -> string -> r -> data eff =
    fun ctxt self v x -> set (mkvar Rmode.t v) (expr ctxt self x)

  and stmtd : type e s r.
    context ->
    (e,r,s) parser -> s list -> data eff = fun ctxt self -> function
    | [] -> pass
    | x :: xs ->
      self.stmt (module struct
        type nonrec t = data eff
        type exp = e
        type stmt = s
        type rmode = r

        let next = stmtd ctxt self

        let bind exp body =
          exp >>-> fun s exp ->
          Var.fresh s >>= fun v ->
          seq (set v !!exp) (body v)

        let error = Knowledge.fail Error

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
        let call _ = assert false

        let move eff = seq eff (next xs)
        let set_bit var exp = move (set_bit ctxt self var exp)
        let set_reg var sz exp = move (set_reg ctxt self var sz exp)
        let set_mem var ks vs exp = move (set_mem ctxt self var ks vs exp)
        let set_ieee754 var s exp = move (set_ieee754 ctxt self var s exp)
        let set_rmode var exp = move (set_rmode ctxt self var exp)

        let push var r = stmtd ((var, Var.ident r) :: ctxt) self xs
        let tmp_bit var exp = bind (expb ctxt self exp) (push var)
        let tmp_reg var exp = bind (expw ctxt self exp) (push var)
        let tmp_mem var exp = bind (expm ctxt self exp) (push var)
        let tmp_float var exp = bind (expf ctxt self exp) (push var)
        let tmp_rmode var exp = bind (expr ctxt self exp) (push var)

        let let_gen t var exp body =
          seq (bind (t ctxt self exp)
                 (fun r -> stmtd ((var, Var.ident r) :: ctxt) self [body]))
            (next xs)

        let let_bit = let_gen expb
        let let_reg = let_gen expw
        let let_mem = let_gen expm
        let let_float = let_gen expf
        let let_rmode = let_gen expr

        let seq ys = seq (next ys) (next xs)
      end) x
end
