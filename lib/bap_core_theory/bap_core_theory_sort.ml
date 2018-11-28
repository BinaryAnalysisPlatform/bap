open Core_kernel
open Caml.Format

module Sort : sig
  type 'a t
  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving compare, sexp]

  val define : exp -> 'a -> 'a t
  val name : 'a t -> string
  val exp : 'a t -> exp
  val type_equal : 'a t -> 'b t -> ('a t, 'b t) Type_equal.t option
  val same : 'a t -> 'b t -> bool
  val pp_exp : formatter -> exp -> unit
  val pp : formatter -> 'a t -> unit
  val compare : 'a t -> 'a t -> int
end = struct
  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving compare, sexp]

  type 'a t = exp

  let rec pp_exp ppf = function
    | Bool -> fprintf ppf "Bool"
    | Cons (sym,[]) -> fprintf ppf "%s" sym
    | Cons (sym,p :: ps) ->
      fprintf ppf "(%s %a" sym pp_param p;
      List.iter ps ~f:(fprintf ppf " %a"  pp_param);
      fprintf ppf ")"
  and pp_param ppf = function
    | Sort x -> pp_exp ppf x
    | Index n -> fprintf ppf "%d" n

  let define exp _ = exp
  let exp x = x
  let name x = asprintf "%a" pp_exp x
  let same x y = compare_exp x y = 0

  let type_equal : type a b.
    a t -> b t -> (a t, b t) Type_equal.t option =
    fun x y -> if same x y then Some Type_equal.T else None

  let pp  = pp_exp
  let compare = compare_exp
end

type 'a sort = 'a Sort.t

module Bool = struct
  type t = Bit

  let t = Sort.define Sort.Bool Bit
  let parse = function
    | Sort.Bool -> Some t
    | _ -> None

  let cast s = parse (Sort.exp s)
end

module Bits = struct
  type 'a t = Bitv : 'a t

  let define (type s) width =
    Sort.(define (Cons ("Bits", [Index width])) Bitv)

  let size : 'a t sort -> int = fun s -> match Sort.exp s with
    | Cons (_, [Index width]) -> width
    | _ -> assert false

  let parse = function
    | Sort.Cons ("Bits", [Index width]) -> Some (define width)
    | _ -> None

  let cast s = parse (Sort.exp s)

end

module Mems = struct
  type ('a,'b) t = Mems : ('a,'b) t
  let define ks vs =
    Sort.(define (Cons (("Memory"),
                        [Sort (exp ks); Sort (exp vs)]))
            Mems)

  let bits e1 = Option.value_exn (Bits.parse e1)

  let keys x = match Sort.exp x with
    | Sort.Cons (_, [Sort e;_]) -> bits e
    | _ -> assert false

  let vals x = match Sort.exp x with
    | Sort.Cons (_, [_; Sort e]) -> bits e
    | _ -> assert false

  let parse = function
    | Sort.Cons ("Memory", [Sort e1; Sort e2]) ->
      Option.(Bits.parse e1 >>= fun s1 ->
              Bits.parse e2 >>| fun s2 ->
              define s1 s2)
    | _ -> None

  let cast s = parse (Sort.exp s)
end

module Floats = struct
  type ('e,'s) t = Floats : ('e,'s) t

  let define es ss =
    Sort.(define (Cons ("Floats", [Sort (exp es); Sort (exp ss)])) Floats)

  let bits e1 = Option.value_exn (Bits.parse e1)

  let sigs x = match Sort.exp x with
    | Sort.Cons ("Floats", [_; Sort e]) -> bits e
    | _ -> assert false

  let exps x = match Sort.exp x with
    | Sort.Cons ("Floats", [Sort e; _]) -> bits e
    | _ -> assert false
end


module Rmode = struct
  type t = Rmode
  type mode = RNE | RNA | RTP | RTN | RTZ
  let string_of_mode = function
    | RNE -> "RNE"
    | RNA -> "RNA"
    | RTP -> "RTP"
    | RTN -> "RTN"
    | RTZ -> "RTZ"

  let define mode =
    let m = Sort.(Cons (string_of_mode mode, [])) in
    Sort.(define (Cons ("RoundingMode", [Sort m])) Rmode)

  let rne = define RNE
  let rna = define RNA
  let rtp = define RTP
  let rtn = define RTN
  let rtz = define RTZ

  let describe s = match Sort.exp s with
    | Cons ("RoundingMode", [Sort (Cons ("RNE", []))]) -> RNE
    | Cons ("RoundingMode", [Sort (Cons ("RNA", []))]) -> RNA
    | Cons ("RoundingMode", [Sort (Cons ("RTP", []))]) -> RTP
    | Cons ("RoundingMode", [Sort (Cons ("RTN", []))]) -> RTN
    | Cons ("RoundingMode", [Sort (Cons ("RTZ", []))]) -> RTZ
    | _ -> assert false
end

type bit = Bool.t
type 'a bitv = 'a Bits.t
type ('a,'b) mem = ('a,'b) Mems.t
type ('a,'b) float = ('a,'b) Floats.t
type rmode = Rmode.t
