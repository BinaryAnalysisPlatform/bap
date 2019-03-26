open Core_kernel
open Caml.Format

open Bap_knowledge
module KB = Knowledge

let package = "core-theory"

module Sort
  : sig
    type exp =
      | Bool
      | Cons of string * param list
    and param =
      | Sort of exp
      | Index of int
    [@@deriving bin_io, compare, sexp]

    type 'a definition

    type 'a t = 'a definition KB.cls

    val define : exp -> 'a -> 'a t

    val exp : 'a t -> exp
    val pp_exp : formatter -> exp -> unit
    val pp : formatter -> 'a t -> unit
  end
= struct
  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving bin_io, compare, sexp]

  type 'a definition = exp
  type 'a t = 'a definition Knowledge.cls

  let base =
    KB.Class.abstract ~package "value"
      ~desc:"result of a computation"

  let define exp _ : 'a t =
    KB.Class.refine base exp

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

  let exp x = KB.Class.(data x)
  let pp ppf x = pp_exp ppf (exp x)
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

module Bitv = struct
  type 'a t = Bitv : 'a t

  let define (type s) width =
    Sort.(define (Cons ("Bitv", [Index width])) Bitv)

  let size : 'a t sort -> int = fun s -> match Sort.exp s with
    | Cons (_, [Index width]) -> width
    | _ -> assert false

  let parse = function
    | Sort.Cons ("Bitv", [Index width]) -> Some (define width)
    | _ -> None

  let cast s = parse (Sort.exp s)
end

module Mem = struct
  type ('a,'b) t = Mems : ('a,'b) t
  let define ks vs =
    Sort.(define (Cons (("Memory"),
                        [Sort (exp ks); Sort (exp vs)]))
            Mems)

  let bits e1 = Option.value_exn (Bitv.parse e1)

  let keys x = match Sort.exp x with
    | Sort.Cons (_, [Sort e;_]) -> bits e
    | _ -> assert false

  let vals x = match Sort.exp x with
    | Sort.Cons (_, [_; Sort e]) -> bits e
    | _ -> assert false

  let parse = function
    | Sort.Cons ("Memory", [Sort e1; Sort e2]) ->
      Option.(Bitv.parse e1 >>= fun s1 ->
              Bitv.parse e2 >>| fun s2 ->
              define s1 s2)
    | _ -> None

  let cast s = parse (Sort.exp s)
end

module Float = struct
  type 'f t = Floats : 'f t

  module Format = struct
    type (_,_) t = Sort.exp * int
    let define exp _ bits = exp, Bitv.size bits
    let exp (x,_) = x
    let bits (_,x) = Bitv.define x
  end

  type ('f,'s) format = ('f,'s) Format.t

  let define (fmt,sz) =
    Sort.(define (Cons ("Floats", [Sort fmt; Index sz])) Floats)

  let format x = match Sort.exp x with
    | Sort.Cons ("Floats", [Sort fmt; Index sz]) -> fmt, sz
    | _ -> assert false

  let size x = Format.bits (format x)

  let parse = function
    | Sort.Cons ("Floats", [Sort fmt; Index sz]) -> Some (define (fmt, sz))
    | _ -> None

  let cast s = parse (Sort.exp s)

end


module Rmode = struct
  type t = Rmode
  let t = Sort.(define (Cons ("RoundingMode", [])) Rmode)
end
