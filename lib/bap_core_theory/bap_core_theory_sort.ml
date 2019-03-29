open Core_kernel
open Caml.Format

open Bap_knowledge
module KB = Knowledge

let package = "core-theory"

module Sort : sig
  type +'a exp
  type +'a sym
  type +'a num

  type 'a t = 'a exp KB.cls

  val sym : string -> 'a sym exp
  val int : int -> 'a num exp
  val app : 'a exp -> 'b exp -> ('a -> 'b) exp
  val (@->) : 'a exp -> 'b exp -> ('a -> 'b) exp

  val t : 'a exp KB.Class.abstract KB.Class.t

  val exp : 'a t -> 'a exp

  val value : 'a num exp -> int
  val name :  'a sym exp -> string

  val hd : ('a -> 'b) exp -> 'a exp
  val tl : ('a -> 'b) exp -> 'b exp

  val pp : formatter -> 'a t -> unit

  module Magic : sig
    val dump : 'a t -> string
    val load : string -> 'a t
  end
end
= struct
  type +'a sym
  type +'a num

  module Exp = struct
    type t =
      | Sym of string
      | Int of int
      | App of t list
    [@@deriving bin_io, compare, sexp]
  end
  open Exp

  type +'a exp = Exp.t


  let app s p = match p with
    | App xs -> App (s::xs)
    | _ -> App [s;p]
  let sym s = Sym s
  let int s = Int s

  let (@->) = app

  let name = function Sym s -> s
                    | _ -> assert false
  let value = function Int s -> s
                     | _ -> assert false

  let hd = function App (x::_) -> x
                  | _ -> assert false
  let tl = function App (_::xs) -> App xs
                  | _ -> assert false


  let rec to_sexp = function
    | Sym s -> Sexp.Atom s
    | Int s -> Sexp.Atom (string_of_int s)
    | App xs -> Sexp.List (List.map xs ~f:to_sexp)

  let is_digit s = String.length s > 0 && Char.is_digit s.[0]


  let rec of_sexp  = function
    | Sexp.Atom x when is_digit x -> Int (int_of_string x)
    | Sexp.Atom x -> Sym x
    | Sexp.List xs -> App (List.map xs ~f:of_sexp)

  open Format

  let pp_sep ppf () = fprintf ppf ",@ "


  let rec pp ppf = function
    | Sym s -> fprintf ppf "%s" s
    | Int n -> fprintf ppf "%d" n
    | App xs ->
      let f,args =
        let sx = List.rev xs in
        List.hd_exn sx, List.(rev @@ tl_exn sx) in
      fprintf ppf "%a(%a)" pp f
        (pp_print_list ~pp_sep pp) args

  type 'a t = 'a exp Knowledge.cls

  let base =
    KB.Class.abstract ~package "value"
      ~desc:"result of a computation"
  let t = base

  let define exp _ : 'a t =
    KB.Class.refine base exp
  let forget = ident
  let refine s _ = s

  let exp x = KB.Class.(data x)
  let pp ppf x = pp ppf (exp x)

  module Magic = struct
    let dump x = Binable.to_string (module Exp) (exp x)

    let load s =
      KB.Class.refine t @@
      Binable.of_string (module Exp) s
  end

end

type 'a sort = 'a Sort.t
type 'a sym = 'a Sort.sym
type 'a num = 'a Sort.num


module Bool : sig
  type t
  val t : t sort
end = struct
  type bool and t = bool sym
  let t = KB.Class.refine Sort.t (Sort.sym "Bool")
end

module Bitv : sig
  type 'a t
  val define : int -> 'a t sort
  val size : 'a t sort -> int
end = struct
  type bitv
  type 'a t = 'a num -> bitv sym
  let define m : 'a t sort =
    KB.Class.refine Sort.t Sort.(int m @-> sym "BitVec")
  let size x = Sort.(value @@ hd (exp x))
end

module Mem : sig
  type ('a,'b) t
  val define : 'a Bitv.t sort -> 'b Bitv.t sort -> ('a,'b) t sort
  val keys : ('a,'b) t sort -> 'a Bitv.t sort
  val vals : ('a,'b) t sort -> 'b Bitv.t sort
end = struct
  type mem
  type ('a,'b) t = 'a Bitv.t -> 'b Bitv.t -> mem sym
  let define (ks : 'a Bitv.t sort) (vs : 'b Bitv.t sort) : ('a,'b) t sort =
    KB.Class.refine Sort.t Sort.(exp ks @-> exp vs @-> sym "Mem")
  let keys x = KB.Class.refine Sort.t Sort.(hd (exp x))
  let vals x = KB.Class.refine Sort.t Sort.(hd (tl (exp x)))
end


module Float : sig
  module Format : sig
    type ('r,'s) t
    val define : 'r Sort.exp -> 's Bitv.t sort -> ('r,'s) t Sort.exp
    val bits : ('r,'s) t Sort.exp -> 's Bitv.t sort
    val exp : ('r,'s) t Sort.exp -> 'r Sort.exp
  end

  type ('r,'s) format = ('r,'s) Format.t
  type 'f t

  val define : ('r,'s) format Sort.exp -> ('r,'s) format t sort
  val format : ('r,'s) format t sort -> ('r,'s) format Sort.exp
  val size : ('r,'s) format t sort -> 's Bitv.t sort
end = struct
  module Format = struct
    type ('r,'s) t = ('r -> 's Bitv.t)
    let define repr bits = Sort.(repr @-> exp bits)
    let bits x = KB.Class.refine Sort.t @@ Sort.(tl x)
    let exp x = Sort.(hd x)
  end
  type float
  type ('r,'s) format = ('r,'s) Format.t
  type 'f t = 'f -> float sym

  let define fmt =
    KB.Class.refine Sort.t @@
    Sort.(fmt @-> sym "Float")

  let format x = Sort.(hd (exp x))
  let size x = Format.bits (format x)
end


module Rmode : sig
  type t
  val t : t sort
end
= struct
  type rmode
  type t = rmode sym
  let t =
    KB.Class.refine Sort.t Sort.(sym "Rmode")
end
