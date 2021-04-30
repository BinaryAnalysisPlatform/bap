open Core_kernel
open Caml.Format

open Bap_knowledge
module KB = Knowledge

let package = "core"


module Sort : sig
  type +'a exp
  type +'a sym
  type +'a num
  type +'a t = 'a exp
  type cls

  type top = unit t
  type name

  val cls : (cls,unit) KB.cls

  val sym : name -> 'a sym exp
  val int : int -> 'a num exp
  val app : 'a exp -> 'b exp -> ('a -> 'b) exp
  val (@->) : 'a exp -> 'b exp -> ('a -> 'b) exp

  val value : 'a num exp -> int
  val name :  'a sym exp -> name

  val hd : ('a -> 'b) exp -> 'a exp
  val tl : ('a -> 'b) exp -> 'b exp

  val pp : formatter -> 'a t -> unit
  val forget : 'a t -> top
  val refine : name -> top -> 'a t option
  val same : 'a t -> 'b t -> bool

  module Top : sig
    type t = top [@@deriving bin_io, compare, sexp]
    val t : top
    include Base.Comparable.S with type t := t
  end

  module Name : sig
    type t
    val declare : ?package:string -> string -> name
    include Base.Comparable.S with type t := t
  end
end
= struct
  type +'a sym
  type +'a num
  type cls = Values

  type name = KB.Name.t [@@deriving bin_io, compare, sexp]

  let names = Hash_set.create (module KB.Name)
  let short_names = Hashtbl.create (module String)

  let cls = KB.Class.declare ~package:"core" "value" ()
      ~public:true
      ~desc:"the denotation of an expression"

  module Name = struct
    type t = name [@@deriving bin_io, compare, sexp]
    let declare ?(package="user") name =
      let key = KB.Name.create ~package name in
      if Hash_set.mem names key then
        failwithf "Type name `%s' is already defined \
                   for package `%s'. Please, pick a unique \
                   name or a different package." name package ();
      Hashtbl.update short_names name ~f:(function
          | None -> 1
          | Some x -> x + 1);
      key

    let to_string name =
      let short = KB.Name.unqualified name in
      if Hashtbl.find_exn short_names short = 1
      then short
      else sprintf "%s" (KB.Name.to_string name)

    include Base.Comparable.Make(struct
        type t = name [@@deriving bin_io, compare, sexp]
      end)
  end

  module Exp = struct
    type t =
      | Sym of name
      | Int of int
      | App of {args : t list; name : name option}
    [@@deriving bin_io, compare, sexp]
  end
  open Exp

  type +'a exp = Exp.t
  type +'a t = 'a exp
  type top = unit t

  let app s p = match p with
    | App {args=xs; name} -> App {args=s::xs; name}
    | Int _ -> App {args=[s;p]; name=None}
    | Sym name -> App {args=[s;p]; name = Some name}

  let sym s = Sym s
  let int s = Int s

  let (@->) = app

  let name = function Sym s -> s
                    | _ -> assert false
  let value = function Int s -> s
                     | _ -> assert false

  let hd = function App {args=x::_} -> x
                  | _ -> assert false
  let tl = function App {args=_::xs; name} -> App {args=xs;name}
                  | _ -> assert false

  let is_digit s = String.length s > 0 && Char.is_digit s.[0]

  open Format

  let pp_sep ppf () = fprintf ppf ",@ "


  let rec pp ppf = function
    | Sym s -> fprintf ppf "%s" (Name.to_string s)
    | Int n -> fprintf ppf "%d" n
    | App {args=xs} ->
      let f,args =
        let sx = List.rev xs in
        List.hd_exn sx, List.(rev @@ tl_exn sx) in
      fprintf ppf "%a(%a)" pp f
        (pp_print_list ~pp_sep pp) args



  let forget = ident
  let refine witness t = match t with
    | App {name=Some name}
    | Sym name when [%compare.equal: name] name witness -> Some t
    | _ -> None

  let forget : 'a t -> unit t = ident

  let same x y = Exp.compare x y = 0

  module Top = struct
    type t = top
    let any = Name.declare ~package "Top"
    let t = forget (sym any)

    include Sexpable.Of_sexpable(Exp)(struct
        type t = top
        let to_sexpable x = x
        let of_sexpable x = x
      end)

    include Binable.Of_binable(Exp)(struct
        type t = top
        let to_binable x = x
        let of_binable x = x
      end)[@@warning "-D"]

    include Base.Comparable.Inherit(Exp)(struct
        type t = top
        let sexp_of_t x = Exp.sexp_of_t x
        let component x = x
      end)
  end
end

type 'a sort = 'a Sort.t
type 'a sym = 'a Sort.sym
type 'a num = 'a Sort.num
type cls = Sort.cls
type 'a t = (cls,'a sort) KB.cls KB.value
let cls = Sort.cls
let empty s : 'a t = KB.Value.empty (KB.Class.refine cls s)
let sort v : 'a sort = KB.Class.sort (KB.Value.cls v)
let resort : ('a sort -> 'b sort option) -> 'a t -> 'b t option =
  fun refine v ->
  Option.(refine (sort v) >>| KB.Value.refine v)

let forget : 'a t -> unit t = fun v ->
  KB.Value.refine v @@ Sort.forget @@ sort v

module type Sort = sig
  val refine : unit sort -> 'a sort option
end

module Top = struct
  let cls = KB.Class.refine cls Sort.Top.t
  include (val KB.Value.derive cls)
end

module Bool : sig
  type t
  val t : t sort
  val refine : Sort.top -> t sort option
end = struct
  type bool and t = bool sym
  let bool = Sort.Name.declare ~package "Bool"
  let t = Sort.sym bool
  let refine x = Sort.refine bool x
end

module Bitv : sig
  type 'a t
  val define : int -> 'a t sort
  val refine : Sort.top -> 'a t sort option
  val size : 'a t sort -> int
end = struct
  type bitv
  type 'a t = 'a num -> bitv sym
  let bitvec = Sort.Name.declare ~package "BitVec"
  let define m : 'a t sort = Sort.(int m @-> sym bitvec)
  let refine s = Sort.refine bitvec s
  let size x = Sort.(value @@ hd x)
end

module Mem : sig
  type ('a,'b) t
  val define : 'a Bitv.t sort -> 'b Bitv.t sort -> ('a,'b) t sort
  val refine : Sort.top -> ('a,'b) t sort option
  val keys : ('a,'b) t sort -> 'a Bitv.t sort
  val vals : ('a,'b) t sort -> 'b Bitv.t sort
end = struct
  type mem
  type ('a,'b) t = 'a Bitv.t -> 'b Bitv.t -> mem sym
  let mem = Sort.Name.declare ~package "Mem"
  let define (ks : 'a Bitv.t sort) (vs : 'b Bitv.t sort) : ('a,'b) t sort =
    Sort.(ks @-> vs @-> sym mem)
  let refine x = Sort.refine mem x
  let keys x = Sort.(hd x)
  let vals x = Sort.(hd (tl x))
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
  val refine : Sort.top -> ('r,'s) format t sort option
  val format : ('r,'s) format t sort -> ('r,'s) format Sort.exp
  val bits : ('r,'s) format t sort -> 's Bitv.t sort
end = struct
  module Format = struct
    type ('r,'s) t = ('r -> 's Bitv.t)
    let define repr bits = Sort.(repr @-> bits)
    let bits x = Sort.(tl x)
    let exp x = Sort.(hd x)
  end
  type float
  type ('r,'s) format = ('r,'s) Format.t
  type 'f t = 'f -> float sym
  let float = Sort.Name.declare ~package "Float"
  let define fmt = Sort.(fmt @-> sym float)
  let refine x = Sort.refine float x
  let format x = Sort.(hd x)
  let bits x = Format.bits (format x)
end

module Rmode : sig
  type t
  val t : t sort
  val refine : unit sort -> t sort option
end
= struct
  type rmode
  type t = rmode sym
  let rmode = Sort.Name.declare "Rmode"
  let t = Sort.(sym rmode)
  let refine x = Sort.refine rmode x
end

type 'a value = 'a t

module Match : sig
  type 'a t
  type 'a refiner = unit sort -> 'a sort option
  val (let|) : 'b t -> (unit -> 'b) -> 'b
  val can : 'a refiner -> unit value -> ('a value -> 'b) -> 'b t
  val both :
    'a refiner -> unit value ->
    'b refiner -> unit value ->
    ('a value -> 'b value -> 'c) -> 'c t
end = struct
  type 'a refiner = unit sort -> 'a sort option
  type 'a value = 'a t
  type 'b t = (unit -> 'b) -> 'b
  let (let|) = (@@)
  let can cast x action k =
    match resort cast x with
    | None -> k ()
    | Some x -> action x
  let both castx x casty y action k =
    match resort castx x, resort casty y with
    | Some x, Some y -> action x y
    | _ -> k ()
end
