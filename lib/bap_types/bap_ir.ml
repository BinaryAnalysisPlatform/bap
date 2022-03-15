let package = "bap"

open Core_kernel
open Bap_core_theory
open Regular.Std
open Bap_common
open Bap_bil
open Bap_knowledge

module Toplevel = Bap_toplevel
module Value = Bap_value
module Dict = Value.Dict
module Vec = Bap_vector
module Var = Bap_var

module Bil = struct
  include Bap_visitor
  include Bap_helpers
end

module Exp = struct
  type t = exp
  include Bap_exp
  include Exp
  include Bap_helpers.Exp
end

module Stmt = struct
  include Bap_stmt
  include Stmt
  include Bap_helpers.Stmt
end

type dict = Dict.t [@@deriving bin_io, compare, sexp]
type 'a vector = 'a Vec.t

module Tid = struct
  open KB.Syntax
  type t = Theory.Label.t [@@deriving bin_io, compare, sexp]
  let last = Toplevel.var "last"
  let name = Toplevel.var "name"
  let repr = Toplevel.var "repr"
  let ivec = Toplevel.var "ivec"
  let addr = Toplevel.var "addr"


  let generate ?package f x =
    Toplevel.put last (f ?package x);
    Toplevel.get last

  let for_ivec ?package s =
    generate ?package Theory.Label.for_ivec s
  let for_addr ?package s =
    generate ?package Theory.Label.for_addr @@
    Bap_bitvector.to_bitvec s

  let set slot tid name = Toplevel.exec begin
      KB.provide slot tid (Some name)
    end

  let set_addr t w = set Theory.Label.addr t (Bap_bitvector.to_bitvec w)
  let set_ivec = set Theory.Label.ivec


  let get slot tid = Toplevel.eval slot (Knowledge.return tid)

  let get_name = get Theory.Label.name
  (* let get_addr = get Theory.Label.addr addr *)
  let get_ivec = get Theory.Label.ivec

  let add_name tid name = Toplevel.exec begin
      KB.provide Theory.Label.aliases tid @@
      Set.singleton (module String) name
    end

  let set_name tid name =
    set Theory.Label.name tid name;
    add_name tid name


  let for_name ?package s =
    let t = generate ?package Theory.Label.for_name s in
    set_name t s;
    t

  let intern n =
    Toplevel.put name begin
      KB.Symbol.intern n Theory.Program.cls >>= fun t ->
      KB.provide Theory.Label.name t (Some n) >>| fun () ->
      t
    end;
    Toplevel.get name

  let repr tid =
    Toplevel.put repr (KB.Object.repr Theory.Program.cls tid);
    Toplevel.get repr

  let parse name =
    Toplevel.put last begin
      KB.Object.read Theory.Program.cls name
    end;
    Toplevel.get last

  let create () =
    Toplevel.put last begin
      KB.Object.create Theory.Program.cls
    end;
    Toplevel.get last

  let to_string : t -> string = fun tid ->
    Format.asprintf "%%%08Lx" (Int63.to_int64 (KB.Object.id tid))

  let of_string : string -> t = fun str ->
    if String.is_empty str
    then intern str
    else match str.[0] with
      | '%' -> parse @@ sprintf "#<%s 0x%s>"
          (KB.Name.show (KB.Class.name Theory.Semantics.cls))
          (String.subo ~pos:1 str)
      | '@' -> intern (String.subo ~pos:1 str)
      | _ -> intern str

  let nil = create ()

  let pp ppf tid = Format.fprintf ppf "%s" (to_string tid)

  let name t = match get_name t with
    | None -> to_string t
    | Some name -> sprintf "@%s" name

  let from_string_exn = of_string
  let from_string x = Ok (from_string_exn x)
  let (!!) = of_string
  include Regular.Make(struct
      type t = Theory.Label.t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Tid"
      let version = "2.0.0"
      let hash x = Int63.hash (KB.Object.id x)
      let pp = pp
      let to_string tid = to_string tid
    end)
end

type tid = Tid.t [@@deriving bin_io, compare, sexp]

type 'a term = {
  tid : tid;
  self : 'a;
  dict : dict;
} [@@deriving bin_io, compare, fields, sexp]

type label =
  | Direct of tid
  | Indirect of exp
[@@deriving bin_io, compare, sexp]

type call = {target : label; return : label option}
[@@deriving bin_io, compare, fields, sexp]

type jmp_kind =
  | Call of call
  | Goto of label
  | Ret  of label
  | Int  of int * tid
[@@deriving bin_io, compare, sexp]


type intent = In | Out | Both [@@deriving bin_io, compare, sexp]

module Rhs : sig
  type top = (Theory.Value.cls, unit) KB.cls
  type t = top Knowledge.value [@@deriving bin_io, compare, sexp]

  val empty : t
  val of_value : 'a Theory.value -> t
  val of_exp : exp -> t
  val with_exp : exp -> t -> t
  val exp : t -> exp

  include Base.Comparable.S with type t := t
end = struct
  type top = (Theory.Value.cls, unit) KB.cls
  let cls : top = Theory.Value.cls

  let forget v = KB.Value.refine v ()

  let empty = KB.Value.empty cls

  let of_value x = forget x

  let of_exp exp =
    KB.Value.put Exp.slot empty exp

  let with_exp exp x =
    KB.Value.put Exp.slot x exp

  let exp x = KB.Value.get Exp.slot x [@@inline]
  include (val KB.Value.derive cls)
end


let to_var v = Theory.Var.create (Var.sort v) (Var.ident v)

module Def : sig
  type t = {
    var : unit Theory.var;
    rhs : Rhs.t;
  } [@@deriving bin_io, compare, sexp]

  val reify : 'a Theory.var -> 'a Theory.value -> t

  val of_bil : var -> exp -> t
end = struct

  type t = {
    var : Theory.Var.Top.t;
    rhs : Rhs.t;
  } [@@deriving bin_io, compare, sexp]

  let reify lhs rhs = {
    var = Theory.Var.forget lhs;
    rhs = Rhs.of_value rhs;
  }

  let of_bil var exp = {
    var = to_var var;
    rhs = Rhs.of_exp exp;
  }
end

module Phi = struct
  type t = {
    var : Theory.Var.Top.t;
    map : Rhs.t Tid.Map.t;
  } [@@deriving bin_io, compare, sexp]
end

module Cnd : sig
  type t = Theory.Bool.t Theory.value
  [@@deriving bin_io, compare, sexp]

  val of_exp : exp -> t
  val exp : t -> exp

  include Base.Comparable.S with type t := t
end = struct

  let empty = Theory.Value.empty Theory.Bool.t
  let of_exp = KB.Value.put Exp.slot empty
  let exp = KB.Value.get Exp.slot
  let cls = KB.Class.refine Theory.Value.cls Theory.Bool.t
  include (val KB.Value.derive cls)
end

module Jmp = struct
  type dst = Resolved of tid
           | Indirect of {
               vec : Rhs.t;
               len : int;
             }
  [@@deriving bin_io, compare, sexp]
  type t = {
    cnd : Cnd.t option;
    dst : dst option;
    alt : dst option;
  } [@@deriving bin_io, compare, sexp]
end

type jmp = Jmp.t [@@deriving bin_io, compare, sexp]
type def = Def.t [@@deriving bin_io, compare, sexp]
type phi = Phi.t [@@deriving bin_io, compare, sexp]

type blk = {
  phis : phi term array;
  defs : def term array;
  jmps : jmp term array;
} [@@deriving bin_io, compare, fields, sexp]

type arg = Def.t
[@@deriving bin_io, compare, sexp]

type sub = {
  name : string;
  blks : blk term array;
  args : arg term array;
} [@@deriving bin_io, compare, fields, sexp]


type path = int array
[@@deriving bin_io, compare, sexp]

module Program : sig
  type t = private {
    subs  : sub term array;
    paths : path Tid.Table.t;
  } [@@deriving bin_io, compare, fields, sexp]

  val empty : unit -> t

  val update : t -> sub term array -> t

end = struct
  type t = {
    subs  : sub term array;
    paths : path Tid.Table.t;
  } [@@deriving bin_io, fields, sexp]

  let mangle_name addr tid name =
    match addr with
    | Some a ->
      sprintf "%s@%s" name @@
      Bap_bitvector.string_of_value ~hex:true a
    | None -> sprintf "%s%%%s" name (Tid.to_string tid)

  let mangle_sub s =
    let addr = Dict.find s.dict Bap_attributes.address in
    let name = mangle_name addr s.tid s.self.name in
    Tid.add_name s.tid s.self.name;
    Tid.set_name s.tid name;
    let self = {s.self with name} in
    {s with self}

  let fix_names news =
    let is_new sub =
      not @@ Tid.equal sub.tid (Tid.for_name sub.self.name) in
    let keep_name tids name tid = Map.set tids ~key:name ~data:tid in
    let tids = Array.fold news ~init:String.Map.empty ~f:(fun tids sub ->
        match Map.find tids sub.self.name with
        | None -> keep_name tids sub.self.name sub.tid
        | Some _ ->
          if is_new sub then tids
          else keep_name tids sub.self.name sub.tid) in
    if Array.length news = Map.length tids then news
    else
      Array.map news ~f:(fun sub ->
          let tid' = Map.find_exn tids sub.self.name in
          if Tid.equal tid' sub.tid then sub
          else mangle_sub sub)

  let empty () = {subs = [| |] ;  paths = Tid.Table.create () }

  let update p subs = { p with subs = fix_names subs }

  let compare x y =
    let compare x y = [%compare:sub term array] x y in
    compare x.subs y.subs
end

type program = Program.t [@@deriving bin_io,compare,sexp]
open Program

module Array = struct
  include Array
  (** [insert xs x i] insert [x] into [xs] in a position before [i].
      [i] can be in a range of [0 <= i <= length xs]
  *)
  let insert xs x i =
    init (length xs + 1) ~f:(fun j ->
        if j < i then xs.(j) else
        if j = i then x
        else xs.(j - 1))

  let push_back xs x = insert xs x (length xs)
  let push_front xs x = insert xs x 0

  (** [remove_if xs ~f] returns array that doesn't contain elements
      for which [f x] is true.  Returns an unmodified array if no such
      elements are in the array.  *)
  let remove_if xs ~f:is_victim =
    if Array.exists xs ~f:is_victim
    then Array.filter xs ~f:(Fn.non is_victim)
    else xs

  let update_if xs y ~f:is_target =
    if Array.exists xs ~f:is_target
    then Array.map xs ~f:(fun x -> if is_target x then y else x)
    else xs


  let pp ppx ppf xs =
    let last = Array.length xs - 1 in
    Array.iteri xs ~f:(fun i x ->
        Format.fprintf ppf "%a" ppx x;
        if i <> last
        then Format.pp_print_cut ppf ())
end


let always = Bap_bil.Exp.Int Bitvector.b1
let never  = Bap_bil.Exp.Int Bitvector.b0
let undefined_exp = Bap_exp.Exp.unknown "undefined" Bap_type.Export.bool_t
let undefined_var = Bap_var.create "undefined" Bap_type.Export.bool_t

let pp_attr ppf attr =
  Format.fprintf ppf "@[.%s %a@]"
    (Value.tagname attr) Value.pp attr

let pp_attrs ppf dict =
  Dict.data dict |> Seq.iter ~f:(pp_attr ppf)

type nil [@@deriving bin_io, compare, sexp]

type _ typ =
  | Nil : nil typ
  | Top : program typ
  | Sub : sub typ
  | Arg : arg typ
  | Blk : blk typ
  | Phi : phi typ
  | Def : def typ
  | Jmp : jmp typ

type ('a,'b) cls = {
  par : 'a typ;
  typ : 'b typ;
  nil : 'b term;
  set : 'a -> 'b term array -> 'a;
  get : 'a -> 'b term array;
}

let string_of_typ : type a . a typ -> string = function
  | Nil -> "nil"
  | Top -> "top"
  | Sub -> "sub"
  | Arg -> "arg"
  | Blk -> "blk"
  | Phi -> "phi"
  | Def -> "def"
  | Jmp -> "jmp"


let cls typ par nil field = {
  par;
  typ;
  nil;
  set = Field.fset field;
  get = Field.get field;
}



let hash_of_term t = Tid.hash (tid t)
let make_term tid self : 'a term = {
  tid; self; dict = Dict.empty;
}

let nil_top = make_term Tid.nil (Program.empty ())

let program_t = {
  par = Nil;
  typ = Top;
  nil = nil_top;
  set = (fun _ _ -> assert false);
  get = (fun _ -> assert false);
}

module Void : sig
  type t
  val t : t Theory.Value.sort
end = struct
  let unsorted = Theory.Value.Sort.Name.declare ~package "Void"
  type unsorted
  type t = unsorted Theory.Value.Sort.sym
  let t = Theory.Value.Sort.sym unsorted
end

let undefined_variable =
  Theory.Var.(forget @@ define Void.t "undefined")

let undefined_semantics =
  Rhs.of_value @@ Theory.Value.empty Void.t

let empty self = {
  tid = Tid.nil;
  dict = Value.Dict.empty;
  self
}

let nil_def : def term = empty Def.{
    var = undefined_variable;
    rhs = undefined_semantics;
  }

let nil_phi : phi term = empty Phi.{
    var = undefined_variable;
    map = Map.empty (module Tid);
  }

let nil_jmp : jmp term = empty Jmp.{
    cnd = None;
    dst = None;
    alt = None;
  }

let nil_blk : blk term =
  make_term Tid.nil {phis= [| |] ; defs = [| |] ; jmps = [| |] }

let nil_arg : arg term = nil_def

let nil_sub : sub term =
  make_term Tid.nil { name = "undefined"; blks = [| |] ; args = [| |]}

let def_t : (blk,def) cls = cls Def Blk nil_def Fields_of_blk.defs
let phi_t : (blk,phi) cls = cls Phi Blk nil_phi Fields_of_blk.phis
let jmp_t : (blk,jmp) cls = cls Jmp Blk nil_jmp Fields_of_blk.jmps
let blk_t : (sub,blk) cls = cls Blk Sub nil_blk Fields_of_sub.blks
let arg_t : (sub,arg) cls = cls Arg Sub nil_arg Fields_of_sub.args

let sub_t : (program, sub) cls = {
  par = Top;
  typ = Sub;
  nil = nil_sub;
  set = Program.update;
  get = Program.subs;
}

let horizontal ppf = Format.pp_open_box ppf 10
let vertical ppf = Format.pp_open_vbox ppf 0

let term_pp ?(no_cut=false) ?(box=horizontal) pp_self ppf t =
  let open Format in
  let attrs = Dict.data t.dict in
  Seq.iter attrs ~f:(fun attr ->
      pp_open_tag ppf (asprintf "%a" pp_attr attr));
  let pp_cut ppf = if not no_cut
    then Format.pp_print_break ppf 1 0
    else fprintf ppf " " in
  box ppf;
  fprintf ppf "%08Lx:%t%a" (Int63.to_int64 (KB.Object.id t.tid))
    pp_cut
    pp_self t.self;
  Format.pp_close_box ppf ();
  Seq.iter attrs ~f:(fun _ -> pp_close_tag ppf ())
[@@warning "-D"] (* for open and close tag *)

let pp_value slots ppf x =
  match slots with
  | [] -> KB.Value.pp ppf x
  | slots -> KB.Value.pp_slots slots ppf x

module Label = struct
  type t = label
  let direct x = Direct x
  let indirect x = Indirect x
  let create () = direct (Tid.create ())
  let change ?(direct=ident) ?(indirect=ident) label =
    match label with
    | Direct x -> Direct (direct x)
    | Indirect x -> Indirect (indirect x)

  include Regular.Make(struct
      type t = label [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Label"
      let version = "1.0.0"

      let hash = Hashtbl.hash
      let pp ppf = function
        | Indirect exp -> Bap_exp.pp ppf exp
        | Direct tid -> Format.fprintf ppf "%s" @@
          Tid.name tid
    end)
end

module Call = struct
  type t = call
  let create ?return ~target ()  = {target; return}
  let return t = t.return
  let target t = t.target
  let with_return t return = { t with return = Some return }
  let with_target t target = { t with target }
  let with_noreturn t = {t with return = None}

  include Regular.Make(struct
      type t = call [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Call"
      let version = "1.0.0"


      let pp_return ppf lab = match lab with
        | Some label ->
          Format.fprintf ppf "with return %a" Label.pp label
        | None -> Format.fprintf ppf "with noreturn"

      let pp ppf c =
        Format.fprintf ppf "call %a %a"
          Label.pp c.target pp_return c.return

      let hash = Hashtbl.hash
    end)
end



module Ir_arg = struct
  type t = arg term

  module Intent = struct
    module T = struct
      type t = intent option [@@deriving bin_io]
    end

    let equal_intent x y = compare_intent x y  =  0

    let domain =
      KB.Domain.optional ~equal:equal_intent ~inspect:sexp_of_intent "intent"
    let persistent = KB.Persistent.of_binable (module T)
    let slot = KB.Class.property Theory.Value.cls "arg-intent" domain
        ~package ~persistent

    let set intent x = match intent with
      | None -> x
      | Some intent -> KB.Value.put slot x intent
    let get x = KB.Value.get slot x
  end

  let set_intent ({self={Def.rhs} as self} as t) intent : t = {
    t with self = {
      self with rhs = Intent.set (Some intent) rhs
    }
  }

  let var {self={Def.var}} = var
  let value {self={Def.var; rhs}} =
    let sort = Theory.Var.sort var in
    KB.Value.refine rhs sort

  let with_intent arg intent = set_intent arg (Some intent)

  let reify ?(tid=Tid.create()) ?intent lhs rhs =
    set_intent (make_term tid @@ Def.reify lhs rhs) intent

  let create ?(tid=Tid.create()) ?intent var exp : t =
    set_intent (make_term tid @@ Def.of_bil var exp) intent
  let lhs    {self={Def.var}} = Var.reify var
  let rhs    {self={Def.rhs}} = Rhs.exp rhs
  let intent {self={Def.rhs}} = KB.Value.get Intent.slot rhs

  let with_unknown_intent t : t = set_intent t None

  let name arg = Var.name (lhs arg)

  let with_rhs ({self={Def.var}} as t) rhs = {
    t with self = Def.{var; rhs}
  }

  let string_of_intent = function
    | Some In -> "in "
    | Some Out -> "out "
    | Some Both -> "in out "
    | None -> ""

  let warn_unused = Bap_value.Tag.register (module Unit)
      ~package
      ~name:"warn-unused"
      ~uuid:"7aa17c89-cc9b-4ed2-8700-620cb9e09491"

  let format = Bap_value.Tag.register (module String)
      ~package
      ~name:"format"
      ~uuid:"d864c411-73eb-48b2-a7e9-33b51fa540c9"

  let alloc_size = Bap_value.Tag.register (module Unit)
      ~package
      ~name:"alloc-size"
      ~uuid:"b29905b3-4fb5-486e-8064-9b63cadc6174"

  let restricted = Bap_value.Tag.register (module Unit)
      ~package
      ~name:"restricted"
      ~uuid:"5ee30262-aed9-4aa8-a8e3-34a061104420"

  let nonnull = Bap_value.Tag.register (module Unit)
      ~package
      ~name:"nonnull"
      ~uuid:"3c0a6181-9a9c-4cf4-aa37-8ceebd773952"

  let pp_sort ppf var = match Var.typ (Var.reify var) with
    | Unk -> Theory.Value.Sort.pp ppf (Theory.Var.sort var)
    | typ -> Bap_type.pp ppf typ

  let pp_self pp_rhs ppf {Def.var; rhs} =
    Format.fprintf ppf "%s :: %s%a = %a"
      (Theory.Var.name var)
      (string_of_intent @@ Intent.get rhs)
      pp_sort var
      pp_rhs rhs

  let pp ppf arg =
    term_pp (pp_self (fun ppf rhs ->
        let exp = Rhs.exp rhs in
        Bap_exp.pp ppf exp)) ppf arg

  let pp_slots slots =
    term_pp (pp_self (pp_value slots))


  module V2 = struct
    type t = arg term [@@deriving bin_io, compare, sexp]
    let module_name = Some "Bap.Std.Arg"
    let version = "2.0.0"

    let hash = hash_of_term
    let pp = pp
  end
  include Regular.Make(V2)
end

module Ir_def = struct
  type t = def term

  let reify ?(tid=Tid.create()) lhs rhs =
    make_term tid @@ Def.reify lhs rhs

  let create ?(tid=Tid.create ()) var exp =
    make_term tid @@ Def.of_bil var exp

  let var {self={Def.var}} = var
  let value {self={Def.var; rhs}} =
    let sort = Theory.Var.sort var in
    KB.Value.refine rhs sort

  let lhs {self={Def.var}} = Var.reify var
  let rhs {self={Def.rhs}} = Rhs.exp rhs

  let with_lhs ({self={Def.rhs}} as t ) v = {
    t with self = Def.{
      var = to_var v;
      rhs;
    }
  }

  let with_rhs ({self={Def.var; rhs}} as t) exp = {
    t with self = Def.{
      var;
      rhs = Rhs.with_exp exp rhs
    }
  }

  let map_exp def ~f : def term =
    with_rhs def (f (rhs def))

  let substitute def x y = map_exp def ~f:(Exp.substitute x y)

  let free_vars def = Exp.free_vars (rhs def)

  let pp_self ppf {Def.var; rhs} =
    Format.fprintf ppf
      "%s@ :=@ %a" (Theory.Var.name var) Bap_exp.pp (Rhs.exp rhs)


  let pp_self_slots slots ppf {Def.var; rhs} =
    Format.fprintf ppf
      "%s@ :=@ %a" (Theory.Var.name var) (pp_value slots) rhs

  let pp ppf x = term_pp pp_self ppf x
  let pp_slots ds = term_pp (pp_self_slots ds)

  module V2 = struct
    type t = def term [@@deriving bin_io, compare, sexp]
    let module_name = Some "Bap.Std.Def"
    let version = "2.0.0"
    let hash = hash_of_term
    let pp = pp
  end
  include Regular.Make(V2)
end

module Ir_phi = struct
  type t = phi term

  let var {self={Phi.var}} = var
  let lhs phi = Var.reify (var phi)

  let with_lhs ({self} as t) lhs = {
    t with self = Phi.{
      self with var = to_var lhs;
    }
  }

  let reify ?(tid=Tid.create ()) var bs =
    let bs = List.map bs ~f:(fun (t,x) -> t, Rhs.of_value x) in
    make_term tid Phi.{
        var = Theory.Var.forget var;
        map = Map.of_alist_exn (module Tid) bs
      }

  let of_list ?(tid=Tid.create()) var bs : phi term =
    let bs = List.map bs ~f:(fun (t,x) -> t, Rhs.of_exp x) in
    make_term tid Phi.{
        var = to_var var;
        map = Map.of_alist_exn (module Tid) bs
      }

  let create ?tid var src exp : phi term =
    of_list ?tid var [src,exp]

  let values {self={Phi.map}} : (tid * exp) Seq.t =
    Map.to_sequence map |>
    Seq.map ~f:(fun (t,x) -> t, Rhs.exp x)

  let options {self={Phi.map; var}} : (tid * _) Seq.t =
    let sort = Theory.Var.sort var in
    Map.to_sequence map |>
    Seq.map ~f:(fun (t,x) -> t, KB.Value.refine x sort)


  let update ({self={Phi.map; var}} as t) tid exp : phi term = {
    t with self = Phi.{
      var;
      map = Map.set map ~key:tid ~data:(Rhs.of_exp exp)
    }
  }

  let remove ({self={Phi.map; var}} as t) tid : phi term = {
    t with self = Phi.{
      var;
      map = Map.remove map tid
    }
  }

  let select {self={Phi.map}} tid : exp option =
    Option.map (Map.find map tid) ~f:Rhs.exp

  let select_or_unknown phi tid = match select phi tid with
    | Some thing -> thing
    | None ->
      let name = Format.asprintf "unresolved-tid %a" Tid.pp tid in
      let typ = Var.typ (lhs phi) in
      Exp.unknown name typ

  let map_exp ({self={Phi.var; map}} as t) ~f : phi term = {
    t with
    self = {
      var;
      map = Map.map map ~f:(fun rhs -> Rhs.with_exp (f (Rhs.exp rhs)) rhs)
    }
  }

  let substitute phi x y = map_exp phi ~f:(Exp.substitute x y)

  let free_vars phi =
    values phi |> Seq.fold ~init:Bap_var.Set.empty ~f:(fun vars (_,e) ->
        Set.union vars (Exp.free_vars e))

  let pp_self ppf {Phi.var; map} =
    Format.fprintf ppf "%s@ <-@ phi(%s)"
      (Theory.Var.name var)
      (String.concat ~sep:", " @@
       List.map ~f:(fun (id,exp) ->
           let exp = Rhs.exp exp in
           Format.asprintf "[%a, %a]" Bap_exp.pp exp Tid.pp id)
         (Map.to_alist map))

  let pp_self_slots ds ppf {Phi.var; map} =
    Format.fprintf ppf "%s@ <-@ phi(%s)"
      (Theory.Var.name var)
      (String.concat ~sep:", " @@
       List.map ~f:(fun (id,exp) ->
           Format.asprintf "[%a, %%%a]" (pp_value ds) exp Tid.pp id)
         (Map.to_alist map))

  let pp = term_pp pp_self
  let pp_slots ds = term_pp (pp_self_slots ds)

  module V2 = struct
    type t = phi term [@@deriving bin_io, compare, sexp]
    let module_name = Some "Bap.Std.Phi"
    let version = "2.0.0"
    let pp = pp
    let hash = hash_of_term
  end
  include Regular.Make(V2)
end

module Ir_jmp = struct
  type t = jmp term
  type dst = Jmp.dst

  let resolved tid = Jmp.Resolved tid
  let indirect dst = Jmp.Indirect {
      vec = Rhs.of_value dst;
      len = Theory.Bitv.size (KB.Class.sort (KB.Value.cls dst));
    }

  let reify ?(tid=Tid.create ()) ?cnd ?alt ?dst () =
    make_term tid Jmp.{cnd; dst; alt}

  let dst_of_lbl : label -> Jmp.dst option = function
    | Direct tid -> Some (Resolved tid)
    | Indirect exp -> match Bap_helpers.Type.infer_exn exp with
      | Imm len -> Some (Indirect {vec = Rhs.of_exp exp; len})
      | _ -> None

  let lbl_of_dst : Jmp.dst -> label = function
    | Resolved tid -> Direct tid
    | Indirect {vec} -> Indirect (Rhs.exp vec)

  let create ?(tid=Tid.create()) ?(cond=always) kind =
    let cnd = if Exp.equal cond always then None else Some (Cnd.of_exp cond) in
    make_term tid @@ match kind with
    | Goto lbl -> Jmp.{
        cnd;
        dst = dst_of_lbl lbl; alt = None;
      }
    | Ret lbl -> Jmp.{
        cnd;
        dst = None; alt = dst_of_lbl lbl
      }
    | Int (int,ret) ->
      let alt = Tid.create () in
      Tid.set_ivec alt int;
      Jmp.{
        cnd;
        dst = Some (Resolved ret);
        alt = Some (Resolved alt);
      }
    | Call t -> {
        cnd;
        dst = Option.bind ~f:dst_of_lbl (Call.return t);
        alt = dst_of_lbl (Call.target t);
      }

  let ivec_of_dst : Jmp.dst -> int option = function
    | Indirect _ -> None
    | Resolved t -> Tid.get_ivec t

  let kind_of_jmp {Jmp.dst; alt} =
    match dst, alt with
    | None, None -> Goto (Indirect (Exp.unknown "unknown" Unk))
    | Some dst, None -> Goto (lbl_of_dst dst)
    | None, Some alt -> Call (Call.create ~target:(lbl_of_dst alt) ())
    | Some dst, Some alt -> match dst, ivec_of_dst alt  with
      | Resolved dst, Some vec -> Int (vec,dst)
      | _ -> Call (Call.create ()
                     ~return:(lbl_of_dst dst)
                     ~target:(lbl_of_dst alt))

  let create_call ?tid ?cond call = create ?tid ?cond (Call call)
  let create_goto ?tid ?cond dest = create ?tid ?cond (Goto dest)
  let create_ret  ?tid ?cond dest = create ?tid ?cond (Ret dest)
  let create_int  ?tid ?cond n t  = create ?tid ?cond (Int (n,t))

  let guard {self={Jmp.cnd}} = cnd
  let with_guard jmp cnd = {jmp with self = Jmp.{
      jmp.self with cnd
    }}

  let dst {self={Jmp.dst}} = dst
  let alt {self={Jmp.alt}} = alt

  let with_dst jmp dst = {jmp with self = Jmp.{
      jmp.self with dst
    }}

  let with_alt jmp alt = {jmp with self = Jmp.{
      jmp.self with alt
    }}

  let resolve = function
    | Jmp.Resolved t -> Either.first t
    | Jmp.Indirect {vec; len} ->
      let s = Theory.Bitv.define len in
      Either.second (KB.Value.refine vec s)

  let kind : jmp term -> jmp_kind = fun t ->
    kind_of_jmp t.self

  let cond_of_jmp {Jmp.cnd} = match cnd with
    | None -> always
    | Some cnd -> KB.Value.get Exp.slot cnd


  let cond : jmp term -> exp = fun t -> cond_of_jmp t.self

  let with_cond t cnd = {
    t with self = Jmp.{
      t.self with cnd = Some (Cnd.of_exp cnd)
    }
  }

  let with_kind t kind =
    let t' = create ~tid:t.tid ~cond:(cond t) kind in
    { t' with dict = t.dict }

  let exps (jmp : jmp term) : exp Sequence.t =
    let open Sequence.Generator in
    let label label = match label with
      | Indirect exp -> yield exp
      | Direct _ -> return () in
    let call call =
      Option.value_map ~default:(return ())
        (Call.return call) ~f:label >>= fun () ->
      label (Call.target call) in
    let r = match kind jmp with
      | Call t -> call  t
      | Goto t | Ret  t -> label t
      | _ -> return () in
    run (r >>= fun () -> yield (cond jmp))

  let map_exp (jmp : jmp term) ~f : jmp term =
    let map_label label = match label with
      | Indirect exp -> Label.indirect (f exp)
      | Direct _ -> label in
    let map_call call : call =
      let return = Option.map (Call.return call) ~f:map_label in
      let target = map_label (Call.target call) in
      Call.create ?return ~target () in
    let jmp : jmp term = with_cond jmp (f (cond jmp)) in
    let kind = match kind jmp with
      | Call t -> Call (map_call  t)
      | Goto t -> Goto (map_label t)
      | Ret  t -> Ret  (map_label t)
      | Int (_,_) as kind -> kind in
    with_kind jmp kind

  let substitute jmp x y = map_exp jmp ~f:(Exp.substitute x y)

  let free_vars jmp =
    exps jmp |> Seq.fold ~init:Bap_var.Set.empty ~f:(fun vars e ->
        Set.union vars (Exp.free_vars e))

  let eval jmp bili =
    let eval_label = function
      | Indirect dst -> bili#eval_jmp (Stmt.jmp dst)
      | Direct _ -> assert false in
    match kind jmp with
    | Goto t -> eval_label t
    | _ -> assert false

  let pp_dst ppf = function
    | Goto dst -> Format.fprintf ppf "goto %a" Label.pp dst
    | Call sub -> Call.pp ppf sub
    | Ret  dst -> Format.fprintf ppf "return %a" Label.pp dst
    | Int (n,t) ->
      Format.fprintf ppf "interrupt 0x%X return %%%a" n Tid.pp t

  let pp_cond ppf cond =
    if Exp.(cond <> always) then
      Format.fprintf ppf "when %a " Bap_exp.pp cond

  let pp_self ppf jmp =
    Format.fprintf ppf "%a%a"
      pp_cond (cond_of_jmp jmp)
      pp_dst (kind_of_jmp jmp)

  let pp ppf x =
    term_pp pp_self ppf x

  let pp_slots _ = pp


  module V2 = struct
    type t = jmp term [@@deriving bin_io, compare, sexp]
    let module_name = Some "Bap.Std.Jmp"
    let version = "2.0.0"
    let hash = hash_of_term
    let pp = pp
  end

  include Regular.Make(V2)
end


module Term = struct
  type 'a t = 'a term

  module Fields = Fields_of_term

  let create self : 'a term = make_term (Tid.create ()) self
  let clone {self} = create self
  let same x y = Tid.equal x.tid y.tid
  let name b = Tid.name b.tid
  let tid x = x.tid
  let with_field field term x = {
    term with self=Field.fset field term.self x
  }

  let apply f t p = {p with self=f (t.get p.self) |> t.set p.self}

  let update t p y =
    apply (fun xs -> Array.update_if xs y ~f:(fun x -> Tid.(x.tid = y.tid))) t p

  let find t p tid =
    Array.find (t.get p.self) ~f:(fun x -> Tid.(x.tid = tid))

  let find_exn t p tid =
    Array.find_exn (t.get p.self) ~f:(fun x -> Tid.(x.tid = tid))

  let nth t p i =
    let xs = t.get p.self in
    if i < Array.length xs then Some xs.(i) else None

  let nth_exn t p i = (t.get p.self).(i)

  let remove t p tid =
    apply (Array.remove_if ~f:(fun x -> Tid.(x.tid = tid))) t p

  let to_seq xs =
    Seq.init (Array.length xs) ~f:(Array.unsafe_get xs)

  let to_seq_rev xs =
    let n = Array.length xs in
    Sequence.init n ~f:(fun i -> Array.unsafe_get xs (n - i - 1))

  let to_sequence ?(rev=false) t p =
    if rev then to_seq_rev (t.get p.self) else to_seq (t.get p.self)

  let enum = to_sequence

  let map t p ~f : 'a term = apply (Array.map ~f) t p

  let filter_map t p ~f : 'a term = apply (Array.filter_map ~f) t p

  let concat_map t p ~f =
    let concat_map xs =
      let vec = Vec.create ~capacity:(Array.length xs) t.nil in
      Array.iter xs ~f:(fun x -> List.iter (f x) ~f:(Vec.append vec));
      Vec.to_array vec in
    apply concat_map t p

  let filter t p ~f = apply (Array.filter ~f) t p
  let findi t p tid =
    Array.findi (t.get p.self) ~f:(fun _i x -> Tid.(x.tid = tid))

  let next t p tid =
    let open Option.Monad_infix in
    let ts = t.get p.self in findi t p tid >>= fun (i,_) ->
    if (i+1) >= Array.length ts then None else Some (ts.(i+1))

  let prev t p tid =
    let open Option.Monad_infix in
    let ts = t.get p.self in findi t p tid >>= fun (i,_) ->
    if i = 0 then None else Some (ts.(i-1))

  let first t p : 'a term option =
    let xs = t.get p.self in
    if Array.is_empty xs then None else (Some xs.(0))

  let last t p : 'a term option =
    let xs = t.get p.self in
    let n = Array.length xs in
    if n = 0 then None else Some (xs.(n-1))

  let after ?(rev=false) t p tid =
    let run,cut = if rev then Seq.take_while,0 else Seq.drop_while,1 in
    Seq.(drop_eagerly (to_sequence ~rev t p |> run ~f:(fun x -> Tid.(x.tid <> tid))) cut)

  let before ?(rev=false) t p tid =
    let run,cut = if rev then Seq.drop_while,1 else Seq.take_while,0 in
    Seq.(drop_eagerly (to_sequence ~rev t p |> run ~f:(fun x -> Tid.(x.tid <> tid))) cut)

  let before_or_after run ?rev t p tid =
    if Array.exists (t.get p.self) ~f:(fun x -> Tid.(x.tid = tid))
    then run ?rev t p tid
    else Seq.empty

  let before t ?rev = before_or_after before ?rev t
  let after t ?rev  = before_or_after after ?rev t

  let insert t where p x =
    let xs = t.get p.self in
    let xs = match where with
      | `after None -> Array.insert xs x (Array.length xs)
      | `before None -> Array.insert xs x 0
      | `after (Some this) | `before (Some this) ->
        match Array.findi xs ~f:(fun _ x -> Tid.(x.tid = this)), where with
        | None,_ -> xs
        | Some (i,_),`before _ -> Array.insert xs x i
        | Some (i,_),`after _ -> Array.insert xs x (i+1) in
    {p with self = t.set p.self xs}

  let prepend t ?before:id = insert t (`before id)
  let append t ?after:id p c = insert t (`after id) p c
  let set_attr t tag x = {t with dict = Dict.set t.dict tag x}
  let attrs t = t.dict
  let get_attr t = Dict.find t.dict
  let del_attr t tag = {t with dict = Dict.remove t.dict tag}
  let has_attr t tag = Option.is_some @@ get_attr t tag
  let with_attrs t dict = {t with dict}

  let length t p = Array.length (t.get p.self)

  let origin = Bap_value.Tag.register (module Tid)
      ~package
      ~public:true
      ~desc:"denotes an origin of a synthetic program"
      ~name:"origin"
      ~uuid:"fa804594-d2fc-4865-824a-3ad481963f54"

  let synthetic = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"denotes a synthetic program"
      ~name:"synthetic"
      ~uuid:"a83b26b5-902e-4aaf-bfa1-503e3ced0b1a"


  let live = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"marks program as live"
      ~name:"live"
      ~uuid:"4d3871ab-9481-4d41-97a3-cd3136acfa90"

  let dead = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"marks program as dead"
      ~name:"dead"
      ~uuid:"6009fb21-2a6c-4511-9aa4-92b2894debc7"

  let visited = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"marks program as dead"
      ~name:"visited"
      ~uuid:"0e162aa3-153f-44a3-88e7-6e42d876e760"

  let precondition = Bap_value.Tag.register (module Exp)
      ~package
      ~public:true
      ~desc:"the program precondition"
      ~name:"precondition"
      ~uuid:"f08c88e1-56f5-4148-822a-ac2dff34bda5"

  let invariant = Bap_value.Tag.register (module Exp)
      ~package
      ~public:true
      ~desc:"the program invariant"
      ~name:"invariant"
      ~uuid:"743d712b-7ee4-46da-b3b7-98d3ca5e618b"

  let postcondition = Bap_value.Tag.register (module Exp)
      ~package
      ~public:true
      ~desc:"the program postcondition"
      ~name:"postcondition"
      ~uuid:"f248e4c1-9efc-4c70-a864-e34706e2082b"

  let equal x y =
    compare_term compare_blk x y = 0

  let equal_tids x y = Tid.equal (tid x) (tid y)

  let domain = Knowledge.Domain.flat ~empty:[] "bir"
      ~equal:(List.equal equal_tids)
      ~inspect:(fun blks -> Sexp.List (List.map blks ~f:(fun b ->
          Sexp.Atom (name b))))

  let persistent = Knowledge.Persistent.of_binable (module struct
      type t = blk term list [@@deriving bin_io]
    end)

  let slot = Knowledge.Class.property
      Theory.Semantics.cls "bir" domain
      ~package ~persistent
      ~public:true
      ~desc:"BIL semantics in a graphical IR"


  let change t p tid f =
    Array.findi (t.get p.self) ~f:(fun _ x -> Tid.(x.tid = tid)) |> function
    | None -> Option.value_map (f None) ~f:(append t p) ~default:p
    | Some (i,x) -> match f (Some x) with
      | None -> remove t p tid
      | Some c ->
        let xs = Array.mapi (t.get p.self) ~f:(fun n x ->
            if i = n then c else x) in
        {p with self = t.set p.self xs}

  let pp = term_pp

  type ('a,'b) cata = 'a term -> 'b

  let this x _t = x

  let cata (type t) (cls : (_,t) cls)
      ~init:default
      ?(program : (program,'a) cata = this default)
      ?(sub : (sub,'a) cata = this default)
      ?(arg : (arg,'a) cata = this default)
      ?(blk : (blk,'a) cata = this default)
      ?(phi : (phi,'a) cata = this default)
      ?(def : (def,'a) cata = this default)
      ?(jmp : (jmp,'a) cata = this default)
      (t : t term) : 'a = match cls.typ with
    | Nil -> assert false
    | Top -> program t
    | Sub -> sub t
    | Arg -> arg t
    | Blk -> blk t
    | Phi -> phi t
    | Def -> def t
    | Jmp -> jmp t

  let match_failure _ = raise (Match_failure ("",0,0))
  type ('a,'b) case = 'a -> 'b

  let switch (type t)
      (cls : (_,t) cls)
      ~(program : (program,'a) cata)
      ~(sub : (sub,'a) cata)
      ~(arg : (arg,'a) cata)
      ~(blk : (blk,'a) cata)
      ~(phi : (phi,'a) cata)
      ~(def : (def,'a) cata)
      ~(jmp : (jmp,'a) cata)
      (t : t term) : 'a = match cls.typ with
    | Nil -> assert false
    | Top -> program t
    | Sub -> sub t
    | Arg -> arg t
    | Blk -> blk t
    | Phi -> phi t
    | Def -> def t
    | Jmp -> jmp t


  type ('a,'b) proj = 'a term -> 'b option

  let nothing _ = None

  let proj cls ?program ?sub ?arg ?blk ?phi ?def ?jmp t =
    cata ~init:None ?program ?sub ?arg ?blk ?phi ?def ?jmp cls t

  type 'a map = 'a term -> 'a term

  let map_term (type t) (cls : (_,t) cls)
      ?(program : program map = ident)
      ?(sub : sub map = ident)
      ?(arg : arg map = ident)
      ?(blk : blk map = ident)
      ?(phi : phi map = ident)
      ?(def : def map = ident)
      ?(jmp : jmp map = ident)
      (t : t term) : t term = match cls.typ with
    | Nil -> assert false
    | Top -> program t
    | Sub -> sub t
    | Arg -> arg t
    | Blk -> blk t
    | Phi -> phi t
    | Def -> def t
    | Jmp -> jmp t


  let map1 (x,y,z) ~f = (f x,y,z)
  let map2 (x,y,z) ~f = (x,f y,z)

  class mapper = object(self)
    inherit Bil.exp_mapper
    method map_term : 't 'p. ('p,'t) cls -> 't term -> 't term =
      fun cls t -> map_term cls t
          ~program:self#run
          ~sub:self#map_sub
          ~arg:self#map_arg
          ~blk:self#map_blk
          ~phi:self#map_phi
          ~def:self#map_def
          ~jmp:self#map_jmp

    method run p = map sub_t ~f:(fun t -> self#map_term sub_t t) p
    method map_sub sub = map arg_t ~f:(self#map_term arg_t) sub |>
                         map blk_t ~f:(self#map_term blk_t)
    method map_blk blk = map phi_t ~f:(self#map_term phi_t) blk |>
                         map def_t ~f:(self#map_term def_t) |>
                         map jmp_t ~f:(self#map_term jmp_t)


    method private map_assn ({self=Def.{var;rhs}} as t) = {
      t with
      self = Def.{
          var = to_var (self#map_sym @@ Var.reify var);
          rhs = Rhs.with_exp (self#map_exp (Rhs.exp rhs)) rhs;
        }
    }

    method map_def = self#map_assn
    method map_arg = self#map_assn

    method map_phi phi =
      let phi = Ir_phi.with_lhs phi @@ self#map_sym (Ir_phi.lhs phi) in
      Ir_phi.map_exp phi ~f:self#map_exp

    method map_jmp jmp = Ir_jmp.map_exp jmp ~f:self#map_exp
  end

  let visit cls ~f term init =
    enum cls term |> Seq.fold ~init ~f:(fun x t -> f t x)

  let fident _t x = x

  class ['a] visitor = object(self)
    inherit ['a] Bil.exp_visitor

    method enter_term : 't 'p. ('p,'t) cls -> 't term -> 'a -> 'a = fun _cls _t x -> x
    method leave_term : 't 'p. ('p,'t) cls -> 't term -> 'a -> 'a = fun _cls _t x -> x
    method visit_term : 't 'p. ('p,'t) cls -> 't term -> 'a -> 'a =
      fun cls t x ->
      let x = self#enter_term cls t x in
      switch cls t
        ~program:(fun t -> self#run t x)
        ~sub:(fun t -> self#visit_sub t x)
        ~arg:(fun t -> self#visit_arg t x)
        ~blk:(fun t -> self#visit_blk t x)
        ~phi:(fun t -> self#visit_phi t x)
        ~def:(fun t -> self#visit_def t x)
        ~jmp:(fun t -> self#visit_jmp t x) |>
      self#leave_term cls t

    method enter_program _p x = x
    method leave_program _p x = x

    method enter_sub _sub x = x
    method leave_sub _sub x = x

    method enter_blk _blk x = x
    method leave_blk _blk x = x

    method run p x =
      self#enter_program p x |>
      visit sub_t ~f:(fun t -> self#visit_term sub_t t) p |>
      self#leave_program p

    method visit_sub sub x =
      self#enter_sub sub x |>
      visit arg_t ~f:(self#visit_term arg_t) sub |>
      visit blk_t ~f:(self#visit_term blk_t) sub |>
      self#leave_sub sub

    method visit_blk blk x =
      self#enter_blk blk x |>
      visit phi_t ~f:(self#visit_term phi_t) blk |>
      visit def_t ~f:(self#visit_term def_t) blk |>
      visit jmp_t ~f:(self#visit_term jmp_t) blk |>
      self#leave_blk blk


    method enter_arg : arg term -> 'a -> 'a = fident
    method enter_phi : phi term -> 'a -> 'a = fident
    method enter_def : def term -> 'a -> 'a = fident
    method enter_jmp : jmp term -> 'a -> 'a = fident

    method leave_arg : arg term -> 'a -> 'a = fident
    method leave_phi : phi term -> 'a -> 'a = fident
    method leave_def : def term -> 'a -> 'a = fident
    method leave_jmp : jmp term -> 'a -> 'a = fident

    method visit_arg arg x =
      self#enter_arg arg x |>
      self#visit_var (Ir_arg.lhs arg) |>
      self#visit_exp (Ir_arg.rhs arg) |>
      self#leave_arg arg

    method visit_phi phi x =
      self#enter_phi phi x |>
      self#visit_var (Ir_phi.lhs phi) |> fun x ->
      Seq.fold (Ir_phi.values phi) ~init:x ~f:(fun data (_,x) ->
          self#visit_exp x data) |>
      self#leave_phi phi

    method visit_def def x =
      self#enter_def def x |>
      self#visit_var (Ir_def.lhs def) |>
      self#visit_exp (Ir_def.rhs def) |>
      self#leave_def def

    method visit_jmp jmp x =
      self#enter_jmp jmp x |> fun x ->
      Seq.fold (Ir_jmp.exps jmp) ~init:x ~f:(fun x e ->
          self#visit_exp e x) |>
      self#leave_jmp jmp
  end
end

module Ir_blk = struct
  type t = blk term
  type elt = [`Def of def term | `Phi of phi term | `Jmp of jmp term]

  module Fields = Fields_of_blk

  module Builder = struct
    type t = {
      b_tid : tid;
      b_defs : def term vector;
      b_phis : phi term vector;
      b_jmps : jmp term vector;
    }

    let create ?(tid=Tid.create()) ?(phis=16) ?(defs=16) ?(jmps=16) () = {
      b_phis = Vec.create ~capacity:phis nil_phi;
      b_defs = Vec.create ~capacity:defs nil_def;
      b_jmps = Vec.create ~capacity:jmps nil_jmp;
      b_tid  = tid
    }

    let add_def b = Vec.append b.b_defs
    let add_jmp b = Vec.append b.b_jmps
    let add_phi b = Vec.append b.b_phis
    let add_elt b = function
      | `Jmp j -> add_jmp b j
      | `Phi p -> add_phi b p
      | `Def d -> add_def b d

    let transfer term_type blk adder bld =
      Term.to_sequence term_type blk |> Seq.iter ~f:(adder bld)

    let init
        ?(same_tid=true)
        ?(copy_phis=false) ?(copy_defs=false) ?(copy_jmps=false) blk =
      let tid = if same_tid then Term.tid blk else Tid.create () in
      let b = create ~tid ()
          ~phis:(Term.length phi_t blk)
          ~defs:(Term.length def_t blk)
          ~jmps:(Term.length jmp_t blk) in
      if copy_phis then transfer phi_t blk add_phi b;
      if copy_defs then transfer def_t blk add_def b;
      if copy_jmps then transfer jmp_t blk add_jmp b;
      b

    let of_vec = Vec.to_array

    let result (b : t) : blk term = {
      tid = b.b_tid;
      dict = Dict.empty;
      self = {
        defs = of_vec b.b_defs;
        phis = of_vec b.b_phis;
        jmps = of_vec b.b_jmps;
      }
    }
  end

  let create ?(phis=[]) ?(defs=[]) ?(jmps=[])
      ?(tid=Tid.create ()) () : blk term =
    match phis,defs,jmps with
    | [],[],[] -> {
        tid;
        dict = Dict.empty;
        self = {
          phis = [| |] ;
          defs = [| |] ;
          jmps = [| |] ;
        }
      }
    | _ ->
      let b = Builder.create ~tid ()
          ~phis:(List.length phis)
          ~defs:(List.length defs)
          ~jmps:(List.length jmps) in
      List.iter phis ~f:(Builder.add_phi b);
      List.iter defs ~f:(Builder.add_def b);
      List.iter jmps ~f:(Builder.add_jmp b);
      Builder.result b


  (* splits [blk] at definition [i], with [i]'th definition in the
     second block *)
  let split_at i blk =
    let next_blk = Tid.create () in
    {
      tid = blk.tid;
      dict = Dict.empty;
      self = {
        phis = blk.self.phis;
        defs = Array.subo blk.self.defs ~len:i;
        jmps = [| Ir_jmp.create_goto (Label.direct next_blk) |]
      }
    }, {
      tid = next_blk;
      dict = Dict.empty;
      self = {
        phis = [| |] ;
        defs = Array.subo blk.self.defs ~pos:i;
        jmps = blk.self.jmps;
      }
    }

  let split_while blk ~f =
    let i = match  Array.findi blk.self.defs ~f:(fun _ d -> not(f d)) with
      | Some (i,_) -> i
      | None -> Array.length blk.self.defs in
    split_at i blk

  let split_top blk = split_at 0 blk
  let split_bot blk = split_at (Array.length blk.self.defs) blk
  let split_before blk def = split_while blk ~f:(fun {tid} -> Tid.(tid <> def.tid))
  let split_after blk def =
    let i = match Array.findi blk.self.defs
                    ~f:(fun _ {tid} -> Tid.(tid = def.tid)) with
    | Some (i,_) -> i + 1
    | None -> Array.length blk.self.defs in
    split_at i blk

  let try_get xs i = try Some (xs.(i)) with Invalid_argument _ -> None
  let nth_def {self} = try_get self.defs
  let first_def blk = nth_def blk 0
  let last_def blk = nth_def blk (Array.length blk.self.defs - 1)
  let nth_jmp {self} = try_get self.jmps
  let first_jmp blk = nth_jmp blk 0
  let last_jmp blk = nth_jmp blk (Array.length blk.self.jmps - 1)

  let elts ?(rev=false) blk =
    let open Seq.Infix in
    let seq = Term.to_sequence in
    if rev then
      Seq.(seq jmp_t ~rev blk >>| fun x -> `Jmp x) @
      Seq.(seq def_t ~rev blk >>| fun x -> `Def x) @
      Seq.(seq phi_t ~rev blk >>| fun x -> `Phi x)
    else
      Seq.(seq phi_t ~rev blk >>| fun x -> `Phi x) @
      Seq.(seq def_t ~rev blk >>| fun x -> `Def x) @
      Seq.(seq jmp_t ~rev blk >>| fun x -> `Jmp x)

  let apply_map name get map skip blk ~f =
    if List.mem ~equal:Poly.equal skip name
    then (get blk.self)
    else Array.map (get blk.self) ~f:(fun x -> map x ~f)

  let map_exp ?(skip=[]) blk ~f = {
    blk with self = {
      phis = apply_map `phi phis Ir_phi.map_exp skip blk ~f;
      defs = apply_map `def defs Ir_def.map_exp skip blk ~f;
      jmps = apply_map `jmp jmps Ir_jmp.map_exp skip blk ~f;
    }
  }

  let map_elts ?phi ?def ?jmp blk =
    let map get ~f = match f with
      | None -> get blk.self
      | Some f -> Array.map (get blk.self) ~f in {
      blk with self = {
        phis = map phis ~f:phi;
        defs = map defs ~f:def;
        jmps = map jmps ~f:jmp
      }
    }

  let substitute ?skip blk x y =
    map_exp ?skip  blk ~f:(Exp.substitute x y)

  let map_phi_lhs p ~f = Ir_phi.with_lhs p (f (Ir_phi.lhs p))
  let map_def_lhs d ~f = Ir_def.with_lhs d (f (Ir_def.lhs d))

  let map_lhs ?(skip=[]) blk ~f = {
    blk with self = {
      phis = apply_map `phi phis map_phi_lhs skip blk ~f;
      defs = apply_map `def defs map_def_lhs skip blk ~f;
      jmps = jmps blk.self;
    }
  }

  let has_lhs cls lhs blk x =
    Term.to_sequence cls blk |>
    Seq.exists ~f:(fun t -> Var.(lhs t = x))

  let defines_var blk x =
    has_lhs phi_t Ir_phi.lhs blk x  ||
    has_lhs def_t Ir_def.lhs blk x

  let free_vars blk =
    let (++) = Set.union and (--) = Set.diff in
    let init = Bap_var.Set.empty,Bap_var.Set.empty in
    fst @@ Seq.fold (elts blk) ~init ~f:(fun (vars,kill) -> function
        | `Phi phi -> vars, kill
        | `Def def ->
          Ir_def.free_vars def -- kill ++ vars,
          Set.add kill (Ir_def.lhs def)
        | `Jmp jmp ->
          Ir_jmp.free_vars jmp -- kill ++ vars, kill)

  let uses_var blk x = Set.mem (free_vars blk) x

  let find_var blk var =
    Term.to_sequence def_t blk ~rev:true |>
    Seq.find ~f:(fun d -> Var.equal (Ir_def.lhs d) var) |> function
    | Some def -> Some (`Def def)
    | None ->
      Term.to_sequence phi_t blk |>
      Seq.find ~f:(fun p -> Var.equal (Ir_phi.lhs p) var) |> function
      | Some phi -> Some (`Phi phi)
      | None -> None

  let occurs b ~after:dominator id =
    Tid.(dominator = id) ||
    Term.(after def_t b dominator |> Seq.exists ~f:(fun x -> Tid.(x.tid = id)))

  let pp_sep xs ys zs ppf =
    if Array.length xs <> 0 &&
       Array.length ys <> 0 ||
       Array.length xs <> 0 &&
       Array.length ys = 0 &&
       Array.length zs <> 0
    then Format.pp_print_cut ppf ()

  let pp_self ppf self =
    Format.fprintf ppf "%a%t%a%t%a"
      (Array.pp Ir_phi.pp) self.phis
      (pp_sep self.phis self.defs self.jmps)
      (Array.pp Ir_def.pp) self.defs
      (pp_sep self.defs self.jmps [||])
      (Array.pp Ir_jmp.pp) self.jmps

  let pp_self_slots ds ppf self =
    Format.fprintf ppf "%a%t%a%t%a"
      (Array.pp (Ir_phi.pp_slots ds)) self.phis
      (pp_sep self.phis self.defs self.jmps)
      (Array.pp (Ir_def.pp_slots ds)) self.defs
      (pp_sep self.defs self.jmps [||])
      (Array.pp (Ir_jmp.pp_slots ds)) self.jmps

  let box ppf = Format.pp_open_vbox ppf 0

  let pp = term_pp ~box pp_self

  let pp_slots ds ppf x =
    term_pp ~box (pp_self_slots ds) ppf x;



  include Regular.Make(struct
      type t = blk term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Blk"
      let version = "1.0.0"
      let hash = hash_of_term
      let pp = pp
    end)
end


module Ir_sub = struct
  type t = sub term

  let new_empty ?(tid=Tid.create ()) ?name () : t =
    let name = match name with
      | Some name -> name
      | None -> match Tid.get_name tid with
        | None -> Tid.to_string tid
        | Some name -> name in
    make_term tid {
      name;
      args = [| |] ;
      blks = [| |] ;
    }


  let name sub = sub.self.name
  let with_name sub name =
    Tid.add_name (Term.tid sub) name;
    {sub with self = {sub.self with name}}

  module Enum(T : Bap_value.S) = struct
    type t = T.t list [@@deriving bin_io, compare,sexp]
    let pp ppf xs =
      List.map xs ~f:(Format.asprintf "%a" T.pp) |>
      String.concat ~sep:", " |>
      Format.fprintf ppf "%s"
  end

  module Aliases = Enum(String)

  module Args = struct
    type t = (arg term, arg term * arg term) Either.t
    [@@deriving bin_io, compare, sexp]
    let pp ppf = function
      | First x -> Format.fprintf ppf "(%s)" (Ir_arg.name x)
      | Second (x,y) ->
        Format.fprintf ppf "(%s, %s)" (Ir_arg.name x) (Ir_arg.name y)
  end


  let aliases = Bap_value.Tag.register (module Aliases)
      ~package
      ~public:true
      ~desc:"alternative names of the program"
      ~name:"aliases"
      ~uuid:"ed73c040-d798-4fc9-96a0-d3c12a870955"

  let const = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is pure and closed term"
      ~name:"const"
      ~uuid:"b8795164-2e19-4469-9776-d41a6e6afe2e"

  let pure = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is pure"
      ~name:"pure"
      ~uuid:"2c477ecb-0de8-4e8e-8cba-052eb67c628a"

  let stub = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is a stub that redirects to the real program"
      ~name:"stub"
      ~uuid:"c9eaf8a5-783d-4e01-9bf2-5d012602475f"

  let extern = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is a part of the public interface"
      ~name:"extern"
      ~uuid:"7965a2ee-ae72-4eb6-88c1-6f2b6108915f"

  let leaf = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is a leaf function"
      ~name:"leaf"
      ~uuid:"0688a9ac-9e55-4e37-a9e5-ac0a4e4af59e"

  let malloc = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program returns value that is allocated with malloc"
      ~name:"malloc"
      ~uuid:"b9237d20-07c2-462f-938d-91e4d438fd07"

  let noreturn = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program terminates the computation"
      ~name:"noreturn"
      ~uuid:"1f2941fb-d227-418d-a1d5-9f9d288a585f"

  let nothrow = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the doesn't throw exceptions"
      ~name:"nothrow"
      ~uuid:"32058b19-95ca-46e3-bee0-d0a3694fd5b1"

  let returns_twice = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is non-determinstic"
      ~name:"returns-twice"
      ~uuid:"40166004-ea98-431b-81b0-4e74a0b681ee"

  let entry_point = Bap_value.Tag.register (module Unit)
      ~package
      ~public:true
      ~desc:"the program is the entry point to the compilation unit"
      ~name:"entry-point"
      ~uuid:"d1eaff96-4ed4-4405-9305-63508440ccc1"

  module Builder = struct
    type t =
      tid option * arg term vector * blk term vector * string option

    let create ?tid ?(args=4) ?(blks=16) ?name () : t =
      tid,
      Vec.create ~capacity:args nil_arg,
      Vec.create ~capacity:blks nil_blk,
      name

    let add_blk (_,_,bs,_) = Vec.append bs
    let add_arg (_,xs,_,_) = Vec.append xs

    let result (tid,args,blks,name) : sub term =
      let tid = match tid with
        | Some tid -> tid
        | None -> Tid.create () in
      let args = Vec.to_array args in
      let blks = Vec.to_array blks in
      let name = match name with
        | Some name -> name
        | None -> Format.asprintf "sub_%a" Tid.pp tid in
      make_term tid {name; args; blks}
  end

  let create ?(args=[]) ?(blks=[]) ?tid ?name () : t =
    match args,blks with
    | [],[] -> new_empty ?tid ?name ()
    | _ ->
      let b = Builder.create ?tid ?name ()
          ~args:(List.length args)
          ~blks:(List.length blks) in
      List.iter args ~f:(Builder.add_arg b);
      List.iter blks ~f:(Builder.add_blk b);
      Builder.result b

  let pp_blks pp ppf blks =
    Array.iter blks ~f:(Format.fprintf ppf "@;%a@;" pp)

  let pp_self ppf self =
    Format.fprintf ppf "sub %s(%s)@;%a@;%a"
      self.name
      (String.concat ~sep:", " @@
       Array.to_list @@
       Array.map self.args ~f:Ir_arg.name)
      (Array.pp Ir_arg.pp) self.args
      (pp_blks Ir_blk.pp) self.blks

  let pp_self_slots ds ppf self =
    Format.fprintf ppf "sub %s(%s)@;%a@;%a"
      self.name
      (String.concat ~sep:", " @@
       Array.to_list @@
       Array.map self.args ~f:Ir_arg.name)
      (Array.pp (Ir_arg.pp_slots ds)) self.args
      (pp_blks (Ir_blk.pp_slots ds)) self.blks

  let pp = term_pp pp_self ~no_cut:true ~box:vertical
  let pp_slots ds = term_pp (pp_self_slots ds)
      ~no_cut:true ~box:vertical

  include Regular.Make(struct
      type t = sub term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Sub"
      let version = "1.0.0"
      let pp = pp
      let hash = hash_of_term
    end)
end

module Ir_program = struct
  type t = program term


  let proj1 t cs = t.self.subs.(cs.(0))
  let proj2 f t cs = (f (proj1 t cs).self).(cs.(1))
  let proj3 f g t cs = (g (proj2 f t cs).self).(cs.(2))

  let def_of_path = proj3 blks defs
  let phi_of_path = proj3 blks phis
  let jmp_of_path = proj3 blks jmps
  let blk_of_path = proj2 blks
  let arg_of_path = proj2 args
  let sub_of_path = proj1

  let get_1st get prog tid : (path * 'a) option =
    with_return (fun {return} ->
        Array.iteri (get prog.self) ~f:(fun i x ->
            if Tid.(x.tid = tid) then return (Some ([|i|], x )));
        None)

  let get_2nd get {self} tid : (path * 'a) option =
    with_return (fun {return} ->
        Array.iteri self.subs ~f:(fun i sub ->
            Array.iteri (get sub.self) ~f:(fun j term ->
                if Tid.(term.tid = tid) then return (Some ([|i;j|],term))));
        None)

  let get_3rd get {self} tid : (path * 'a) option =
    with_return (fun {return} ->
        Array.iteri self.subs ~f:(fun i {self} ->
            Array.iteri self.blks ~f:(fun j blk ->
                Array.iteri (get blk.self) ~f:(fun k ent ->
                    if Tid.(ent.tid = tid)
                    then return (Some ([|i; j; k|], ent)))));
        None)

  type 'a getter = program term -> tid -> (path * 'a term) option
  let get_and_cache (get : 'a getter) prog tid : 'a term option =
    let open Option.Monad_infix in
    get prog tid >>= fun (path,term) ->
    Tid.Table.change prog.self.paths tid (fun _ -> Some path);
    Some term

  type 'a entry = {
    get : 'a getter;
    of_path : program term -> path -> 'a term;
  }

  let make_getter (term : 'a entry) prog (tid : tid) : 'a term option =
    let open Option.Monad_infix in
    match Tid.Table.find prog.self.paths tid with
    | None -> get_and_cache term.get prog tid
    | Some path ->
      try
        let thing = term.of_path prog path in
        if Tid.(thing.tid = tid) then Some thing
        else get_and_cache term.get prog tid
      with Invalid_argument _ -> get_and_cache term.get prog tid

  type 'a finder = program term -> tid -> 'a term option
  let finder level get of_path = make_getter {get = level get; of_path}

  let finder_of_type (type b) (t : b typ) : b finder = match t with
    | Def -> finder get_3rd defs def_of_path
    | Jmp -> finder get_3rd jmps jmp_of_path
    | Phi -> finder get_3rd phis phi_of_path
    | Blk -> finder get_2nd blks blk_of_path
    | Arg -> finder get_2nd args arg_of_path
    | Sub -> finder get_1st subs sub_of_path
    | Top -> (fun p tid -> Option.some_if Tid.(p.tid = tid) p)
    | Nil -> assert false

  let lookup t = finder_of_type t.typ

  let parent (type a) (t : (a,'b) cls) p tid : a term option =
    match lookup t p tid with
    | None -> None
    | Some _ ->
      let path = Tid.Table.find_exn p.self.paths tid in
      match t.par with
      | Blk -> Some (blk_of_path p path)
      | Arg -> Some (arg_of_path p path)
      | Sub -> Some (sub_of_path p path)
      | Top -> Some p
      | _ -> None

  module Builder = struct
    type t = tid option * sub term vector

    let create ?tid ?(subs=16) () : t =
      tid, Vec.create ~capacity:subs nil_sub

    let add_sub (_,subs) =
      Vec.append subs

    let result (tid,subs) : program term =
      let tid = match tid with
        | Some tid -> tid
        | None -> Tid.create () in
      let p = Program.empty () in
      make_term tid @@ Program.update p (Vec.to_array subs)
  end

  let create ?(subs=[]) ?(tid=Tid.create ()) () : t =
    match subs with
    | [] -> make_term tid (Program.empty ())
    | _ ->
      let b = Builder.create ~tid ()
          ~subs:(List.length subs) in
      List.iter subs ~f:(Builder.add_sub b);
      Builder.result b

  let pp_self ppf self =
    Format.fprintf ppf "program@;%a"
      (Array.pp Ir_sub.pp) self.subs

  let pp_self_slots ds ppf self =
    Format.fprintf ppf "program@;%a"
      (Array.pp (Ir_sub.pp_slots ds)) self.subs

  let pp_slots ds = term_pp (pp_self_slots ds)
      ~no_cut:true ~box:vertical
  let pp = term_pp pp_self ~no_cut:true ~box:vertical

  include Regular.Make(struct
      type t = program term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Program"
      let version = "1.0.0"
      let pp = pp
      let hash = hash_of_term
    end)
end
