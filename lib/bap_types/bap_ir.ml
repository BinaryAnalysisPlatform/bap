open Core_kernel.Std
open Regular.Std
open Bap_common
open Bap_bil

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
  exception Overrun
  type t = Int63.t [@@deriving bin_io, compare, sexp]

  module Tid_generator = Bap_state.Make(struct
      type t = Int63.t ref
      let create () = ref (Int63.zero)
    end)

  let create =
    fun () ->
      let last_tid = !Tid_generator.state in
      Int63.incr last_tid;
      if last_tid.contents = Int63.zero
      then raise Overrun;
      last_tid.contents

  let nil = Int63.zero
  module Tid = Regular.Make(struct
      type nonrec t = Int63.t [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Tid"
      let version = "1.0.0"

      let hash = Int63.hash

      let pp ppf tid =
        Format.fprintf ppf "%08Lx" (Int63.to_int64 tid)

      let to_string tid = Format.asprintf "%a" pp tid
    end)

  module Name_resolver = Bap_state.Make(struct
      type t = string Tid.Table.t
      let create () = Tid.Table.create ()
    end)

  let names = Name_resolver.state

  let rev_lookup name =
    Hashtbl.to_alist !names |> List.find_map ~f:(fun (tid,x) ->
        Option.some_if (x = name) tid) |> function
    | None -> invalid_argf "unbound name: %s" name ()
    | Some name -> name

  let from_string_exn str = match str.[0] with
    | '%' -> Scanf.sscanf str "%%%X" (Int63.of_int)
    | '@' -> Scanf.sscanf str "@%s" rev_lookup
    | _ -> invalid_arg "label should start from '%' or '@'"

  let from_string str = Or_error.try_with ~backtrace:true (fun () ->
      from_string_exn str)

  let set_name tid name =
    Hashtbl.set !names ~key:tid ~data:name

  let name tid = match Hashtbl.find !names tid with
    | None -> Format.asprintf "%%%a" Tid.pp tid
    | Some name -> sprintf "@%s" name

  module State = struct
    let set_name_resolver resolver = names := resolver
  end

  let (!!) = from_string_exn
  include Tid
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

type jmp = (exp * jmp_kind) [@@deriving bin_io, compare, sexp]
type def = (var * exp) [@@deriving bin_io, compare, sexp]
type phi = (var * exp Tid.Map.t) [@@deriving bin_io, compare, sexp]

type blk = {
  phis : phi term array;
  defs : def term array;
  jmps : jmp term array;
} [@@deriving bin_io, compare, fields, sexp]

type arg = var * exp * intent option
  [@@deriving bin_io, compare, sexp]

type sub = {
  name : string;
  blks : blk term array;
  args : arg term array;
} [@@deriving bin_io, compare, fields, sexp]


type path = int array
  [@@deriving bin_io, compare, sexp]


type program = {
  subs  : sub term array;
  paths : path Tid.Table.t;
} [@@deriving bin_io, fields, sexp]

let compare_program x y =
  let compare x y = [%compare:sub term array] x y in
  compare x.subs y.subs


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
    Array.iter xs ~f:(fun x -> Format.fprintf ppf "%a" ppx x)
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

module Leaf = struct
  let create ?(tid=Tid.create ()) lhs rhs = {
    tid;
    self = (lhs,rhs);
    dict = Value.Dict.empty;
  }

  let make tid exp dst = {
    tid; self = (exp,dst); dict = Value.Dict.empty
  }

  let lhs {self=(x,_)} = x
  let rhs {self=(_,x)} = x
  let with_lhs def lhs = {def with self = (lhs, snd def.self)}
  let with_rhs def rhs = {def with self = (fst def.self, rhs)}
end

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
let make_term tid self : 'a term = {tid; self; dict = Dict.empty}

let nil_top = make_term Tid.nil {
    subs = [| |] ; paths = Tid.Table.create ();
  }

let program_t = {
  par = Nil;
  typ = Top;
  nil = nil_top;
  set = (fun _ _ -> assert false);
  get = (fun _ -> assert false);
}

let nil_def : def term =
  Leaf.make Tid.nil undefined_var undefined_exp

let nil_phi : phi term =
  Leaf.make Tid.nil undefined_var Tid.Map.empty

let nil_jmp : jmp term =
  Leaf.make Tid.nil undefined_exp (Goto (Direct Tid.nil))

let nil_blk : blk term =
  make_term Tid.nil {phis=[| |] ; defs = [| |] ; jmps = [| |] }

let nil_arg : arg term =
  make_term Tid.nil (undefined_var,undefined_exp,None)

let nil_sub : sub term = make_term Tid.nil {
    name = "undefined"; blks = [| |] ; args = [| |]}

let def_t : (blk,def) cls = cls Def Blk nil_def Fields_of_blk.defs
let phi_t : (blk,phi) cls = cls Phi Blk nil_phi Fields_of_blk.phis
let jmp_t : (blk,jmp) cls = cls Jmp Blk nil_jmp Fields_of_blk.jmps
let blk_t : (sub,blk) cls = cls Blk Sub nil_blk Fields_of_sub.blks
let arg_t : (sub,arg) cls = cls Arg Sub nil_arg Fields_of_sub.args
let sub_t : (program, sub) cls =
  cls Sub Top nil_sub Fields_of_program.subs

let term_pp pp_self ppf t =
  let open Format in
  let attrs = Dict.data t.dict in
  Seq.iter attrs ~f:(fun attr ->
      pp_open_tag ppf (asprintf "%a" pp_attr attr));
  fprintf ppf "@[%a: %a@]@." Tid.pp t.tid pp_self t.self;
  Seq.iter attrs ~f:(fun _ -> pp_close_tag ppf ())




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
        | Direct tid -> Format.fprintf ppf "%s" @@ Tid.name tid
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
        Format.fprintf ppf "@[call %a %a@]"
          Label.pp c.target pp_return c.return

      let hash = Hashtbl.hash
    end)
end

module Ir_arg = struct
  type t = arg term
  let create ?(tid=Tid.create()) ?intent var exp : t =
    make_term tid (var,exp,intent)

  let lhs    {self=(r,_,_)} = r
  let rhs    {self=(_,r,_)} = r
  let intent {self=(_,_,r)} = r
  let map3 (x,y,z) ~f = (x,y, f z)
  let with_intent (t : t) intent : t = {
    t with self = map3 t.self ~f:(fun _ -> Some intent)
  }
  let with_unknown_intent t : t = {
    t with self = map3 t.self ~f:(fun _ -> None)
  }
  let name arg = Var.name (lhs arg)


  let warn_unused = Bap_value.Tag.register (module Unit)
      ~name:"warn-unused"
      ~uuid:"7aa17c89-cc9b-4ed2-8700-620cb9e09491"

  let format = Bap_value.Tag.register (module String)
      ~name:"format"
      ~uuid:"d864c411-73eb-48b2-a7e9-33b51fa540c9"

  let alloc_size = Bap_value.Tag.register (module Unit)
      ~name:"alloc-size"
      ~uuid:"b29905b3-4fb5-486e-8064-9b63cadc6174"

  let restricted = Bap_value.Tag.register (module Unit)
      ~name:"restricted"
      ~uuid:"5ee30262-aed9-4aa8-a8e3-34a061104420"

  let nonnull = Bap_value.Tag.register (module Unit)
      ~name:"nonnull"
      ~uuid:"3c0a6181-9a9c-4cf4-aa37-8ceebd773952"


  include Regular.Make(struct
      type t = arg term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Arg"
      let version = "1.0.0"

      let hash = hash_of_term

      let string_of_intent = function
        | Some In -> "in "
        | Some Out -> "out "
        | Some Both -> "in out "
        | None -> ""

      let pp_self ppf (var,exp,intent) =
        Format.fprintf ppf "%s :: %s%a = %a"
          (Var.name var)
          (string_of_intent intent)
          Bap_type.pp (Var.typ var)
          Bap_exp.pp exp

      let pp = term_pp pp_self
    end)
end


module Ir_def = struct
  type t = def term
  include Leaf

  let map_exp def ~f : def term =
    with_rhs def (f (rhs def))

  let substitute def x y = map_exp def ~f:(Exp.substitute x y)

  let free_vars def = Exp.free_vars (rhs def)


  include Regular.Make(struct
      type t = def term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Def"
      let version = "1.0.0"

      let hash = hash_of_term

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a := %a" Var.pp lhs Bap_exp.pp rhs

      let pp = term_pp pp_self
    end)
end

module Ir_phi = struct
  type t = phi term
  include Leaf

  let of_list ?tid var bs : phi term =
    create ?tid var (Tid.Map.of_alist_reduce bs ~f:(fun _ x -> x))

  let create ?tid var src exp : phi term = of_list var [src,exp]

  let values (phi : phi term) : (tid * exp) Seq.t =
    Map.to_sequence (rhs phi)

  let update (phi : phi term) tid exp : phi term =
    with_rhs phi (Map.add (rhs phi) ~key:tid ~data:exp)

  let remove phi tid : phi term =
    with_rhs phi (Map.remove (rhs phi) tid)

  let select phi tid : exp option =
    Map.find (rhs phi) tid

  let select_or_unknown phi tid = match select phi tid with
    | Some thing -> thing
    | None ->
      let name = Format.asprintf "no path from %a" Tid.pp tid in
      Bap_exp.Exp.unknown name (Var.typ (lhs phi))

  let map_exp phi ~f : phi term =
    with_rhs phi (Map.map (rhs phi) ~f)

  let substitute phi x y = map_exp phi ~f:(Exp.substitute x y)

  let free_vars phi =
    values phi |> Seq.fold ~init:Bap_var.Set.empty ~f:(fun vars (_,e) ->
        Set.union vars (Exp.free_vars e))

  include Regular.Make(struct
      type t = phi term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Phi"
      let version = "1.0.0"

      let hash = hash_of_term

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a := phi(%s)"
          Var.pp lhs
          (String.concat ~sep:", " @@
           List.map ~f:(fun (id,exp) ->
               Format.asprintf "[%a, %%%a]" Bap_exp.pp exp Tid.pp id)
             (Map.to_alist rhs))
      let pp = term_pp pp_self
    end)
end

module Ir_jmp = struct
  type t = jmp term
  include Leaf

  let create_call ?tid ?(cond=always) call =
    create ?tid cond (Call call)

  let create_goto ?tid ?(cond=always) dest =
    create ?tid cond (Goto dest)

  let create_ret  ?tid ?(cond=always) dest =
    create ?tid cond (Ret  dest)

  let create_int  ?tid ?(cond=always) n t  =
    create ?tid cond (Int (n,t))

  let create      ?tid ?(cond=always) kind =
    create ?tid cond kind

  let kind = rhs
  let cond = lhs
  let with_cond = with_lhs
  let with_kind = with_rhs

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
    let jmp = with_cond jmp (f (cond jmp)) in
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


  include Regular.Make(struct
      type t = jmp term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Jmp"
      let version = "1.0.0"

      let hash = hash_of_term

      let pp_dst ppf = function
        | Goto dst -> Format.fprintf ppf "goto %a" Label.pp dst
        | Call sub -> Call.pp ppf sub
        | Ret  dst -> Format.fprintf ppf "return %a" Label.pp dst
        | Int (n,t) ->
          Format.fprintf ppf "interrupt 0x%X return %%%a" n Tid.pp t

      let pp_cond ppf cond =
        if Exp.(cond <> always) then
          Format.fprintf ppf "when %a " Bap_exp.pp cond

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a%a" pp_cond lhs pp_dst rhs

      let pp = term_pp pp_self
    end)
end


module Term = struct
  type 'a t = 'a term

  module Fields = Fields_of_term

  let create self : 'a term = make_term (Tid.create ()) self
  let clone {self} = create self
  let same x y = x.tid = y.tid
  let name b = Tid.name b.tid
  let tid x = x.tid
  let with_field field term x = {
    term with self=Field.fset field term.self x
  }

  let apply f t p = {p with self=f (t.get p.self) |> t.set p.self}

  let update t p y =
    apply (fun xs -> Array.update_if xs y ~f:(fun x -> x.tid = y.tid)) t p

  let find t p tid =
    Array.find (t.get p.self) ~f:(fun x -> x.tid = tid)

  let find_exn t p tid =
    Array.find_exn (t.get p.self) ~f:(fun x -> x.tid = tid)

  let nth t p i =
    let xs = t.get p.self in
    if i < Array.length xs then Some xs.(i) else None

  let nth_exn t p i = (t.get p.self).(i)

  let remove t p tid =
    apply (Array.remove_if ~f:(fun x -> x.tid = tid)) t p

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
    Array.findi (t.get p.self) ~f:(fun i x -> x.tid = tid)

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
    Seq.(drop_eagerly (to_sequence ~rev t p |> run ~f:(fun x -> x.tid <> tid)) cut)

  let before ?(rev=false) t p tid =
    let run,cut = if rev then Seq.drop_while,1 else Seq.take_while,0 in
    Seq.(drop_eagerly (to_sequence ~rev t p |> run ~f:(fun x -> x.tid <> tid)) cut)

  let before_or_after run ?rev t p tid =
    if Array.exists (t.get p.self) ~f:(fun x -> x.tid = tid)
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
        match Array.findi xs ~f:(fun _ x -> x.tid = this), where with
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
  let has_attr t tag = get_attr t tag <> None
  let with_attrs t dict = {t with dict}

  let length t p = Array.length (t.get p.self)

  let origin = Bap_value.Tag.register (module Tid)
      ~name:"origin"
      ~uuid:"fa804594-d2fc-4865-824a-3ad481963f54"

  let synthetic = Bap_value.Tag.register (module Unit)
      ~name:"synthetic"
      ~uuid:"a83b26b5-902e-4aaf-bfa1-503e3ced0b1a"


  let live = Bap_value.Tag.register (module Unit)
      ~name:"live"
      ~uuid:"4d3871ab-9481-4d41-97a3-cd3136acfa90"

  let dead = Bap_value.Tag.register (module Unit)
      ~name:"dead"
      ~uuid:"6009fb21-2a6c-4511-9aa4-92b2894debc7"

  let visited = Bap_value.Tag.register (module Unit)
      ~name:"visited"
      ~uuid:"0e162aa3-153f-44a3-88e7-6e42d876e760"

  let precondition = Bap_value.Tag.register (module Exp)
      ~name:"precondition"
      ~uuid:"f08c88e1-56f5-4148-822a-ac2dff34bda5"

  let invariant = Bap_value.Tag.register (module Exp)
      ~name:"invariant"
      ~uuid:"743d712b-7ee4-46da-b3b7-98d3ca5e618b"

  let postcondition = Bap_value.Tag.register (module Exp)
      ~name:"postcondition"
      ~uuid:"f248e4c1-9efc-4c70-a864-e34706e2082b"

  let change t p tid f =
    Array.findi (t.get p.self) ~f:(fun _ x -> x.tid = tid) |> function
    | None -> Option.value_map (f None) ~f:(append t p) ~default:p
    | Some (i,x) -> match f (Some x) with
      | None -> remove t p tid
      | Some c ->
        let xs = Array.mapi (t.get p.self) ~f:(fun n x ->
            if i = n then c else x) in
        {p with self = t.set p.self xs}

  let pp = term_pp

  type ('a,'b) cata = 'a term -> 'b

  let this x t = x

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



    method map_arg arg = {
      arg with
      self = map1 ~f:self#map_sym arg.self |>
             map2 ~f:self#map_exp
    }

    method map_phi phi =
      let phi = Ir_phi.(with_lhs phi (self#map_sym (lhs phi))) in
      Ir_phi.map_exp phi ~f:self#map_exp

    method map_def def =
      let def = Ir_def.(with_lhs def (self#map_sym (lhs def))) in
      Ir_def.map_exp def ~f:self#map_exp

    method map_jmp jmp = Ir_jmp.map_exp jmp ~f:self#map_exp
  end

  let visit cls ~f term init =
    enum cls term |> Seq.fold ~init ~f:(fun x t -> f t x)

  let fident t x = x

  class ['a] visitor = object(self)
    inherit ['a] Bil.exp_visitor

    method enter_term : 't 'p. ('p,'t) cls -> 't term -> 'a -> 'a = fun cls t x -> x
    method leave_term : 't 'p. ('p,'t) cls -> 't term -> 'a -> 'a = fun cls t x -> x
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

    method enter_program p x = x
    method leave_program p x = x

    method enter_sub sub x = x
    method leave_sub sub x = x

    method enter_blk blk x = x
    method leave_blk blk x = x

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
      self#visit_var (fst3 arg.self) |>
      self#visit_exp (snd3 arg.self) |>
      self#leave_arg arg

    method visit_phi phi x =
      self#enter_phi phi x |>
      self#visit_var (fst phi.self) |> fun x ->
      Map.fold (snd phi.self) ~init:x ~f:(fun ~key ~data x ->
          self#visit_exp data x) |>
      self#leave_phi phi

    method visit_def def x =
      self#enter_def def x |>
      self#visit_var (fst def.self) |>
      self#visit_exp (snd def.self) |>
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

  let create ?(tid=Tid.create ()) () : blk term = {
    tid;
    dict = Dict.empty;
    self = {
      phis = [| |] ;
      defs = [| |] ;
      jmps = [| |] ;
    }
  }

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
  let split_before blk def = split_while blk ~f:(fun {tid} -> tid <> def.tid)
  let split_after blk def =
    let i = match Array.findi blk.self.defs
                    ~f:(fun _ {tid} -> tid = def.tid) with
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
    if List.mem ~equal:Polymorphic_compare.equal skip name
    then (get blk.self)
    else Array.map (get blk.self) ~f:(fun x -> map x ~f)

  let map_exp ?(skip=[]) blk ~f = {
    blk with self = {
      phis = apply_map `phi phis Ir_phi.map_exp skip blk ~f;
      defs = apply_map `def defs Ir_def.map_exp skip blk ~f;
      jmps = apply_map `jmp jmps Ir_jmp.map_exp skip blk ~f;
    }
  }

  let substitute ?skip blk x y =
    map_exp ?skip  blk ~f:(Exp.substitute x y)

  let map_phi_lhs p ~f = Ir_phi.(with_lhs p (f (lhs p)))
  let map_def_lhs d ~f = Ir_def.(with_lhs d (f (lhs d)))

  let map_lhs ?(skip=[]) blk ~f = {
    blk with self = {
      phis = apply_map `phi phis map_phi_lhs skip blk ~f;
      defs = apply_map `def defs map_def_lhs skip blk ~f;
      jmps = jmps blk.self;
    }
  }

  let defines_var blk x =
    let exists t =
      Term.to_sequence t blk |>
      Seq.exists ~f:(fun y -> Var.(Leaf.lhs y = x)) in
    exists phi_t || exists def_t

  let free_vars blk =
    let (++) = Set.union and (--) = Set.diff in
    let init = Bap_var.Set.empty,Bap_var.Set.empty in
    fst @@ Seq.fold (elts blk) ~init ~f:(fun (vars,kill) -> function
        | `Phi phi -> vars, Set.add kill (Ir_phi.lhs phi)
        | `Def def ->
          Ir_def.free_vars def -- kill ++ vars,
          Set.add kill (Ir_def.lhs def)
        | `Jmp jmp ->
          Ir_jmp.free_vars jmp -- kill ++ vars, kill)

  let uses_var blk x = Set.mem (free_vars blk) x

  let find_var blk var =
    Term.to_sequence def_t blk ~rev:true |>
    Seq.find ~f:(fun d -> Ir_def.lhs d = var) |> function
    | Some def -> Some (`Def def)
    | None ->
      Term.to_sequence phi_t blk |>
      Seq.find ~f:(fun p -> Ir_phi.lhs p = var) |> function
      | Some phi -> Some (`Phi phi)
      | None -> None

  let occurs b ~after:dominator id =
    dominator = id ||
    Term.(after def_t b dominator |> Seq.exists ~f:(fun x -> x.tid = id))

  include Regular.Make(struct
      type t = blk term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Blk"
      let version = "1.0.0"

      let hash = hash_of_term

      let pp_self ppf self =
        Format.fprintf ppf "@[@.%a%a%a@]"
          (Array.pp Ir_phi.pp) self.phis
          (Array.pp Ir_def.pp) self.defs
          (Array.pp Ir_jmp.pp) self.jmps

      let pp = term_pp pp_self
    end)
end


module Ir_sub = struct
  type t = sub term

  let create ?(tid=Tid.create ()) ?name () : t =
    let name = match name with
      | Some name -> name
      | None -> Tid.to_string tid in
    make_term tid {
      name;
      args = [| |] ;
      blks = [| |] ;
    }

  let name sub = sub.self.name
  let with_name sub name = {sub with self = {sub.self with name}}

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
      ~name:"aliases"
      ~uuid:"ed73c040-d798-4fc9-96a0-d3c12a870955"

  let const = Bap_value.Tag.register (module Unit)
      ~name:"const"
      ~uuid:"b8795164-2e19-4469-9776-d41a6e6afe2e"

  let pure = Bap_value.Tag.register (module Unit)
      ~name:"pure"
      ~uuid:"2c477ecb-0de8-4e8e-8cba-052eb67c628a"

  let stub = Bap_value.Tag.register (module Unit)
      ~name:"stub"
      ~uuid:"c9eaf8a5-783d-4e01-9bf2-5d012602475f"

  let extern = Bap_value.Tag.register (module Unit)
      ~name:"extern"
      ~uuid:"7965a2ee-ae72-4eb6-88c1-6f2b6108915f"

  let leaf = Bap_value.Tag.register (module Unit)
      ~name:"leaf"
      ~uuid:"0688a9ac-9e55-4e37-a9e5-ac0a4e4af59e"

  let malloc = Bap_value.Tag.register (module Unit)
      ~name:"malloc"
      ~uuid:"b9237d20-07c2-462f-938d-91e4d438fd07"

  let noreturn = Bap_value.Tag.register (module Unit)
      ~name:"noreturn"
      ~uuid:"1f2941fb-d227-418d-a1d5-9f9d288a585f"

  let nothrow = Bap_value.Tag.register (module Unit)
      ~name:"nothrow"
      ~uuid:"32058b19-95ca-46e3-bee0-d0a3694fd5b1"

  let returns_twice = Bap_value.Tag.register (module Unit)
      ~name:"returns-twice"
      ~uuid:"40166004-ea98-431b-81b0-4e74a0b681ee"

  let entry_point = Bap_value.Tag.register (module Unit)
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
  include Regular.Make(struct
      type t = sub term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Sub"
      let version = "1.0.0"

      let hash = hash_of_term
      let pp_self ppf self =
        Format.fprintf ppf "@[<v>sub %s(%s)@.%a%a@]"
          self.name
          (String.concat ~sep:", " @@
           Array.to_list @@
           Array.map self.args ~f:Ir_arg.name)
          (Array.pp Ir_arg.pp) self.args
          (Array.pp Ir_blk.pp) self.blks

      let pp = term_pp pp_self
    end)
end

module Ir_program = struct
  type t = program term

  let create ?(tid=Tid.create ()) () : t = make_term tid {
      subs = [| |] ;
      paths = Tid.Table.create ();
    }

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
            if x.tid = tid then return (Some ([|i|], x )));
        None)

  let get_2nd get {self} tid : (path * 'a) option =
    with_return (fun {return} ->
        Array.iteri self.subs ~f:(fun i sub ->
            Array.iteri (get sub.self) ~f:(fun j term ->
                if term.tid = tid then return (Some ([|i;j|],term))));
        None)

  let get_3rd get {self} tid : (path * 'a) option =
    with_return (fun {return} ->
        Array.iteri self.subs ~f:(fun i {self} ->
            Array.iteri self.blks ~f:(fun j blk ->
                Array.iteri (get blk.self) ~f:(fun k ent ->
                    if ent.tid = tid
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
        if thing.tid = tid then Some thing
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
    | Top -> (fun p tid -> Option.some_if (p.tid = tid) p)
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

    let add_sub (_,subs) = Vec.append subs

    let result (tid,subs) : program term =
      let tid = match tid with
        | Some tid -> tid
        | None -> Tid.create () in
      make_term tid {
        subs = Vec.to_array subs;
        paths = Tid.Table.create ();
      }
  end

  include Regular.Make(struct
      type t = program term [@@deriving bin_io, compare, sexp]
      let module_name = Some "Bap.Std.Program"
      let version = "1.0.0"

      let hash = hash_of_term
      let pp_self ppf self =
        Format.fprintf ppf "@[<v>program@.%a@]"
          (Array.pp Ir_sub.pp) self.subs
      let pp = term_pp pp_self
    end)
end
