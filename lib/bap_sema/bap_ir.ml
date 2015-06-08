open Core_kernel.Std
open Bap_types.Std

module Dict = Value.Dict

module Tid = struct
  exception Overrun
  type t = Int63.t
  let create =
    let tid = ref Int63.zero in
    fun () ->
      Int63.incr tid;
      if tid.contents = Int63.zero
      then raise Overrun;
      tid.contents
  let nil = Int63.zero
  include Regular.Make(struct
      type nonrec t = Int63.t with bin_io, compare, sexp
      let module_name = "Bap.Std.Tid"
      let hash = Int63.hash

      let pp ppf tid =
        Format.fprintf ppf "%08Lx" (Int63.to_int64 tid)

      let to_string tid = Format.asprintf "%a" pp tid
    end)
end

type tid = Tid.t with bin_io, compare, sexp

type 'a term = {
  tid : tid;
  self : 'a;
  dict : dict;
} with bin_io, compare, fields, sexp

type label =
  | Direct of tid
  | Indirect of exp
with bin_io, compare, sexp

type call = {target : label; return : label option}
with bin_io, compare, fields, sexp

type jmp_kind =
  | Call of call
  | Goto of label
  | Ret  of label
  | Int  of int * tid
with bin_io, compare, sexp


type intent = In | Out | Both with bin_io, compare, sexp

type jmp = (exp * jmp_kind) with bin_io, compare, sexp
type def = (var * exp) with bin_io, compare, sexp
type phi = (var * def term list) with bin_io, compare, sexp

type blk = {
  phis : phi term array;
  defs : def term array;
  jmps : jmp term array;
} with bin_io, compare, fields, sexp

type arg = var * intent option
with bin_io, compare, sexp

type sub = {
  name : string;
  blks : blk term array;
  args : arg term array;
} with bin_io, compare, fields, sexp


type path = int array
with bin_io, compare, sexp


type program = {
  subs  : sub term array;
  paths : path Tid.Table.t;
} with bin_io, fields, sexp

let compare_program x y =
  let compare x y = <:compare<sub term array>> x y in
  compare x.subs y.subs


module Array = struct
  include Array
  (** [insert xs x i] insert [x] into [xs] in a position befor [i].
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


let always = Bil.(int Word.b1)
let never  = Bil.(int Word.b0)
let undefined_exp = Bil.unknown "undefined" bool_t
let undefined_var = Var.create "undefined" bool_t

let pp_attr ppf attr =
  Format.fprintf ppf "@[.%s %a@]"
    (Value.tagname attr) Value.pp attr

let pp_attrs ppf dict =
  Dict.data dict |> Seq.iter ~f:(pp_attr ppf)

module Leaf = struct
  let create lhs rhs = {
    tid = Tid.create ();
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

type _ typ =
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

let nil_def : def term =
  Leaf.make Tid.nil undefined_var undefined_exp

let nil_phi : phi term =
  Leaf.make Tid.nil undefined_var []

let nil_jmp : jmp term =
  Leaf.make Tid.nil undefined_exp (Goto (Direct Tid.nil))

let nil_blk : blk term =
  make_term Tid.nil {phis=[| |] ; defs = [| |] ; jmps = [| |] }

let nil_arg : arg term = make_term Tid.nil (undefined_var,None)

let nil_sub : sub term = make_term Tid.nil {
    name = "undefined"; blks = [| |] ; args = [| |]}

let def_t : (blk,def) cls = cls Def Blk nil_def Fields_of_blk.defs
let phi_t : (blk,phi) cls = cls Phi Blk nil_phi Fields_of_blk.phis
let jmp_t : (blk,jmp) cls = cls Jmp Blk nil_jmp Fields_of_blk.jmps
let blk_t : (sub,blk) cls = cls Blk Sub nil_blk Fields_of_sub.blks
let arg_t : (sub,arg) cls = cls Arg Sub nil_arg Fields_of_sub.args
let sub_t : (program, sub) cls =
  cls Sub Top nil_sub Fields_of_program.subs

module Term = struct
  type 'a t = 'a term

  module Fields = Fields_of_term

  let create self : 'a term = make_term (Tid.create ()) self
  let clone {self} = create self
  let same x y = x.tid = y.tid
  let name b = Format.asprintf "%a" Tid.pp b.tid
  let tid x = x.tid
  let with_field field term x = {
    term with self=Field.fset field term.self x
  }

  let apply f t p = {p with self=f (t.get p.self) |> t.set p.self}

  let update t p y =
    apply (fun xs -> Array.update_if xs y ~f:(fun x -> x.tid = y.tid)) t p

  let find t p tid = Array.find (t.get p.self) ~f:(fun x -> x.tid = tid)

  let remove t p tid =
    apply (Array.remove_if ~f:(fun x -> x.tid = tid)) t p

  let to_seq xs =
    Seq.init (Array.length xs) ~f:(Array.unsafe_get xs)

  let to_seq_rev xs =
    let n = Array.length xs in
    Sequence.init n ~f:(fun i -> Array.unsafe_get xs (n - i - 1))

  let to_sequence ?(rev=false) t p =
    if rev then to_seq_rev (t.get p.self) else to_seq (t.get p.self)

  let map t p ~f : 'a term = apply (Array.map ~f) t p

  let filter_map t p ~f : 'a term = apply (Array.filter_map ~f) t p

  let concat_map t p ~f =
    let concat_map xs =
      let vec = Vector.create ~capacity:(Array.length xs) t.nil in
      Array.iter xs ~f:(fun x -> List.iter (f x) ~f:(Vector.append vec));
      Vector.to_array vec in
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
  let get_attr t = Dict.find t.dict
  let del_attr t tag = {t with dict = Dict.remove t.dict tag}
  let has_attr t tag = get_attr t tag <> None
  let length t p = Array.length (t.get p.self)

  let pp pp_self ppf t =
    let open Format in
    let attrs = Dict.data t.dict in
    Seq.iter attrs ~f:(fun attr ->
        pp_open_tag ppf (asprintf "%a" pp_attr attr));
    Format.fprintf ppf "@[%a: %a@]@." Tid.pp t.tid pp_self t.self;
    Seq.iter attrs ~f:(fun _ -> pp_close_tag ppf ());
end

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
      type t = label with bin_io, compare, sexp
      let module_name = "Bap.Std.Label"
      let hash = Hashtbl.hash
      let pp ppf = function
        | Indirect exp -> Exp.pp ppf exp
        | Direct tid -> Format.fprintf ppf "%%%a" Tid.pp tid
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
      type t = call with bin_io, compare, sexp
      let module_name = "Bap.Std.Call"

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
  include Leaf
  let create ?intent ?name typ : t =
    let tid = Tid.create () in
    let tmp = Option.is_none name in
    let name = match name with
      | Some name -> name
      | None -> "arg" in
    let var = Var.create ~tmp name typ in
    make_term tid (var,intent)

  let var = lhs
  let intent = rhs
  let with_intent (t : t) intent : t = with_rhs t (Some intent)
  let with_unknown_intent : t -> t = fun t -> with_rhs t None
  let name arg = Var.name (var arg)

  include Regular.Make(struct
      type t = arg term with bin_io, compare, sexp
      let module_name = "Bap.Std.Arg"
      let hash = hash_of_term

      let string_of_intent = function
        | Some In -> "in "
        | Some Out -> "out "
        | Some Both -> "in out "
        | None -> ""

      let pp_self ppf (var,intent) =
        Format.fprintf ppf "%s :: %s%a"
          (Var.name var)
          (string_of_intent intent)
          Type.pp (Var.typ var)

      let pp = Term.pp pp_self
    end)
end

module Ir_def = struct
  type t = def term
  include Leaf
  include Regular.Make(struct
      type t = def term with bin_io, compare, sexp
      let module_name = "Bap.Std.Def"
      let hash = hash_of_term

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a := %a" Var.pp lhs Exp.pp rhs

      let pp = Term.pp pp_self
    end)
end

module Ir_phi = struct
  type t = phi term
  include Leaf
  let create var def : phi term =
    create var [def]

  let defs (phi : phi term) : def term seq = Seq.of_list (rhs phi)
  let add_def (phi : phi term) def : phi term = with_rhs phi (def :: rhs phi)
  let remove_def phi tid : phi term =
    with_rhs phi (List.filter (rhs phi) ~f:(fun p -> p.tid <> tid))

  include Regular.Make(struct
      type t = phi term with bin_io, compare, sexp
      let module_name = "Bap.Std.Phi"
      let hash = hash_of_term

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a = phi(%s)"
          Var.pp lhs
          (String.concat ~sep:", "
             (List.map rhs
                ~f:(Fn.compose Tid.to_string tid)))

      let pp = Term.pp pp_self
    end)
end

module Ir_jmp = struct
  type t = jmp term
  include Leaf

  let create_call ?(cond=always) call = create cond (Call call)
  let create_goto ?(cond=always) dest = create cond (Goto dest)
  let create_ret  ?(cond=always) dest = create cond (Ret  dest)
  let create_int  ?(cond=always) n t  = create cond (Int (n,t))
  let create      ?(cond=always) kind = create cond kind

  let kind = rhs
  let cond = lhs
  let with_cond = with_lhs
  let with_kind = with_rhs

  include Regular.Make(struct
      type t = jmp term with bin_io, compare, sexp
      let module_name = "Bap.Std.Jmp"
      let hash = hash_of_term

      let pp_dst ppf = function
        | Goto dst -> Format.fprintf ppf "goto %a" Label.pp dst
        | Call sub -> Call.pp ppf sub
        | Ret  dst -> Format.fprintf ppf "return %a" Label.pp dst
        | Int (n,t) ->
          Format.fprintf ppf "interrupt 0x%X return %%%a" n Tid.pp t

      let pp_cond ppf cond =
        if Exp.(cond <> always) then
          Format.fprintf ppf "when %a " Exp.pp cond

      let pp_self ppf (lhs,rhs) =
        Format.fprintf ppf "%a%a" pp_cond lhs pp_dst rhs

      let pp = Term.pp pp_self
    end)
end

module Ir_blk = struct
  type t = blk term
  type elt = Def of def term | Phi of phi term | Jmp of jmp term

  module Fields = Fields_of_blk

  module Builder = struct
    type t = {
      b_tid : tid;
      b_defs : def term vector;
      b_phis : phi term vector;
      b_jmps : jmp term vector;
    }

    let create ?tid ?(phis=16) ?(defs=16) ?(jmps=16) () = {
      b_phis = Vector.create ~capacity:phis nil_phi;
      b_defs = Vector.create ~capacity:defs nil_def;
      b_jmps = Vector.create ~capacity:jmps nil_jmp;
      b_tid = match tid with
        | None -> Tid.create ()
        | Some tid -> tid
    }

    let add_def b = Vector.append b.b_defs
    let add_jmp b = Vector.append b.b_jmps
    let add_phi b = Vector.append b.b_phis
    let add_elt b = function
      | Jmp j -> add_jmp b j
      | Phi p -> add_phi b p
      | Def d -> add_def b d

    let of_vec = Vector.to_array

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

  let create () : blk term = {
    tid = Tid.create ();
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
      Seq.(seq jmp_t ~rev blk >>| fun x -> Jmp x) @
      Seq.(seq def_t ~rev blk >>| fun x -> Def x) @
      Seq.(seq phi_t ~rev blk >>| fun x -> Phi x)
    else
      Seq.(seq phi_t ~rev blk >>| fun x -> Phi x) @
      Seq.(seq def_t ~rev blk >>| fun x -> Def x) @
      Seq.(seq jmp_t ~rev blk >>| fun x -> Jmp x)

  let dominated b ~by:dominator id =
    dominator = id ||
    Term.(after def_t b dominator |> Seq.exists ~f:(fun x -> x.tid = id))

  include Regular.Make(struct
      type t = blk term with bin_io, compare, sexp
      let module_name = "Bap.Std.Blk"
      let hash = hash_of_term

      let pp_self ppf self =
        Format.fprintf ppf "@[@.%a%a%a@]"
          (Array.pp Ir_phi.pp) self.phis
          (Array.pp Ir_def.pp) self.defs
          (Array.pp Ir_jmp.pp) self.jmps

      let pp = Term.pp pp_self
    end)
end


module Ir_sub = struct
  type t = sub term

  let create ?name () : t =
    let tid = Tid.create () in
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

  module Builder = struct
    type t =
      tid option * arg term vector * blk term vector * string option

    let create ?tid ?(args=4) ?(blks=16) ?name () : t =
      tid,
      Vector.create ~capacity:args nil_arg,
      Vector.create ~capacity:blks nil_blk,
      name

    let add_blk (_,_,bs,_) = Vector.append bs
    let add_arg (_,xs,_,_) = Vector.append xs

    let result (tid,args,blks,name) : sub term =
      let tid = match tid with
        | Some tid -> tid
        | None -> Tid.create () in
      let args = Vector.to_array args in
      let blks = Vector.to_array blks in
      let name = match name with
        | Some name -> name
        | None -> Format.asprintf "sub_%a" Tid.pp tid in
      make_term tid {name; args; blks}
  end
  include Regular.Make(struct
      type t = sub term with bin_io, compare, sexp
      let module_name = "Bap.Std.Sub"
      let hash = hash_of_term
      let pp_self ppf self =
        Format.fprintf ppf "@[<v>sub %s(%s)@.%a%a@]"
          self.name
          (String.concat ~sep:", " @@
           Array.to_list @@
           Array.map self.args ~f:Ir_arg.name)
          (Array.pp Ir_arg.pp) self.args
          (Array.pp Ir_blk.pp) self.blks

      let pp = Term.pp pp_self
    end)
end

module Ir_program = struct
  type t = program term

  let create () : t = Term.create {
      subs = [| |] ;
      paths = Tid.Table.create ();
    }

  let def_of_path {self} : path -> def term = function
    | [| i; j; k |] -> self.subs.(i).self.blks.(j).self.defs.(k)
    | _ -> assert false

  let phi_of_path {self} : path -> phi term = function
    | [| i; j; k |] -> self.subs.(i).self.blks.(j).self.phis.(k)
    | _ -> assert false

  let jmp_of_path {self} : path -> jmp term = function
    | [| i; j; k |] -> self.subs.(i).self.blks.(j).self.jmps.(k)
    | _ -> assert false

  let blk_of_path {self} : path -> blk term = function
    | [| i; j |] -> self.subs.(i).self.blks.(j)
    | _ -> assert false

  let arg_of_path {self} : path -> arg term = function
    | [| i; j |] -> self.subs.(i).self.args.(j)
    | _ -> assert false

  let sub_of_path {self} : path -> sub term = function
    | [| i |] -> self.subs.(i)
    | _ -> assert false

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

  let lookup t = finder_of_type t.typ

  let parent (type a) (t : (a,'b) cls) p tid : a term option =
    match lookup t p tid with
    | None -> None
    | Some _ ->
      let child = Tid.Table.find_exn p.self.paths tid in
      let path = Array.subo ~len:(Array.length child - 1) child in
      match t.par with
      | Blk -> Some (blk_of_path p path)
      | Arg -> Some (arg_of_path p path)
      | Sub -> Some (sub_of_path p path)
      | Top -> Some p
      | _ -> None

  module Builder = struct
    type t = tid option * sub term vector

    let create ?tid ?(subs=16) () : t =
      tid, Vector.create ~capacity:subs nil_sub

    let add_sub (_,subs) = Vector.append subs

    let result (tid,subs) : program term =
      let tid = match tid with
        | Some tid -> tid
        | None -> Tid.create () in
      make_term tid {
        subs = Vector.to_array subs;
        paths = Tid.Table.create ();
      }
  end

  include Regular.Make(struct
      type t = program term with bin_io, compare, sexp
      let module_name = "Bap.Std.Program"
      let hash = hash_of_term
      let pp_self ppf self =
        Format.fprintf ppf "@[<v>program@.%a@]"
          (Array.pp Ir_sub.pp) self.subs
      let pp = Term.pp pp_self
    end)
end
