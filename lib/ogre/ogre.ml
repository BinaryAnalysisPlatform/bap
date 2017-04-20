open Core_kernel.Std
open Monads.Std
open Format

type entry = {
  fields : string String.Map.t
} [@@deriving compare]

type row = {row : entry array}
type 'a seq = 'a Sequence.t

module Type = struct
  type typ = Int | Str | Bool | Float
  [@@deriving compare,enumerate,sexp]
  type 'a t = {
    parse : string -> 'a option;
    pack : 'a -> string;
    typ : typ;
  }

  type 'a field = {
    t : 'a t;
    name : string;
  }

  type header = {
    fname : string;
    ftype : typ;
  } [@@deriving compare, sexp]

  type signature = header list [@@deriving compare]

  type ('f,'k) scheme = {
    read : entry -> 'a -> 'b option;
    save : (entry -> 'd) -> 'c;
    signature : signature;
  } constraint 'f = 'a -> 'b
    constraint 'k = 'c -> 'd


  type ('r,'a,'b) sealed = ('k,'f) scheme
    constraint 'f = 'a -> 'r
    constraint 'k = 'b -> entry

  let pack sexp x = Sexp.to_string (sexp x)
  let atom parse s =
    try Some (parse (Sexp.Atom s)) with exn -> None

  let header_equal x y = compare_header x y = 0

  let int = {
    parse = atom int64_of_sexp;
    pack = pack sexp_of_int64;
    typ = Int;
  }

  let bool = {
    parse = atom bool_of_sexp;
    pack = pack sexp_of_bool;
    typ = Bool;
  }

  let str = {
    parse = atom string_of_sexp;
    pack = ident;
    typ = Str;
  }

  let float = {
    parse = atom float_of_sexp;
    pack = pack sexp_of_float;
    typ = Float;
  }

  let string_of_typ = function
    | Int -> "int"
    | Str -> "str"
    | Bool -> "bool"
    | Float -> "float"

  let names = all_of_typ |> List.map ~f:(fun t -> string_of_typ t,t)

  let get {fields} f = Option.(Map.find fields f.name >>= f.t.parse)

  let add {name; t} x {fields} = {
    fields = Map.add fields ~key:name ~data:(t.pack x)
  }

  let empty = {fields = String.Map.empty}

  let scheme field = {
    signature = [{fname = field.name; ftype = field.t.typ}];
    save = (fun k x -> k (add field x empty));
    read = fun ent k -> match get ent field with
      | None -> None
      | Some x -> Some (k x)
  }

  let ($) {read; save; signature} fld = {
    signature = signature @ [{
        fname = fld.name;
        ftype = fld.t.typ;
      }];
    save = (fun k -> save (fun ent x -> k (add fld x ent)));
    read = fun ent k -> match read ent k with
      | None -> None
      | Some k -> match get ent fld with
        | None -> None
        | Some x -> Some (k x)
  }

  let pack t x = t.pack x

  let ok t x = Option.is_some (t.parse x)
  let check = function
    | Int -> ok int
    | Str -> ok str
    | Bool -> ok bool
    | Float -> ok float

  let signature_mismatch ~expect:s1 ~got:s2 =
    Or_error.errorf "signature mismatch"

  let def name t = {name;t}
  let (%:) = def
end

type 'a field = 'a Type.field
type ('a,'s) scheme = ('a,'s) Type.scheme

module Attribute = struct
  type ('a,'k) info = {
    name : string;
    sign : Type.signature;
    read : entry -> 'a option;
    save : (entry -> 'd) -> 'c;
  } constraint 'k = 'c -> 'd


  type ('a,'k) t = unit -> ('a,'k) info

  let declare ~name {Type.signature; read; save} cons = {
    name; sign = signature;
    read = (fun entry -> read entry cons);
    save = save;
  }

  let pack t x = (t ()).save x
  let name t = (t ()).name
end

type ('a,'k) typeinfo = ('a,'k) Attribute.info
let declare = Attribute.declare


module Doc = struct
  module Error = Monad.Result.Error
  open Error.Syntax

  type t = {
    scheme  : Type.signature String.Map.t;
    entries : entry list String.Map.t;
  }

  let empty = {
    scheme = String.Map.empty;
    entries = String.Map.empty;
  }

  let errorf fmt = Or_error.errorf fmt

  let reduce_fields fields =
    List.sort fields ~cmp:compare_entry |>
    List.remove_consecutive_duplicates
      ~equal:(fun x y -> compare x y = 0)

  let update_scheme name sign doc =
    match Map.find doc.scheme name with
    | Some s when Type.compare_signature s sign <> 0 ->
      Type.signature_mismatch ~expect:s ~got:sign
    | _ -> Ok {
        doc with scheme = Map.add doc.scheme ~key:name ~data:sign
      }

  let typecheck_entry name {fields} {scheme} =
    match Map.find scheme name with
    | None -> errorf "The attribute %s is not declared" name
    | Some sign ->
      Error.List.fold sign ~init:0 ~f:(fun fnum {Type.fname; ftype} ->
          match Map.find fields fname with
          | None ->
            errorf "attribute %S didn't provide \
                    a value for the field %S" name fname
          | Some value when Type.check ftype value -> Ok (fnum + 1)
          | Some bad ->
            errorf "value %S is not in a domain of values of\
                    field '%s.%s'" bad name fname) >>= fun checked ->
      if checked = Map.length fields then Ok ()
      else errorf "attribute %S has an arity %d, while a value with \
                   arity %d was provided" name checked (Map.length fields)

  let update_entries name packed doc =
    typecheck_entry name packed doc >>| fun () -> {
      doc with entries =
                 Map.add_multi doc.entries ~key:name ~data: packed
    }

  let merge doc {scheme; entries} =
    Map.to_sequence scheme |>
    Error.Seq.fold ~init:doc ~f:(fun doc (name,sign) ->
      update_scheme name sign doc) >>= fun doc ->
    Map.to_sequence entries |>
    Error.Seq.fold ~init:doc ~f:(fun doc (name,values) ->
        Error.List.fold values ~init:doc ~f:(fun doc value ->
          update_entries name value doc))


  let put k attr =
    let {Attribute.name; save; sign} = attr () in
    save (fun packed ->
        k (fun doc ->
            Or_error.(update_scheme name sign doc >>=
                      update_entries name packed)))

  let get {scheme;entries} attr =
    let {Attribute.name; read; sign} = attr () in
    match Map.find entries name with
    | None -> Ok []
    | Some entries -> Ok (List.filter_map ~f:read entries)


  let tabulate xs = List.mapi xs ~f:(fun i x -> (i,x))


  let parse_tname name =
    List.Assoc.find Type.names name |> function
    | Some typ -> Ok typ
    | None ->
      errorf "expected <field-type> ::= %s"
        (List.map Type.names ~f:fst |> String.concat ~sep:" | ")

  let parse_fname name =
    if Char.is_alpha name.[0] &&
       String.for_all name ~f:(fun c ->
           Char.is_alphanum c || c = '-')
    then Ok name
    else errorf "bad field name %s, expected [a..z][a..z-]*" name

  let parse_fdecl = function
    | i, Sexp.Atom tname ->
      parse_tname tname >>| fun ftype ->
      {Type.fname = sprintf "%d" i; ftype}
    | _, Sexp.List [Sexp.Atom fname; Sexp.Atom tname] ->
      parse_fname fname >>= fun fname ->
      parse_tname tname >>| fun ftype ->
      {Type.ftype; fname}
    | _ ->
      errorf "expected <decl> ::= \
              <field-type> | (<field-name> <field-type>)"

  let parse_fdefn = function
    | Sexp.Atom x -> Ok (`Positional x)
    | Sexp.List [Sexp.Atom fname; Sexp.Atom x] -> Ok (`Named (fname,x))
    | _ -> errorf "expected <attribute-value> ::= \
                   <field-value> | (<field-name> <field-value>)"

  let positional header pos x =
    match List.Assoc.find header pos with
    | None -> errorf "wrong arity"
    | Some {Type.fname} -> Ok (fname,x)

  let named hdr name x =
    List.find hdr ~f:(fun (_,{Type.fname}) -> name = fname) |> function
    | None -> errorf "unknown field %s" name
    | Some _ -> Ok (name,x)

  let nest_defn scheme name values =
    match Map.find scheme name with
    | None -> errorf "%s is not declared in the scheme" name
    | Some header ->
      let header = tabulate header in
      Error.List.map (tabulate values) ~f:(function
          | pos, `Positional x -> positional header pos x
          | _, `Named (fnam,x) -> named header fnam x) >>=
      String.Map.of_alist_or_error

  let parse_entry doc = function
    | Sexp.List (Sexp.Atom "declare" :: Sexp.Atom name :: flds) ->
      Error.List.map ~f:parse_fdecl (tabulate flds) >>= fun sign ->
      update_scheme name sign  doc
    | Sexp.List (Sexp.Atom "declare" :: _) ->
      errorf "expected (declare <attribute-name> <field-decls>)"
    | Sexp.List (Sexp.Atom name :: flds) ->
      Error.List.map flds ~f:parse_fdefn >>=
      nest_defn doc.scheme name >>= fun fields ->
      update_entries name {fields} doc
    | _ -> errorf "expected <ogre-entry> ::= \
                   | (declare <attribute-name> <field-decls>)\
                   | (<attribute-name> <attribute-value>)"

  let of_sexps =
    Error.List.fold ~init:empty ~f:parse_entry

  let sexp_of_header {Type.fname; ftype} =
    let tname = Type.string_of_typ ftype in
    if String.for_all fname ~f:Char.is_digit
    then Sexp.Atom tname
    else Sexp.(List [Atom fname; Atom tname])

  let sexp_of_decl (name,s) =
    Sexp.(List (Atom "declare" :: Atom name ::
                List.map ~f:sexp_of_header s))


  let sexps_of_value scheme name {fields} =
    match Map.find scheme name with
    | None -> invalid_argf "can't find a header for attribute %s" name ()
    | Some sign ->
      List.map sign ~f:(fun {Type.fname} ->
          match Map.find fields fname with
          | None -> invalid_argf "malformed data base - \
                                  attribute %s misses field %s"
                      name fname ()
          | Some value -> Sexp.Atom value)

  let sexp_of_attr scheme name value =
    Sexp.(List (Atom name :: sexps_of_value scheme name value))

  let pp_scheme ppf {scheme} =
    fprintf ppf "@[<v0>";
    Map.iteri scheme ~f:(fun ~key:n ~data:s ->
        fprintf ppf "@[%a@]@;" Sexp.pp_hum (sexp_of_decl (n,s)));
    fprintf ppf "@]"

  let pp_body ppf {scheme; entries} =
    fprintf ppf "@[<v>";
    Map.iteri entries ~f:(fun ~key:n ~data:vs ->
        List.iter vs ~f:(fun v ->
            fprintf ppf "@[%a@]@;"
              Sexp.pp_hum (sexp_of_attr scheme n v)));
    fprintf ppf "@]"

  let pp ppf t =
    fprintf ppf "%a@\n%a" pp_scheme t pp_body t

  let load channel = Or_error.try_with_join (fun () ->
      of_sexps (Sexp.input_sexps channel))

  let from_file name = In_channel.with_file name ~f:load

  let save doc ch =
    let ppf = formatter_of_out_channel ch in
    pp ppf doc;
    pp_print_flush ppf ()

  let to_file doc name =
    Out_channel.with_file name ~f:(save doc)

  let from_string str =
    Or_error.try_with_join (fun () ->
        Sexp.scan_sexps (String.strip str |> Lexing.from_string) |>
        of_sexps)

  let to_string x = asprintf "%a" pp x
end

type ('a,'k) attribute = ('a,'k) Attribute.t

type doc = Doc.t
type 'a column = {attr : 'a; field : Type.header}
[@@deriving compare,sexp]




module Exp = struct
  type name = Name of string | Pos of int [@@deriving compare,sexp]
  type bop = And | Or | Lt | Add | Sub [@@deriving compare, sexp]
  type uop = Not [@@deriving compare, sexp]
  type var = name column [@@deriving compare, sexp]
  type 'a exp =
    | True
    | Int of int64
    | Str of string
    | Flt of float
    | Var of 'a
    | Bop of bop * 'a exp * 'a exp
    | Uop of uop * 'a exp
  [@@deriving compare, sexp]

  type t = var exp [@@deriving compare, sexp]

  type ivar = int column [@@deriving compare, sexp]
  type indexed = ivar exp [@@deriving compare, sexp]

  module Var = Comparable.Make(struct
      type t = var [@@deriving compare,sexp]
    end)

  let string_of_name = function
    | Name s -> s
    | Pos n -> sprintf "#%d" n

  let bool = function
    | true -> True
    | false -> Uop (Not,True)

  let less compare x y = bool (compare x y < 0)

  let not x = Uop (Not,x)

  (** preserves NNF, pushes disjuncts inwards, short-circuits expressions  *)
  let rec simpl = function
    | Bop (And,Uop (Not,True),_)
    | Bop (And,_,Uop (Not,True)) -> Uop (Not,True)
    | Bop (And,True,e)
    | Bop (And,e,True) -> simpl e
    | Bop (Or, Uop (Not,True),e)
    | Bop (Or, e, Uop (Not,True)) -> simpl e
    | Bop (Or, True,_)
    | Bop (Or, _,True) -> True
    | Bop (Lt,Int x, Int y) -> less compare_int64 x y
    | Bop (Lt,Str x, Str y) -> less compare_string x y
    | Bop (Lt,Flt x, Flt y) -> less compare_float x y
    | Bop (Add,(Int 0L | Flt 0.),x)
    | Bop (Add,x,(Int 0L | Flt 0.)) -> simpl x
    | Bop (Sub,x,(Int 0L | Flt 0.)) -> simpl x
    | Bop (Add,Int x, Int y) -> Int Int64.(x + y)
    | Bop (Sub,Int x, Int y) -> Int Int64.(x - y)
    | Bop (Add,Flt x, Flt y) -> Flt Float.(x + y)
    | Bop (Sub,Flt x, Flt y) -> Flt Float.(x - y)
    | Uop (Not,(Uop (Not,x))) -> simpl x
    | Uop (Not,Bop (Or,x,y)) -> simpl (Bop (And, not x, not y))
    | Uop (Not,Bop (And,x,y)) -> simpl (Bop (Or, not x, not y))
    | Bop (Or, Bop (And, x,y),z)
    | Bop (Or, z, Bop (And,x,y)) ->
      simpl (Bop (And, Bop (Or,z,x), Bop (Or,z,y)))
    | Uop (op,e) -> Uop (op, simpl e)
    | Bop (op,x,y) -> Bop (op,simpl x, simpl y)
    | e -> e

  module Syntax = struct
    let bool = bool
    let str x = Str x
    let var x = Var x
    let int x = Int x
    let float x = Flt x
    let (&&) x y = simpl (Bop (And,x,y))
    let (||) x y = simpl (Bop (Or,x,y))
    let (==>) x y = simpl (Bop (Or,Uop(Not,x),y))
    let not x = simpl (Uop (Not,x))
    let (<) x y = simpl (Bop(Lt,x,y))
    let (=) x y = simpl (not (x < y) && not (y < x))
    let (<>) x y = simpl (not (x = y))
    let (>) x y = simpl (y < x)
    let (<=) x y = simpl (not (y < x))
    let (>=) x y = simpl (not (y > x))
    let (+) x y = simpl (Bop (Add,x,y))
    let (-) x y = simpl (Bop (Sub,x,y))
  end

  let validate_exp doc = Ok ()

  module Seq = Sequence

  let rec n_cross_product xs =
    let open Seq in match next xs with
    | None -> singleton empty
    | Some (row,rows) ->
      row >>= fun elt ->
      n_cross_product rows >>|
      append (singleton elt)


  let has_all_attributes names data =
    List.for_all names ~f:(Map.mem data)

  let select names data =
    Seq.of_list names |>
    Seq.map ~f:(fun name -> match Map.find data name with
        | None -> failwithf "precondition fails for %s" name ()
        | Some values ->
          Seq.of_list values |> Seq.map ~f:(fun value -> name,value))

  let parser {Type.parse; typ} inj =
    typ, fun value -> match parse value with
      | None -> failwith "expression parse error"
      | Some x -> inj x

  let parsers = [
    parser Type.int (fun x -> Int x);
    parser Type.str (fun x -> Str x);
    parser Type.float (fun x -> Flt x);
    parser Type.bool (function
        | true -> True
        | false -> Uop (Not,True))
  ]

  let lift ftype = List.Assoc.find_exn parsers ftype

  let lookup_var name {fields} {attr; field={Type.fname;ftype}} =
    if attr = Some name || attr = None
    then Map.find fields fname |> Option.map ~f:(lift ftype)
    else None

  (* join on one variable represented by an equivalence class *)
  let unify_var columns row =
    let columns = Seq.of_list columns in
    Seq.concat_map row  ~f:(fun (name,value) ->
        Seq.filter_map columns ~f:(lookup_var name value)) |>
    Seq.next |> function
    | None -> True
    | Some (r,es) ->
      Seq.fold es ~init:True ~f:(fun e p -> Syntax.(e && (p = r)))


  (* build a constraint that will unify all equivalence classes *)
  let unify joins row =
    List.fold joins ~init:True ~f:(fun exp cls ->
        Syntax.(exp && unify_var cls row))

  let matches var pos offs = match var with
    | Pos p -> p = pos
    | Name n -> match Map.find offs n with
      | None -> invalid_argf "bad name %s" n ()
      | Some p -> p = pos

  (* @pre: all attributes are unique
     @pre: variables are type checked
     @pre: the database is typechecked
     @pre: row degree is greater than zero
  *)
  let bind_vars offs vars row =
    Set.fold vars ~init:Var.Map.empty ~f:(fun bs var ->
        let {field={Type.fname; ftype}; attr} = var in
        Array.foldi ~init:bs row ~f:(fun fn bs ({fields}) ->
            if matches attr fn offs
            then match Map.find fields fname with
              | None -> failwithf "bad field %s" fname ()
              | Some v -> match Map.find bs var with
                | None -> Map.add bs ~key:var ~data:(lift ftype v)
                | Some v' ->
                  failwithf "variable %s is ambiguous"
                    (string_of_name attr) ()
            else bs))

  let sat lookup exp =
    let rec eval = function
      | True | Int _ | Str _ | Flt _ as r -> r
      | Var n -> lookup n
      | Uop (op,x) -> uop op x
      | Bop (op,x,y) -> bop op x y
    and uop Not x = simpl (Uop (Not,eval x))
    and bop op x y = match op with
      | And -> eval_and x y
      | Or -> eval_or x y
      | op -> simpl (Bop (op,eval x,eval y))
    and eval_and x y = match eval x with
      | True -> eval y
      | Uop (Not,True) as r -> r
      | _ -> failwith "type error: and ~> bot"
    and eval_or x y = match eval x with
      | True -> True
      | Uop (Not,True) -> eval y
      | _ -> failwith "type error: or ~> bot" in
    match eval exp with
    | True -> true
    | Uop (Not,True) -> false
    | _ -> failwith "sat ~> bot"

  let rec subst sub = function
    | True | Int _ | Str _ | Flt _ as x -> x
    | Uop (op,x) -> simpl (Uop (op, subst sub x))
    | Bop (op,x,y) -> simpl (Bop (op,subst sub x,subst sub y))
    | Var n -> sub n


  let vars exp =
    let rec collect vars = function
      | True | Int _ | Str _ | Flt _ -> vars
      | Var v -> Set.add vars v
      | Bop (_,x,y) -> collect (collect vars x) y
      | Uop (_,x) -> collect vars x in
    collect Var.Set.empty exp

  let offsets names =
    List.foldi names ~init:String.Map.empty ~f:(fun pos offs name ->
        Map.add offs ~key:name ~data:pos)

  let normalize_vars offs =
    subst (function
        | {attr=Pos n} as v -> (Var v)
        | {attr=Name n} as v -> match Map.find offs n with
          | None -> invalid_argf "%s is unbound" n ()
          | Some n -> Var {v with attr = Pos n})

  let names_index names =
    let offs = offsets names in
    function
    | Name s -> Map.find_exn offs s
    | Pos n -> n

  let index_vars names exp =
    let index_of_name = names_index names in
    let rec index subst = function
      | True | Int _ | Str _ | Flt _ as x -> subst,x
      | Bop (op,x,y) ->
        let subst,x = index subst x in
        let subst,y = index subst y in
        subst, Bop (op,x,y)
      | Uop (op,x) ->
        let subst,x = index subst x in
        subst, Uop(op,x)
      | Var {attr = name; field = {Type.fname; ftype} as fld} ->
        let pos,gets = subst in
        let idx = index_of_name name in
        let get row =
          lift ftype (Map.find_exn row.(idx).fields fname) in
        (pos+1,get::gets), Var {attr=pos; field=fld} in
    let (_,gets),exp = index (0,[]) exp in
    let subs = Array.of_list_rev gets in
    let sub row {attr=off} = subs.(off) row in
    sub,exp

  let vars_of_join names scheme = function
    | {attr = Some name; field} -> [{attr = Name name; field}]
    | {attr = None; field} ->
      List.filter_mapi names ~f:(fun pos name ->
          match Map.find scheme name with
          | None -> None
          | Some sign ->
            if List.mem sign field ~equal:Type.header_equal
            then Some {attr = Pos pos; field }
            else None)

  let unify_class names scheme cls =
    match List.concat_map cls ~f:(vars_of_join names scheme) with
    | [] | [_] -> True
    | x::y::vs ->
      List.fold vs ~init:Syntax.(var x = var y) ~f:(fun cs z ->
        Syntax.(cs && var x = var z))

  let unify names scheme joins =
    List.fold joins ~init:True ~f:(fun cs cls ->
        Syntax.(cs && unify_class names scheme cls))


  let eval names joins exp {Doc.scheme; entries} : row seq =
    let module Error = Monad.Result.Error in
    let open Error.Syntax in
    let cs = unify names scheme joins in
    let sub,exp = index_vars names Syntax.(cs && exp) in
    let exp = simpl exp in
    if has_all_attributes names entries
    then n_cross_product (select names entries) |>
         Seq.filter_map ~f:(fun row ->
             let row = Seq.map row ~f:snd |> Seq.to_array in
             if Array.length row > 0 && sat (sub row) exp
             then Some {row}
             else None)
    else Seq.empty
  include Syntax
end


module Query = struct
  type exp = Exp.t
  type join = string option column
  type 'f tables = {
    arity : int;
    names : string list;
    read : row -> 'f;
  }

  type 'f t = {
    where : exp;
    join : join list list;
    tables : 'f tables;
  }

  let get_exn {row} attr pos =
    let {Attribute.name; read} = attr () in
    match read (Array.get row pos) with
    | None -> failwithf "can't parse attribute %s" name ()
    | Some x -> x


  let from attr = {
    names = [Attribute.name attr];
    arity = 1;
    read = fun row k -> k (get_exn row attr 0)
  }

  let ($) {read; names; arity} attr = {
    names = names @ [Attribute.name attr];
    arity = arity + 1;
    read = fun row k -> read row k (get_exn row attr arity)
  }

  let header_of_field {Type.name=fname; t={Type.typ}} =
    {Type.fname; ftype = typ}

  module Array = struct
    let get attr field =
      let {Attribute.name=aname} = attr () in
      Exp.var {attr=Exp.Name aname; field = header_of_field field}
  end

  module String = struct
    let get field p =
      Exp.var {attr = Exp.Pos p; field = header_of_field field}
  end

  let field ?from fld = {
    attr = Option.map from ~f:(fun attr ->
        let {Attribute.name} = attr () in name);
    field = header_of_field fld;
  }

  let select ?(where=Exp.True) ?(join=[]) tables = {
    where; join; tables;
  }

  include Exp.Syntax
end

type 'a query = 'a Query.t


module type S = sig
  include Monad.S
  include Monad.Trans.S with type 'a t := 'a t
  val require : ?that:('a -> bool) -> ('a,_) attribute -> 'a t
  val request : ?that:('a -> bool) -> ('a,_) attribute -> 'a option t
  val foreach : ('a -> 'b t) query -> f:'a -> 'b seq t
  val provide : (_, 'a -> unit t) attribute -> 'a
  val fail : Error.t -> 'a t
  val failf : ('a, formatter, unit, unit -> 'b t) format4 -> 'a
  val eval : 'a t -> doc -> 'a  Or_error.t m
  val exec : 'a t -> doc -> doc Or_error.t m
  val run : 'a t -> doc -> ('a * doc) Or_error.t m
end

module Make(B : Monad.S) = struct
  type state = {
    doc : Doc.t;
  }

  module EM = struct
    type 'a t = 'a Or_error.t B.t
    include Monad.Result.Error.Make(B)
  end

  module M = Monad.State.Make(Doc)(EM)
  open M.Syntax

  type 'a monad = 'a Monad.State.T1(Doc)(EM).t
  type 'a t = 'a monad
  type 'a m = 'a B.t
  type 'a e = doc -> ('a * doc) Or_error.t m

  let failf fmt =
    let buf = Buffer.create 512 in
    let ppf = formatter_of_buffer buf in
    let kon ppf () =
      pp_print_flush ppf ();
      let err = Or_error.error_string (Buffer.contents buf) in
      (M.lift (B.return err)) in
    kfprintf kon ppf fmt


  let foldm xs ~init ~f =
    Sequence.delayed_fold xs ~init
      ~f:(fun s a ~k -> f s a >>= k)
      ~finish:M.return


  let foreach {Query.where; join; tables={Query.read; names}} ~f
    : 'b seq t =
    M.get () >>= fun doc -> match Exp.eval names join where doc with
    (* | Error err -> M.lift (B.return (Error err)) *)
    | rows ->
      foldm rows ~init:[] ~f:(fun xs row -> read row f >>| fun x -> x :: xs) >>|
      Sequence.of_list
      (* M.Seq.map rows ~f:(fun row -> read row f) *)

  let require ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "required attribute %s" (Attribute.name attr) in
    M.get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (B.return (Error err))
    | Ok [] -> failf "%s is not provided" name ()
    | Ok xs -> match List.filter ~f:that xs with
      | [x] ->  M.lift (B.return (Ok x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambiguous" name ()

  let request ?(that=fun _ -> true) attr : 'a option t =
    let name = sprintf "requested attribute %s" (Attribute.name attr) in
    M.get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (B.return (Error err))
    | Ok [] -> M.lift (B.return (Ok None))
    | Ok xs -> match List.filter ~f:that xs with
      | [x] ->  M.lift (B.return (Ok (Some x)))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambiguous" name ()

  let provide (attr : (_, 'a -> unit monad) attribute) : 'a =
    Doc.put (fun save ->
        M.get () >>= fun doc -> match save doc with
        | Error err -> failf "failed to save an attribute" ()
        | Ok doc -> M.put doc) attr

  include M

  let fail err = M.lift (B.return (Error err))
  let liftm  = B.map ~f:(fun x -> (Ok x))
  let lift x = lift (liftm x)
  let take f m = B.map m ~f:(Or_error.map ~f)
  let eval m env = take fst (run m env)
  let exec m env = take snd (run m env)
end

include Make(Monad.Ident)
