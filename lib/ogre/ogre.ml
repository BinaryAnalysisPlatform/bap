open Core_kernel.Std
open Monads.Std
open Format

type entry = {
  fields : string String.Map.t
} [@@deriving compare]

type row = {row : entry String.Map.t}
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

  let string = {
    parse = atom string_of_sexp;
    pack = pack sexp_of_string;
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
    fields =
      Map.add fields ~key:name ~data:(t.pack x)
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
    signature = [{
        fname = fld.name;
        ftype = fld.t.typ;
      }] @ signature ;
    save = (fun k -> save (fun ent x -> k (add fld x ent)));
    read = fun ent k -> match read ent k with
      | None -> None
      | Some k -> match get ent fld with
        | None -> None
        | Some x -> Some (k x)
  }

  let pack t x = t.pack x

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
    desc : string;
    sign : Type.signature;
    read : entry -> 'a option;
    save : (entry -> 'd) -> 'c
  } constraint 'k = 'c -> 'd


  type ('a,'k) t = unit -> ('a,'k) info

  let define  ~desc ~name {Type.signature; read; save} cons = {
    name; desc; sign = signature;
    read = (fun entry -> read entry cons);
    save = save;
  }

  let pack t x = (t ()).save x
  let name t = (t ()).name
end

type ('a,'k) typeinfo = ('a,'k) Attribute.info


module Doc = struct
  module Parse = Monad.Result.Error
  open Parse.Syntax

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

  let update_entries name packed doc = Ok {
      doc with entries =
                  Map.add_multi doc.entries ~key:name ~data: packed
    }

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
      Parse.List.map (tabulate values) ~f:(function
          | pos, `Positional x -> positional header pos x
          | _, `Named (fnam,x) -> named header fnam x) >>=
      String.Map.of_alist_or_error

  let parse_entry doc = function
    | Sexp.List (Sexp.Atom "declare" :: Sexp.Atom name :: flds) ->
      Parse.List.map ~f:parse_fdecl (tabulate flds) >>= fun sign ->
      update_scheme name sign  doc
    | Sexp.List (Sexp.Atom "declare" :: _) ->
      errorf "expected (declare <attribute-name> <field-decls>)"
    | Sexp.List (Sexp.Atom name :: flds) ->
      Parse.List.map flds ~f:parse_fdefn >>=
      nest_defn doc.scheme name >>= fun fields ->
      update_entries name {fields} doc
    | _ -> errorf "expected <ogre-entry> ::= \
                  | (declare <attribute-name> <field-decls>)\
                  | (<attribute-name> <attribute-value>)"

  let of_sexps =
    Parse.List.fold ~init:empty ~f:parse_entry

  let sexp_of_header {Type.fname; ftype} =
    let tname = Type.string_of_typ ftype in
    if String.for_all fname ~f:(Char.is_digit)
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

  let from_file name =
    Or_error.try_with_join (fun () -> In_channel.with_file name ~f:load)

  let save doc ch =
    let ppf = formatter_of_out_channel ch in
    pp ppf doc;
    pp_print_flush ppf ()

  let to_file doc name =
    Or_error.try_with @@ fun () -> Out_channel.with_file name ~f:(save doc)

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
  type bop = And | Or | Lt | Imp | Add | Sub [@@deriving sexp]
  type uop = Not [@@deriving sexp]
  type var = string column [@@deriving compare, sexp]
  type exp =
    | True
    | Int of int64
    | Str of string
    | Flt of float
    | Var of var
    | Bop of bop * exp * exp
    | Uop of uop * exp
  [@@deriving sexp]
  type t = exp [@@deriving sexp]

  module Vars = Map.Make(struct
      type t = var [@@deriving compare,sexp]
    end)

  let bool = function
    | true -> True
    | false -> Uop (Not,True)

  let less compare x y = bool (compare x y < 0)

  let rec simpl = function
    | Bop (And,Uop (Not,True),_)
    | Bop (And,_,Uop (Not,True)) -> Uop (Not,True)
    | Bop (And,True,e)
    | Bop (And,e,True) -> simpl e
    | Bop (Or, Uop (Not,True),e)
    | Bop (Or, e, Uop (Not,True)) -> simpl e
    | Bop (Or, True,_)
    | Bop (Or, _,True) -> True
    | Bop (Imp, True, x) -> simpl x
    | Bop (Imp, Uop (Not,True),_) -> True
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
    | Uop (op,e) -> Uop (op, simpl e)
    | Bop (op,x,y) -> Bop (op,simpl x, simpl y)
    | e -> e

  module Syntax = struct
    let bool = bool
    let var x = Var x
    let int x = Int x
    let float x = Flt x
    let (&&) x y = simpl (Bop (And,x,y))
    let (||) x y = simpl (Bop (Or,x,y))
    let (==>) x y = simpl (Bop (Imp,x,y))
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

  let linearlize map =
    Map.to_sequence map |>
    Seq.map ~f:(fun (name,values) ->
        Seq.of_list values |>
        Seq.map ~f:(fun value -> name,value))

  let rec n_cross_product xs =
    let open Seq in match next xs with
    | None -> singleton empty
    | Some (row,rows) ->
      row >>= fun elt ->
      n_cross_product rows >>|
      append (singleton elt)

  let cross map =
    linearlize map |> n_cross_product

  let parser {Type.parse; typ} inj =
    typ, fun value -> match parse value with
    | None -> failwith "expression parse error"
    | Some x -> inj x

  let parsers = [
    parser Type.int (fun x -> Int x);
    parser Type.string (fun x -> Str x);
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
    Seq.fold row ~init:True ~f:(fun exp (name,value) ->
        Seq.filter_map columns ~f:(lookup_var name value) |>
        Seq.next |> function
        | None -> exp
        | Some (r,es) -> Seq.fold es ~init:exp ~f:(fun e p ->
            simpl Syntax.(e && p = r)))

  (* build a constraint that will unify all equivalence classes *)
  let unify joins row =
    Seq.of_list joins |>
    Seq.fold ~init:True ~f:(fun exp cls ->
        simpl Syntax.(exp && unify_var cls row))


  (* @pre: all attributes are unique
     @pre: variables are type checked
     @pre: the database is typechecked
     @pre: row degree is greater than zero
  *)
  let bind_vars vars row =
    List.fold vars ~init:Vars.empty ~f:(fun vars var ->
        let {field={Type.fname; ftype}; attr} = var in
        Seq.find_map row ~f:(fun (name,{fields}) ->
            if name <> attr then None
            else match Map.find fields fname with
              | None -> failwithf "%s doesn't have %s" name fname ()
              | Some v -> Some (lift ftype v)) |> function
        | None -> failwithf "didn't find attribute %s" attr ()
        | Some x -> Map.add vars ~key:var ~data:x)

  let rec subst bind = function
    | True | Int _ | Str _ | Flt _ as x -> x
    | Uop (op,x) -> simpl (Uop (op, subst bind x))
    | Bop (op,x,y) -> simpl (Bop (op,subst bind x,subst bind y))
    | Var n -> match bind n with
      | None -> Var n
      | Some e -> subst bind e

  let sat e = match simpl e with
    | True -> Some true
    | Uop (Not,True) -> Some false
    | _ -> None

  let rec vars : exp -> var list = function
    | True | Int _ | Str _ | Flt _ -> []
    | Var v -> [v]
    | Bop (_,x,y) -> vars x @ vars y
    | Uop (_,x) -> vars x



  let eval joins exp {Doc.scheme; entries} : row seq Or_error.t  =
    let module Error = Monad.Result.Error in
    let open Error.Syntax in
    let errorf msg = Or_error.errorf msg in
    let vars = vars exp in
    Error.Seq.filter (cross entries) ~f:(fun row ->
        let cs = unify joins row in
        let bs = bind_vars vars row in
        let exp = subst (Map.find bs) Syntax.(cs && exp) in
        match sat exp with
        | None -> errorf "bad expression"
        | Some r -> Ok r) >>=
    Error.Seq.map ~f:(fun attrs ->
        let init = {row=String.Map.empty} in
        Error.Seq.fold ~init attrs ~f:(fun {row} (name,entry) ->
            match Map.find row name with
            | Some _ -> errorf "duplicate column"
            | None -> Ok {row = Map.add row ~key:name ~data:entry}))

  include Syntax
end


module Query = struct
  type exp = Exp.t
  type join = string option column
  type 'f tables = {
    read : row -> 'f;
  }

  type 'f t = {
    where : exp;
    join : join list list;
    tables : 'f tables;
  }

  let get_exn {row} attr =
    let {Attribute.name; read} = attr () in
    match Map.find row name with
    | None -> invalid_argf "internal error - missing table %s" name ()
    | Some entry -> match read entry with
      | None -> invalid_argf "internal error - broken table %s" name ()
      | Some x -> x


  let from attr = {
    read = fun row k -> k (get_exn row attr)
  }

  let (&) {read} attr = {
    read = fun row k -> read row k (get_exn row attr)
  }

  let header_of_field {Type.name=fname; t={Type.typ}} =
    {Type.fname; ftype = typ}

  let (@) field attr =
    let {Attribute.name=aname} = attr () in
    Exp.var {attr=aname; field = header_of_field field}

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

module Monad = struct
  type 'a seq = 'a Sequence.t
  type state = {
    doc : Doc.t;
  }

  module M = Monad.State.Make(Doc)(Monad.Result.Error)
  open M.Syntax

  type 'a m = 'a Or_error.t
  type ('a,'e) storage = ('a,'e) Monad.State.storage
  type ('a,'e) statem = ('a,'e) Monad.State.state
  type 'a t = (('a,doc) storage m, doc) statem

  let failf fmt =
    let buf = Buffer.create 512 in
    let ppf = formatter_of_buffer buf in
    let kon ppf () =
      pp_print_flush ppf ();
      M.lift (Or_error.error_string (Buffer.contents buf)) in
    kfprintf kon ppf fmt


  let foreach ({Query.where; join; tables} : ('a -> 'b t) Query.t) ~f
    : 'b seq t =
    M.get () >>= fun doc -> match Exp.eval join where doc with
    | Error err -> M.lift (Error err)
    | Ok rows -> M.Seq.map rows ~f:(fun row -> tables.Query.read row f)

  let require ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "required attribute %s" (Attribute.name attr) in
    M.get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> failf "%s is not provided" name ()
    | Ok xs -> match List.filter ~f:that xs with
      | [x] ->  M.lift (Ok x)
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let request ?(that=fun _ -> true) attr : 'a option t =
    let name = sprintf "requested attribute %s" (Attribute.name attr) in
    M.get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> M.lift (Ok None)
    | Ok xs -> match List.filter ~f:that xs with
      | [x] ->  M.lift (Ok (Some x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let provide (attr : (_,'b -> unit t) attribute) =
    Doc.put (fun save ->
        M.get () >>= fun doc -> match save doc with
        | Error err -> failf "failed to save an attribute" ()
        | Ok doc -> M.put doc) attr

  include M

  let eval m env = run m env |> Or_error.map ~f:fst
  let exec m env = run m env |> Or_error.map ~f:snd
end
