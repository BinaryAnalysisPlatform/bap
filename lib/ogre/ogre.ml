open Core_kernel.Std
open Monads.Std
open Format

type entry = {
  fields : string String.Map.t
} [@@deriving compare]

module Writer = struct
  type 'a packer = {pack : 'a -> string}
  type packed = string list
  type 'c t = {
    write : (packed -> 'b) -> 'a;
  } constraint 'c = 'a -> 'b

  let scheme t = {
    write = fun k x -> k [t.pack x];
  }

  let ($) scm t = {
    write = fun k -> scm.write (fun pkg x -> k (pkg @ [t.pack x]))
  }

  let run {write} = write ident
end

module Type = struct
  type typ = Int | Str | Bool [@@deriving compare,enumerate]
  type 'a t = {
    parse : string -> 'a option;
    pack : 'a -> string;
    compare : 'a -> 'a -> int;
    typ : typ;
  }

  type 'a field = {
    t : 'a t;
    name : string;
  }

  type header = {
    fname : string;
    ftype : typ;
  } [@@deriving compare]

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
    compare = compare_int64;
    typ = Int;
  }

  let bool = {
    parse = atom bool_of_sexp;
    pack = pack sexp_of_bool;
    compare = compare_bool;
    typ = Bool;
  }

  let string = {
    parse = atom string_of_sexp;
    pack = pack sexp_of_string;
    compare = compare_string;
    typ = Str;
  }

  let string_of_typ = function
    | Int -> "int"
    | Str -> "str"
    | Bool -> "bool"

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

module Exp = struct
  type column = {attr : string option; field : string}
  type bop = And | Or | Lt | Imp | Add | Sub [@@deriving sexp]
  type uop = Not [@@deriving sexp]
  type exp =
    | True
    | Int of int64
    | Str of string
    | Var of string
    | Bop of bop * exp * exp
    | Uop of uop * exp
  [@@deriving sexp]

  let bool = function
    | true -> True
    | false -> Uop (Not,True)

  let var x = Var x
  let int x = Int x

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
    | Bop (Add,Int 0L,x)
    | Bop (Add,x,Int 0L) -> simpl x
    | Bop (Sub,x,Int 0L) -> simpl x
    | Bop (Add,Int x, Int y) -> Int Int64.(x + y)
    | Bop (Sub,Int x, Int y) -> Int Int64.(x - y)
    | Uop (Not,(Uop (Not,x))) -> simpl x
    | Uop (op,e) -> Uop (op, simpl e)
    | Bop (op,x,y) -> Bop (op,simpl x, simpl y)
    | e -> e

  module Syntax = struct
    let (&&) x y = simpl (Bop (And,x,y))
    let (||) x y = simpl (Bop (Or,x,y))
    let (==>) x y = simpl (Bop (Imp,x,y))
    let not x = simpl (Uop (Not,x))
    let (<) x y = simpl (Bop(Lt,x,y))
    let (=) x y = simpl (not (x < y) && not (y < x))
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

  let parser t inj value = match t.Type.parse value with
    | None -> None
    | Some x -> Some (inj x)

  let parsers = [
    parser Type.int (fun x -> Int x);
    parser Type.string (fun x -> Str x);
    parser Type.bool (function
        | true -> True
        | false -> Uop (Not,True))
  ]

  let lift x =
    List.find_map parsers ~f:(fun parse -> parse x) |> function
    | None -> invalid_argf "internal error - bad value" ()
    | Some x -> x

  let bind_attr name {fields} {attr; field} =
    printf "checking attribute %s@\n" name;
    if attr = Some name || attr = None
    then Map.find fields field |> Option.map ~f:lift
    else None

  let bind_var var columns row =
    Seq.concat_map row ~f:(fun (name,value) ->
        List.filter_map columns ~f:(bind_attr name value) |>
        Seq.of_list) |> Seq.to_list |> function
    | [] -> None
    | x :: xs ->
      let cs = simpl (List.fold xs ~init:True ~f:(fun e y ->
        Syntax.(e && x = y))) in
      Some (x, cs)

  let bind_vars vars row =
    List.fold vars ~init:(True,String.Map.empty)
      ~f:(fun (cs,bs) (var,cols) -> match bind_var var cols row with
        | None -> cs,bs
        | Some (v,cs') -> Syntax.(cs && cs'), Map.add bs ~key:var ~data:v)

  let rec subst bind = function
    | True | Int _ | Str _ as x -> x
    | Uop (op,x) -> simpl (Uop (op, subst bind x))
    | Bop (op,x,y) -> simpl (Bop (op,subst bind x,subst bind y))
    | Var n -> match bind n with
      | None -> Var n
      | Some e -> subst bind e

  let sat e = match simpl e with
    | True -> Some true
    | Uop (Not,True) -> Some false
    | _ -> None

  let eval vars exp {Doc.scheme; entries} =
    let module Error = Monad.Result.Error in
    let open Error.Syntax in
    Error.Seq.filter (cross entries) ~f:(fun row ->
        let cs,bs = bind_vars vars row in
        printf "inferred constraint: %a@\n"
          Sexp.pp_hum (sexp_of_exp cs);
        printf "applying substitution:@\n%a@\nto expression:@\n%a@\n"
          Sexp.pp_hum (String.Map.sexp_of_t sexp_of_exp bs)
          Sexp.pp_hum (sexp_of_exp Syntax.(cs && exp));
        let exp = subst (Map.find bs) Syntax.(cs && exp) in
        match sat exp with
        | None ->
          printf "%a@\n" Sexp.pp_hum (sexp_of_exp exp);
          Or_error.errorf "bad expression"
        | Some r -> Ok r)

end

module Monad = struct
  module Choice_or_error = Monad.Option.Make(Monad.Result.Error)
  module M = Monad.State.Make(Doc)(struct
      type 'a t = 'a option Or_error.t
      include Choice_or_error
    end)

  module Lst = List
  include M


  type 'a m = 'a option Or_error.t
  type ('a,'e) storage = ('a,'e) Monad.State.storage
  type ('a,'e) state = ('a,'e) Monad.State.state
  type 'a t = (('a,doc) storage m, doc) state

  let failf fmt =
    let buf = Buffer.create 512 in
    let ppf = formatter_of_buffer buf in
    let kon ppf () =
      pp_print_flush ppf ();
      M.lift (Or_error.error_string (Buffer.contents buf)) in
    kfprintf kon ppf fmt


  let foreach ?(that=fun _ -> true) attr : 'a list t =
    get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (Error err)
    | Ok xs -> M.lift (Ok (Some (Lst.filter ~f:that xs)))

  let require ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "required attribute %s" (Attribute.name attr) in
    get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> failf "%s is not provided" name ()
    | Ok xs -> match Lst.filter ~f:that xs with
      | [x] ->  M.lift (Ok (Some x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let request ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "requested attribute %s" (Attribute.name attr) in
    get () >>= fun doc -> match Doc.get doc attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> M.lift (Ok None)
    | Ok xs -> match Lst.filter ~f:that xs with
      | [x] ->  M.lift (Ok (Some x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let provide (attr : (_,'b -> unit t) attribute) =
    Doc.put (fun save ->
        get () >>= fun doc -> match save doc with
        | Error err -> failf "failed to save an attribute" ()
        | Ok doc -> put doc) attr

  let take f = Or_error.map ~f:(Option.map ~f)
  let eval m doc = take fst (run m doc)
  let exec m doc = take snd (run m doc)

end

module Example = struct
  open Monad

  module Attrs : sig
    type address
    type comment

    val comment : (comment, (int64 -> string -> 'a) -> 'a) attribute
    val entry_point : (address, (int64 -> bool -> string -> 'a) -> 'a) attribute

  end = struct
    let address = Type.("address" %: int)
    let flag = Type.("flag" %: bool)
    let comm = Type.("comm" %: string)

    type address = Addr of int64 * bool * string [@@deriving variants]
    type comment = Cmnt of int64 * string [@@deriving variants]

    (* scheme t1 $ t2 ... $tn *)
    let comment () =
      Attribute.define
        ~desc:"a comment to an address"
        ~name:"comment"
        Type.(scheme address $comm)
        cmnt

    let entry_point () =
      Attribute.define
        ~desc:"executable entry-point"
        ~name:"entry-point"
        Type.(scheme address $flag $comm)
        addr
  end

  open Attrs

  let packed = Doc.put ident entry_point 0xDEADBEEFL false "hope" Doc.empty


  let save_entry_point () : unit t =
    provide entry_point 0xDEADBEEFL false "hope" >>= fun () ->
    provide comment 0xDEADBEAFL "here" >>= fun () ->
    return ()

end


let doc = ok_exn (Doc.from_file "test.ogre")
let vars = Exp.["x", [
    {attr=Some "student"; field = "teacher"};
    {attr=Some "teacher"; field="id"}
  ]]

let test () = Exp.eval vars Exp.(Syntax.(var "x" = int 0L)) doc
