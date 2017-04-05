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
  type 'a t = {
    parse : string -> 'a option;
    pack : 'a -> string;
    compare : 'a -> 'a -> int;
    syntax : string;
  }

  type 'a field = {
    t : 'a t;
    name : string;
  }

  type header = {
    fname : string;
    tname : string;
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
    syntax = "INTEGER";
  }

  let bool = {
    parse = atom bool_of_sexp;
    pack = pack sexp_of_bool;
    compare = compare_bool;
    syntax = "BOOL";
  }

  let string = {
    parse = atom string_of_sexp;
    pack = pack sexp_of_string;
    compare = compare_string;
    syntax = "STRING";
  }

  let enum variants = {
    string with
    syntax = "(" ^ String.concat ~sep:" | " variants ^ ")";
  }

  let get {fields} f = Option.(Map.find fields f.name >>= f.t.parse)

  let add {name; t} x {fields} = {
    fields =
      Map.add fields ~key:name ~data:(t.pack x)
  }

  let empty = {fields = String.Map.empty}

  let scheme field = {
    signature = [{fname = field.name; tname = field.t.syntax}];
    save = (fun k x -> k (add field x empty));
    read = fun ent k -> match get ent field with
      | None -> None
      | Some x -> Some (k x)
  }

  let ($) {read; save; signature} fld = {
    signature = [{
        fname = fld.name;
        tname = fld.t.syntax;
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

module Spec = struct
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

  let update_scheme scheme name sign =
    match Map.find scheme name with
    | Some s when Type.compare_signature s sign <> 0 ->
      Type.signature_mismatch ~expect:s ~got:sign
    | _ -> Ok (Map.add scheme ~key:name ~data:sign)


  let add entries name packed =
    Map.add_multi entries ~key:name ~data:packed

  let put k attr =
    let {Attribute.name; save; sign} = attr () in
    save (fun packed ->
        k (fun {scheme; entries} ->
            match update_scheme scheme name sign with
            | Error happens -> Error happens
            | Ok scheme -> Ok {scheme; entries = add entries name packed}))


  let get {scheme;entries} attr =
    let {Attribute.name; read; sign} = attr () in
    match Map.find entries name with
    | None -> Ok []
    | Some entries -> Ok (List.filter_map ~f:read entries)


  let tabulate xs = List.mapi xs ~f:(fun i x -> (i,x))

  let parse_tname name =
    if List.mem ["integer"; "bool"; "string"] name
    then Ok name
    else errorf "expected <field-type> ::= integer | bool | string"

  let parse_fname name =
    if Char.is_alpha name.[0] &&
       String.for_all name ~f:(fun c ->
           Char.is_alphanum c || c = '-')
    then Ok name
    else errorf "bad field name %s, expected [a..z][a..z-]*" name


  let parse_fdecl = function
    | i, Sexp.Atom tname ->
      parse_tname tname >>| fun tname ->
      {Type.fname = sprintf "%d" i; tname}
    | _, Sexp.List [Sexp.Atom fname; Sexp.Atom tname] ->
      parse_fname fname >>= fun fname ->
      parse_tname tname >>| fun tname ->
      {Type.tname; Type.fname}
    | _ ->
      errorf "expected <decl> ::= \
              <field-type> | (<field-name> <field-type>)"

  let parse_decl = function
    | Sexp.List (Sexp.Atom "declare" :: Sexp.Atom name :: flds) ->
      Parse.List.map ~f:parse_fdecl (tabulate flds) >>| fun flds ->
      name,flds
    | _ -> errorf "expected (declare <field-decls>)"

  let parse_decls declarations =
    Parse.List.map declarations ~f:parse_decl >>=
    Parse.List.fold ~init:String.Map.empty ~f:(fun decls (name,sign) ->
        if Map.mem decls name
        then errorf "Multiple declarations of attirbute %s" name
        else Ok (Map.add decls ~key:name ~data:sign))

  let parse_fdefn = function
    | Sexp.Atom x -> Ok (`Positional x)
    | Sexp.List [Sexp.Atom fname; Sexp.Atom x] -> Ok (`Named (fname,x))
    | _ -> errorf "expected <attribute-value> ::= \
                  <field-value> | (<field-name> <field-value>)"

  let parse_defn = function
    | Sexp.List (Sexp.Atom name :: flds) ->
      Parse.List.map flds ~f:parse_fdefn >>| fun es -> name,es
    | _ ->
      errorf "expected <attribute> ::= \
             (<attribute-name> <attribute-value> ..)"

  let positional header pos x =
    match List.Assoc.find header pos with
    | None -> errorf "wrong arity"
    | Some {Type.fname} -> Ok (fname,x)

  let named hdr name x =
    List.find hdr ~f:(fun (_,{Type.fname}) -> name = fname) |> function
      | None -> errorf "unknown field %s" name
      | Some _ -> Ok (name,x)

  let parse_defns scheme defs =
    Parse.List.map defs ~f:parse_defn >>=
    Parse.List.fold ~init:String.Map.empty ~f:(fun entries (name,values) ->
        match Map.find scheme name with
        | None -> errorf "%s is not declared in the scheme" name
        | Some header ->
          let header = tabulate header in
          Parse.List.map (tabulate values) ~f:(function
            | pos, `Positional x -> positional header pos x
            | _, `Named (fnam,x) -> named header fnam x) >>=
          String.Map.of_alist_or_error >>| fun fields ->
          add entries name {fields})

  let of_sexp = function
    | Sexp.List [
        Sexp.List (Sexp.Atom "scheme" :: ds);
        Sexp.List (Sexp.Atom "body" :: bs)] ->
      parse_decls ds >>= fun scheme ->
      parse_defns scheme bs >>= fun entries ->
      !!{scheme; entries}
    | _ -> errorf "expected ((scheme <decls>) (body <defns>))"

  let dump_header {Type.fname; tname} =
    if String.for_all fname ~f:(Char.is_digit)
    then Sexp.Atom tname
    else Sexp.(List [Atom fname; Atom tname])

  let dump_signature = List.map ~f:dump_header

  let dump_decl (name,s) =
    Sexp.(List (Atom "declare" :: Atom name :: dump_signature s))

  let dump_scheme scheme =
    Map.to_alist scheme |> List.map ~f:dump_decl

  let dump_value scheme name {fields} =
    match Map.find scheme name with
    | None -> invalid_argf "can't find a header for attribute %s" name ()
    | Some sign ->
      List.map sign ~f:(fun {Type.fname} ->
          match Map.find fields fname with
          | None -> invalid_argf "malformed data base - \
                                  attribute %s misses field %s"
                      name fname ()
          | Some value -> Sexp.Atom value)

  let dump_attr scheme (name,values) =
    List.map values ~f:(fun v ->
        Sexp.(List (Atom name :: dump_value scheme name v)) )


  let dump_body scheme entries =
    Map.to_alist entries |> List.concat_map ~f:(dump_attr scheme)

  let to_sexp {scheme; entries} =
    Sexp.List [
      Sexp.List (Sexp.Atom "scheme" :: dump_scheme scheme);
      Sexp.List (Sexp.Atom "body" :: dump_body scheme entries)
    ]

  let load channel = of_sexp (Sexp.input_sexp channel)
  let save spec channel =  (Sexp.output_hum channel (to_sexp spec))
  let from_string str = of_sexp (Sexp.of_string str)
  let to_string x = Sexp.to_string_hum (to_sexp x)
  let pp ppf x = Sexp.pp_hum ppf (to_sexp x)

end

type ('a,'k) attribute = ('a,'k) Attribute.t

type spec = Spec.t

module Monad = struct
  module Choice_or_error = Monad.Option.Make(Monad.Result.Error)
  module M = Monad.State.Make(Spec)(struct
      type 'a t = 'a option Or_error.t
      include Choice_or_error
    end)

  module Lst = List
  include M

  type 'a m = 'a option Or_error.t
  type ('a,'e) storage = ('a,'e) Monad.State.storage
  type ('a,'e) state = ('a,'e) Monad.State.state
  type 'a t = (('a,spec) storage m, spec) state

  let failf fmt =
    let buf = Buffer.create 512 in
    let ppf = formatter_of_buffer buf in
    let kon ppf () =
      pp_print_flush ppf ();
      M.lift (Or_error.error_string (Buffer.contents buf)) in
    kfprintf kon ppf fmt

  let foreach ?(that=fun _ -> true) attr : 'a list t =
    get () >>= fun spec -> match Spec.get spec attr with
    | Error err -> M.lift (Error err)
    | Ok xs -> M.lift (Ok (Some (Lst.filter ~f:that xs)))

  let require ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "required attribute %s" (Attribute.name attr) in
    get () >>= fun spec -> match Spec.get spec attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> failf "%s is not provided" name ()
    | Ok xs -> match Lst.filter ~f:that xs with
      | [x] ->  M.lift (Ok (Some x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let request ?(that=fun _ -> true) attr : 'a t =
    let name = sprintf "requested attribute %s" (Attribute.name attr) in
    get () >>= fun spec -> match Spec.get spec attr with
    | Error err -> M.lift (Error err)
    | Ok [] -> M.lift (Ok None)
    | Ok xs -> match Lst.filter ~f:that xs with
      | [x] ->  M.lift (Ok (Some x))
      | [] -> failf "%s doesn't satisfy the constraint" name ()
      | _  -> failf "%s values are ambigious" name ()

  let provide (attr : (_,'b -> unit t) attribute) =
    Spec.put (fun save ->
        get () >>= fun spec -> match save spec with
        | Error err -> failf "failed to save an attribute" ()
        | Ok spec -> put spec) attr

  let run m spec =
    match run m spec with
    | Error err -> Error err
    | Ok None -> Or_error.errorf "result is empty"
    | Ok Some x -> Ok x

  let eval m spec = Or_error.map ~f:fst (run m spec)
  let exec m spec = Or_error.map ~f:snd (run m spec)


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

    let scheme1 = Type.(scheme address $flag)

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

  let packed = Spec.put ident entry_point 0xDEADBEEFL false "hope" Spec.empty


  let save_entry_point () : unit t =
    provide entry_point 0xDEADBEEFL false "hope" >>= fun () ->
    provide comment 0xDEADBEAFL "here" >>= fun () ->
    return ()

end
