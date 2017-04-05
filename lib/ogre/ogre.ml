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

  type signature = {
    arity : int;
    names : string list;
    grammar : string list;
  } [@@deriving compare]

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
    syntax = "INT";
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
    signature = {
      arity = 1;
      names = [field.name];
      grammar = [field.t.syntax];
    };
    save = (fun k x -> k (add field x empty));
    read = fun ent k -> match get ent field with
      | None -> None
      | Some x -> Some (k x)
  }

  let ($)
    = fun
      {read; save; signature={arity;grammar;names}} fld -> {
    signature = {
      arity = arity + 1;
      grammar = [fld.t.syntax] @ grammar;
      names = [fld.name] @ names;
    };
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
  type ('a,'k) t = {
    name : string;
    desc : string;
    sign : Type.signature;
    read : entry -> 'a option;
    save : (entry -> 'd) -> 'c
  } constraint 'k = 'c -> 'd


  let define  ~desc ~name {Type.signature; read; save} cons = {
    name; desc; sign = signature;
    read = (fun entry -> read entry cons);
    save = save;
  }

  let pack {save} x = save x
end

module Spec = struct
  module ErrM = Monad.Result.Error
  open ErrM.Syntax

  type t = {
    scheme  : Type.signature String.Map.t;
    entries : entry list String.Map.t;
  }

  let empty = {
    scheme = String.Map.empty;
    entries = String.Map.empty;
  }

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

  let put k {Attribute.name; save; sign} =
    save (fun packed ->
        k (fun {scheme; entries} ->
            match update_scheme scheme name sign with
            | Error happens -> Error happens
            | Ok scheme -> Ok {scheme; entries = add entries name packed}))

end

module Example = struct
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

  (* let pack_entry_point = Spec.put ident entry_point *)

  let packed = Spec.put ident (entry_point ()) 0xDEADBEEFL false "hope" Spec.empty

  (* let packed = make_entry_point 0xDEADBEEFL false "hope" *)
end


type spec = Spec.t

module Monad = struct
  module Choice_or_error = Monad.Option.Make(Monad.Result.Error)
  module M = Monad.State.Make(Spec)(struct
      type 'a t = 'a option Or_error.t
      include Choice_or_error
    end)
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

  (* let require ?(that=fun _ -> true) (attr : 'a attribute) : 'a t = *)
  (*   get () >>= fun spec -> match Spec.get spec attr with *)
  (*   | [] -> failf "required attribute %s is not provided" attr.name () *)
  (*   | [x] -> M.lift (Ok (Some x)) *)
  (*   | _  -> failf "required attribute %s was provided multiple times" *)
  (*             attr.name () *)

  (* let collect (attr : 'a attribute) : 'a list t = *)
  (*   get () >>= fun spec -> M.lift (Ok (Some (Spec.get spec attr))) *)


  let provide attr =
    Spec.put (fun save ->
        get () >>= fun spec -> match save spec with
        | Error err -> failf "failed to save an attribute" ()
        | Ok spec -> put spec) (attr ())

  open Example

  let save_entry_point () : unit t =
    provide entry_point 0xDEADBEEFL false "hope" >>= fun () ->
    provide comment 0xDEADBEAFL "here" >>= fun () ->
    return ()

end
