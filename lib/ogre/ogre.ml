open Core_kernel.Std
open Monads.Std
open Format

type entry = {
  fields : string String.Map.t
} [@@deriving compare]


(* module Writer = struct *)
(*   type 'a t = {pack : 'a -> string} *)

(*   type packed = string list *)

(*   type ('a,'b) writer = { *)
(*     write : (packed -> 'b) -> 'a; *)
(*   } *)

(*   let scheme t = { *)
(*     write = fun k x -> k [t.pack x]; *)
(*   } *)

(*   let ($) scm t = { *)
(*     write = fun k -> scm.write (fun pkg x -> k (pkg @ [t.pack x])) *)
(*   } *)

(*   let run {write} = write ident *)


(*   let int = {pack = string_of_int} *)
(*   let bool = {pack = string_of_bool} *)

(*   ;; *)
(*   run (scheme int $bool) 42 false *)
(* end *)



module Type = struct
  type 'a t = {
    parse : string -> 'a option;
    pack : 'a -> string;
    compare : 'a -> 'a -> int;
    syntax : string;
  }

  module Writer = struct

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

  type 'a field = {
    t : 'a t;
    name : string;
  }

  type signature = {
    arity : int;
    names : string list;
    grammar : string list;
  } [@@deriving compare]

  type ('k, 'f) scheme = {
    read : entry -> 'a -> 'b option;
    save : 'k Writer.t;
    signature : signature;
  } constraint 'f = 'a -> 'b

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
    save = Writer.scheme field.t;
    read = fun ent k -> match get ent field with
      | None -> None
      | Some x -> Some (k x)
  }

  let ($) {read; save; signature={arity;grammar;names}} fld = {
    signature = {
      arity = arity + 1;
      grammar = [fld.t.syntax] @ grammar;
      names = [fld.name] @ names;
    };
    save = Writer.(save $fld.t);
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

module Attribute = struct
  type 'a t = {
    name : string;
    desc : string;
    sign : Type.signature;
    read : entry -> 'a option;
  }

  let define ~desc ~name {Type.signature; read} cons = {
    name; desc;
    sign = signature;
    read = (fun entry -> read entry cons);
  }
end

(* module Spec = struct *)
(*   module ErrM = Monad.Result.Error *)
(*   open ErrM.Syntax *)

(*   type t = { *)
(*     scheme  : Type.signature String.Map.t; *)
(*     entries : entry list String.Map.t; *)
(*   } *)

(*   let reduce_fields fields = *)
(*     List.sort fields ~cmp:compare_entry |> *)
(*     List.remove_consecutive_duplicates *)
(*       ~equal:(fun x y -> compare x y = 0) *)

(*   let update_scheme scheme {Attribute.name;sign} = *)
(*     match Map.find scheme name with *)
(*     | Some s when Type.compare_signature s sign <> 0 -> *)
(*       Type.signature_mismatch ~expect:s ~got:sign *)
(*     | _ -> Ok (Map.add scheme ~key:name ~data:sign) *)


(*   let update_contents entries {Attribute.name} value = *)
(*     Ok entries *)

(*   let put {scheme; entries} attr value = *)
(*     update_scheme scheme attr >>= fun scheme -> *)
(*     update_contents entries attr value >>= fun entries -> *)
(*     Ok {scheme; entries} *)


(* end *)

module Example = struct
  let address = Type.("address" %: int)
  let flag = Type.("flag" %: bool)
  let comm = Type.("comm" %: string)

  type address = Addr of int64 * bool * string [@@deriving variants]


  let scheme = Type.(scheme address $flag $comm)


  let entry_point =
    Attribute.define
      ~desc:"executable entry-point"
      ~name:"entry-point"
      Type.(scheme address $flag $comm)
      addr


end

(* module Spec =a struct *)
(*   type t = entry list String.Map.t *)
(*   let put t {to_sexp; name} x = Map.add_multi t ~key:name ~data:(to_sexp x) *)
(*   let get t {of_sexp; name} = match Map.find t name with *)
(*     | None -> [] *)
(*     | Some xs -> List.map ~f:of_sexp xs *)
(* end *)

(* type spec = Spec.t *)

(* module Monad = struct *)
(*   module Choice_or_error = Monad.Option.Make(Monad.Result.Error) *)
(*   module M = Monad.State.Make(Spec)(struct *)
(*       type 'a t = 'a option Or_error.t *)
(*       include Choice_or_error *)
(*     end) *)
(*   include M *)

(*   type 'a m = 'a option Or_error.t *)
(*   type ('a,'e) storage = ('a,'e) Monad.State.storage *)
(*   type ('a,'e) state = ('a,'e) Monad.State.state *)
(*   type 'a t = (('a,spec) storage m, spec) state *)

(*   let failf fmt = *)
(*     let buf = Buffer.create 512 in *)
(*     let ppf = formatter_of_buffer buf in *)
(*     let kon ppf () = *)
(*       pp_print_flush ppf (); *)
(*       M.lift (Or_error.error_string (Buffer.contents buf)) in *)
(*     kfprintf kon ppf fmt *)

(*   let require ?(that=fun _ -> true) (attr : 'a attribute) : 'a t = *)
(*     get () >>= fun spec -> match Spec.get spec attr with *)
(*     | [] -> failf "required attribute %s is not provided" attr.name () *)
(*     | [x] -> M.lift (Ok (Some x)) *)
(*     | _  -> failf "required attribute %s was provided multiple times" *)
(*               attr.name () *)

(*   let collect (attr : 'a attribute) : 'a list t = *)
(*     get () >>= fun spec -> M.lift (Ok (Some (Spec.get spec attr))) *)

(*   let provide (attr : 'a attribute) x : unit t = *)
(*     update (fun s -> Spec.put s attr x) *)

(* end *)
