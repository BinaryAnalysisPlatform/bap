open Core_kernel.Std
open Bap_bundle.Std
open Bap.Std
open Bap_c.Std
include Self()
open Cabs
open Option.Monad_infix

let int size sign : C.Type.basic = match size,sign with
  | (NO_SIZE,(NO_SIGN|SIGNED)) -> `sint
  | (SHORT,(NO_SIGN|SIGNED)) -> `sshort
  | (LONG,(NO_SIGN|SIGNED)) -> `slong
  | (LONG_LONG,(NO_SIGN|SIGNED)) -> `slong_long
  | (NO_SIZE,UNSIGNED) -> `uint
  | (SHORT,UNSIGNED) -> `ushort
  | (LONG,UNSIGNED) -> `ulong
  | (LONG_LONG,UNSIGNED) -> `ulong_long

let char sign : C.Type.basic = match sign with
  | NO_SIGN  -> `char
  | SIGNED   -> `schar
  | UNSIGNED -> `uchar

let cv = C.Type.Qualifier.{
    const = false;
    volatile = false;
    restrict = ();
  }

let cvr = C.Type.Qualifier.{ cv with restrict = false}
let restricted cvr = C.Type.Qualifier.{cvr with restrict = true}
let no_qualifier = `no_qualifier

let spec qualifier t = C.Type.Spec.{t; attrs = []; qualifier}

let basic t = `Basic (spec cv t)
let pointer t = `Pointer (spec cvr t)
let restrict t = `Pointer (spec (restricted cvr) t)


let size = function
  | CONSTANT CONST_INT s -> Some (Int.of_string s)
  | _ -> None

let array size t = `Array (spec cvr (t,size))

let structure fields = `Structure (spec no_qualifier fields)
let union fields = `Union (spec no_qualifier fields)
let enum fields : C.Type.t = basic (`enum fields)
let func variadic return args =
  let args = match args with
    | [_,`Void] -> []
    | args -> args in
  `Function (spec no_qualifier C.Type.Proto.{
      return; args; variadic
    })

let name_groups s : name_group list -> 'a list =
  List.concat_map ~f:(fun (_,_,ns) ->
      List.map ns ~f:(fun (n,t,attrs,_) -> s^"."^n,t,attrs))

let single_names : single_name list -> 'a list =
  List.map ~f:(fun (_,s,(n,t,attrs,_)) -> n,t,attrs)

let rec gnu_attr = function
  | GNU_NONE -> None
  | GNU_CALL (name,args) -> Some C.Type.Attr.{
      name; args = gnu_attrs_args args
    }
  | GNU_ID s -> Some C.Type.Attr.{name = s; args = []}
  | GNU_CST _
  | GNU_EXTENSION
  | GNU_INLINE  -> None
and gnu_attrs_args = List.filter_map ~f:(function
    | GNU_ID s
    | GNU_CST
        (CONST_INT s|CONST_FLOAT s|CONST_CHAR s|CONST_STRING s) ->
      Some s
    | _ -> None)

let gnu_attrs = List.filter_map ~f:gnu_attr

let with_attrs attrs : C.Type.t -> C.Type.t =
  let add t = C.Type.Spec.{ t with attrs = t.attrs @ attrs} in
  function
  | `Void -> `Void
  | `Basic t -> `Basic (add t)
  | `Pointer t -> `Pointer (add t)
  | `Array t -> `Array (add t)
  | `Structure t -> `Structure (add t)
  | `Union t -> `Union (add t)
  | `Function t -> `Function (add t)

type qualifier = {
  apply : 'a 'b.('a C.Type.qualifier,'b) C.Type.spec ->
    ('a C.Type.qualifier,'b) C.Type.spec
}

let const = {
  apply = fun t -> C.Type.Spec.{
      t with qualifier = C.Type.Qualifier.{
      t.qualifier with const = true}}
}

let volatile = {
  apply = fun t -> C.Type.Spec.{
      t with qualifier = C.Type.Qualifier.{
      t.qualifier with volatile = true}
    }
}

let rec qualify f : C.Type.t -> C.Type.t =
  function
  | `Basic t -> `Basic (f.apply t)
  | `Pointer t -> `Pointer (f.apply t)
  | `Structure s -> `Structure C.Type.Spec.{
      s with t = List.map s.t ~f:(fun (n,t) -> n, qualify f t)
    }
  | x -> x

let field tag name = {C.Type.Field.tag; name}

type tag = {
  lookup : 'a. ('a list -> C.Type.t) -> string -> C.Type.t
}


let ctype gamma {lookup} t =
  let rec ctype : base_type -> C.Type.t = function
    | NO_TYPE | TYPE_LINE _ | OLD_PROTO _ | BITFIELD _ | VOID -> `Void
    | CHAR sign -> basic @@ char sign
    | INT (size,sign) -> basic @@ int size sign
    | FLOAT _ -> basic @@ `float
    | DOUBLE long -> basic @@ if long then `long_double else `double
    | PTR t -> pointer @@ ctype t
    | RESTRICT_PTR t -> restrict @@ ctype t
    | ARRAY (et,ice) -> array (size ice) @@ ctype et
    | STRUCT (n,[]) -> lookup structure n
    | UNION  (n,[]) -> lookup union n
    | ENUM   (n,[]) -> lookup enum n
    | STRUCT (n,fs) -> structure @@ fields (field n) (name_groups n fs)
    | UNION (n,fs) -> union @@ fields (field n) (name_groups n fs)
    | PROTO (r,args,v) -> func v (ctype r) @@ fields ident (single_names args)
    | NAMED_TYPE name -> gamma name
    | ENUM (name,fs) -> enum @@ enum_items name fs
    | CONST t -> qualify const @@ ctype t
    | VOLATILE t -> qualify volatile @@ ctype t
    | GNU_TYPE (a,t) -> with_attrs (gnu_attrs a) @@ ctype t
  and enum_items tag =
    List.map ~f:(fun (name,exp) -> match exp with
        | CONSTANT (CONST_INT x) -> field tag name, Some x
        | _ -> field tag name, None)
  and fields : 'a.  (string -> 'a) -> 'b -> ('a * 'c) list = fun field ->
    List.map ~f:(fun (name,t,a) ->
        field name, with_attrs (gnu_attrs a) @@ ctype t) in
  ctype t

let repr sizeof size types =
  List.find types ~f:(fun t ->
      sizeof#integer (t :> C.Type.integer) = size)

let is_signed = function
  | #C.Type.signed -> true
  | #C.Type.unsigned -> false


let parse (size : C.Size.base) parse lexbuf =
  let env = String.Table.create () in
  let tags = String.Table.create () in
  let gamma name = match Hashtbl.find env name with
    | Some t -> t
    | None -> `Void in
  let lookup what name = match Hashtbl.find tags name with
    | Some t -> t
    | None -> what [] in
  let add name t = Hashtbl.set env ~key:name ~data:t in
  let tag name t = Hashtbl.set tags ~key:name ~data:t in
  let typedef_int sz t =
    let bits = Size.in_bits sz in
    let sign = if is_signed t then "" else "u" in
    let name = sprintf "%sint%d_t" sign bits in
    Clexer.add_type name;
    add name (basic (t :> C.Type.basic)) in
  List.iter [`r8; `r16; `r32; `r64] ~f:(fun sz ->
      let def types =
        Option.iter (repr size sz types) ~f:(typedef_int sz) in
      def C.Type.all_of_unsigned;
      def C.Type.all_of_signed);
  let process = function
    | DECDEF (_,_,[name,t,a,_])
    | FUNDEF ((_,_,(name,t,a,_)),_)
    | TYPEDEF ((_,_,[name,t,a,_]),_)
    | ONLYTYPEDEF (_,_,[name,t,a,_]) ->
      add name (with_attrs (gnu_attrs a) (ctype gamma {lookup} t))
    | ONLYTYPEDEF (STRUCT (n,_) | UNION (n,_) | ENUM (n,_) as t,_,_) ->
      tag n (ctype gamma {lookup} t)
    |  _ -> () in
  List.iter ~f:process (parse lexbuf);
  Hashtbl.to_alist env

let parser size name =
  In_channel.with_file name ~f:(fun input ->
      let open Clexer in
      init {
        !current_handle with
        h_in_channel = input;
        h_file_name = name;
        h_out_channel = stderr;
      };
      init_lexicon ();
      let lexbuf = Lexing.from_function (get_buffer current_handle) in
      let parser = Cparser.file initial in
      try Ok (parse size parser lexbuf) with exn ->
        Or_error.of_exn exn)

let () = C.Parser.provide parser
