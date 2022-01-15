let doc = "
  # DESCRIPTION

  Applies semantic actions to the matching byte patterns. The patterns
  are specified in an XML format, described below, and the actions are
  implemented with Primus Lisp methods. Used to identify function
  starts, instruction encodings, function names, etc.

  # INPUT FORMAT

  The patterns are represented with XML files, each corresponding to a
  specific target. The $(b,patternconstraints.xml) files are used as
  the table of contents and contain mapping between targets and files
  that provide patterns for that target.

  The file scheme itself is derived from the Ghidra bytesearch
  patterns, so that the patterns provided by Ghidra could be used as
  is.

  # PATTERNS SCHEME

  The file with patterns must have a single element $(b, patternlist),
  which contains a list of $(b,pattern) or $(b,patternpairs)
  elements. The $(b,pattern) element is composed of a list of
  $(b,data) elements and a list of action elements. The $(b,data)
  element descibes the ditted pattern, and an action is any element
  (other than $(b,data)), which is translated to a Primus Lisp singal,
  $(b,bap:patterns-action name addr attrs), where $(b,name) is the
  element name, $(b,addr) is the address where the pattern matches,
  and $(b,attrs) is the list of element attributes, accessible via
  $(b,patterns-attribute) function.

  The $(b,patternspairs) element is very similar to the $(b,patterns)
  element except that the list of patterns is described by a cartesian
  product of two sets of patterns, $(b,prepatterns) and
  $(b,postpatterns). Both $(b,prepatterns) and $(b,postpatterns) must
  contain a non-empty list of $(b,data) elements and nothing more. The
  resulting patterns are made by concatenating every prepattern with
  every postpattern and leaving only those combinations that have the
  total number of non-masked bits equal to $(b,totalbits) and the
  total number of non-masked bits in the postpattern part equal to
  $(b,postbits). When the resulting pattern matches with a sequence of
  bytes, the address of a byte that matches with the start of the
  postpattern is passed to the $(b,bap:patterns-action) method.

  # PATTERNCONSTRAINTS SCHEME

  The $(b,patternconstraints.xml) file is used as the table of
  contents and contain a mapping between targets (languages in Ghidra
  parlance) and paths to corresponding files, relative to the location
  of the $(b,patternconstraints.xml) file.

  The file must have a single $(b,patternconstraints) element that
  contains a list of $(b,language) elements. The language element is
  required to have the $(b,id) attribute which must be either the name
  of a BAP target (see $(b,bap list targets)) or a Ghidra language
  specification, which is four-tuple of elements, separated with
  $(b,:). The first element is the architecture name, the second is
  the endiannes, the third is the bitness, and the last is the
  variant. Any field except the architecture, could use $(b,default)
  or just $(b,*) as the wildcard character that matches with
  anything. The endianness is specified as either $(b,LE) for little
  or $(b,BE) for big endianness. For instructions in little endian and
  data in big endian, use $(b,LEBE).

  The $(b,language) element contains a list of $(b,patternfile)
  or $(b,compiler) elements. The $(b,patternfile) element contains the
  path to the patterns file, and the $(b,compiler) element contains
  the $(b,patternfile) element with patterns specific to a compiler,
  which is specified in the required $(b,id) attribute of the
  $(b,compiler) element.


  # DITTED PATTERNS

  Each pattern is described as a ditted sequence of bits or nibbles.
  Each bit is represented with $(b,0), $(b,1), and $(b,.) that match,
  correspondigly with, zero, one, and any bit. And a nibble is a
  a hexadecimal digit, matching with their corresponding
  four-bit representation, or $(b,.), which matches with any four
  bits (i.e., with any binary number in the range from $(b,0) to
  $(b,0xF)). The nibble sequence must start with $(b,0x) and continues
  until the next whitespace character. If the sequence doesn't start
  with $(b,0x) then it is assumed to be a sequence of bits. Sequences
  could be separated by the arbitrary number of whitespace characters.

  # BUILTIN ACTIONS

  All actions in the $(b,bap) namespace, which is set as the default
  namespace when parsing the patterns file, are reserved to BAP. It
  is possible to add arbitrary actions, provided that they are not
  using the $(b,bap) namespace. The following set of actions have
  predefined meaning.

  $(b,functionstart) and $(b,possiblefuncstart) mark the matching
  sequence as the function start. The attributes could be used to
  impose an extra constraint. The current implementation ignore them,
  but they will be implemented later.

  $(b,setcontext) is used to control the disassembler context and
  currently the following two attributes are recongized, $(b,name)
  and $(b,value). When the name is set to $(b,TMode) then the matching
  sequence has the encoding T32 if the value is $(b,1) and A32
  otherwise.




"

open Core_kernel
open Bap_core_theory
open Bap.Std
open Bap_main
open Bap_primus.Std

module Sigma = Primus.Lisp.Semantics
module Lambda = Theory.Label


include Loggers()

module Name = struct
  type t = Xmlm.name
  let matches ?(ns="") what (uri,name) =
    [%equal : (string * string)] (ns,what) (uri,name)
end

module Attributes = struct
  type t = Xmlm.attribute list

  let lookup attrs attr = List.find_map attrs ~f:(fun (k,v) ->
      Option.some_if (Name.matches attr k) v)

  type 'a arg = t -> 'a option
  let str name : string arg = fun attrs -> lookup attrs name
  let int name : int arg = fun attrs ->
    match lookup attrs name with
    | None -> None
    | Some x -> try Some (int_of_string x) with _ -> None

  let ($) : ('a -> 'b) arg -> 'a arg -> 'b arg =
    fun argf arg attrs ->
    match argf attrs, arg attrs with
    | Some f, Some x -> Some (f x)
    | _,_ -> None

  let const : 'a -> 'a arg =
    fun x _ -> Some x
end

type name = Name.t
type attributes = Attributes.t



(** The parser combinator library for XML streams.

    A parser is stateful machine that recognizes a sequence of XML
    tokens (signals). A parser combinator is a function that takes
    several parsers and produces another parser. When a parser
    recognizes a sequence of tokens it generates a value, which
    witnesses that the parser was used in the derivation. A parser may
    recognize an empty sequence of tokens, i.e., it doesn't consume
    any input, e.g., [return ()] produces [()] without consuming any
    tokens. Values generated by parsers could be a parsers
    themselves, therefore the generated grammar is value-dependent and
    can recongize language that are describe with context-sensitive
    grammars.

    The XMLM source is destructive so we keep an extra state to keep
    track of the consumed tokens and to enable better error
    messages. Whether a parser is consuming a token or not is stored
    in its state. It is a dynamic property, since a parser could be
    combined from parsers that either consume or not. For example,

    {[
      let foo = element "foo" >>$ `foo
      let bar = return `bar
      let exp = foo <|> bar
    ]}

    when [exp] matches a sequence of tokens it could either use the
    [foo] or [bar] deriviation and either consume ([foo]) or not
    ([bar]) tokens. In the following example,

    {[
      let start = exp >>= function
        | `bar -> return 1
        | `foo -> return 2
    ]}

    the produced parser [start] will consume tokens if [foo] is
    in the derivation, despite that the parser was created with
    [return 2].

    The parser may fail because it doesn't recognize the sequence of
    tokens, the soft failure, or because a runtime error happened, the
    hard error. A soft error is normal during parsing and could be
    backtracked with [<|>] and other operators. The hard error can
    never be backtracked and immediately halts parsing.
    The runtime error can be generated with [error msg] or result from
    an error in the underlying source.
*)
module Parser : sig

  (** ['a t] is the parser that generates a value of type ['a].*)
  type 'a t

  (** the representation of the parsing error.  *)
  type error


  (** [p >>= f] creates context-dependent parser.

      If [p] succeeds with [x] then uses [f x] to create a new parser
      that depends on [x]. If it fails, then [f] is not called.

      The resulting grammar is [R ::= pq], where [q] is [f x].
  *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [return x] always succeeds and generates [x].

      The parser doesn't affect the state.
  *)
  val return : 'a -> 'a t

  (** [p >>| f] maps with [f] the result of [p].

      Creates a parser that returns [f x] if [p] succeeds with [x].

      The resulting grammar is [R := pq], where [q] is
      [return (f x)]. The [q] parser grammar is [ɛ], i.e., it matches
      with an empty string. *)
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  (** [p >> q] composes [p] and [q].

      the generated grammar is [pq] and the result of [p >> q] is the
      result of [q] (the result of [p] is ignored).
  *)
  val ( >> ) : 'a t -> 'b t -> 'b t

  (** [p << q] if [p] followed [q] succeeds, returns the result of [p] *)
  val ( << ) : 'a t -> 'b t -> 'a t

  (** [p >>$ x] if [p] succeeds returns [x]. *)
  val ( >>$ ) : 'a t -> 'b -> 'b t

  (** [p <|> q] if [p]   *)
  val ( <|> ) : 'a t -> 'a t -> 'a t

  (** [!*p] reconizes zero or more [p],

      see {!fold} for more details.*)
  val (!*) : 'a t -> 'a list t

  (** [!+p] recognizes one or more [p] *)
  val (!+) : 'a t -> 'a list t

  (** [star p = !*p]  *)
  val star : 'a t -> 'a list t

  (** [plus p = !+p]  *)
  val plus : 'a t -> 'a list t

  (** [many p = star p = !*p]  *)
  val many : 'a t -> 'a list t

  (** [ignore p = p >>$ ()] *)
  val ignore : 'a t -> unit t


  (** [choice [p; q; ...; z] is [p <|> q <|> ... z].  *)
  val choice : 'a t list -> 'a t

  (** [required p] rejects [p] if it's value is [None] *)
  val required : 'a option t -> 'a t

  (** [fold p f x] folds over the successive derivation of [p].

      Generates the longest possible derivation of [p] by trying it
      as long as it consumes input _and_ generates values.

      The resulting parser will consume input if at least one of the
      derivations of [p] was consuming. If [plus] is true and [p]
      didn't consume input (i.e., [f] was never called) then the
      derivation is rejected and [fold ~plus p f x] doesn't generate
      any value.

      The [fold] function is a generic function that is used to
      implement [star], [many], and [plus].
  *)
  val fold : ?plus:bool -> 'a t -> ('b -> 'a -> 'b) -> 'b -> 'b t

  (** XML-specific grammar elements  *)

  (** [dtd] recognizes DTD or its absence.

      Matches with the [`Dtd] signal or with an empty sequence.
      This parser never rejects any input.
  *)
  val dtd : Xmlm.dtd t

  (** [eoi] matches with the end of input.  *)
  val eoi : bool t

  (** [any] recognizes any XML tree.

      The recognized subtree is read (including all chidlren) and
      discarded.
  *)
  val any : unit t

  (** [tag name] recognizes tag with the given name.

      Recognizes the start-tag of an element with the given [name].
      Note, the empty-element tag is represented with two XML tokens
      (signals), the start-tag and the end-tag.

      Note, each start-tag should be accompanied with a corresponding
      end-tag, see {!close}. This parser only matches the start-tag.
  *)
  val tag : string -> attributes t



  (** [start] recognizes a start tag with any name.

      Produces the name of the recognized tag together with the
      attributes.
  *)
  val start : Xmlm.tag t


  (** [close] recognizes any end-tag. *)
  val close : unit t

  (** [tree name children] recognizes an element with [children].

      Recognizes an element with the given [name] that has contents
      that is recognized by [children]. Returns a pair composed of
      the attributes of the matched element and the result of the
      [children] parser.
  *)
  val tree : string -> 'a t -> (attributes * 'a) t

  (** [attr tag attr] recognizes an element with attribute.

      Recognizes and element with the given name that has an attribute
      named [attr], returns the value of that attribute.
  *)
  val attr : string -> string -> string t

  (** [data] recongizes the non-markup contents of an element. *)
  val data : string t

  (** [error msg] is a parser that stops parsing with the error [msg].  *)
  val error : string -> 'a t

  (** [run parser input] runs the [parser] on the specified [input]. *)
  val run : 'a t -> Xmlm.input -> ('a,error) result

  (** [pp_error ppf err] prints the parsing error.  *)
  val pp_error : Format.formatter -> error -> unit
end = struct
  type reject =
    | No_input
    | Unexpected of Xmlm.signal
    | User_error of string
    | Missing_attribute of {tag: string; attr : string}
    | Is_required

  type state = {
    input : Xmlm.input;
    empty : bool;
  }

  type 'a parser = state -> state * ('a,reject) result
  type 'a t = 'a parser

  type error = Xmlm.pos * reject

  let empty v s = {s with empty=true},v
  let unexpected signal = empty@@Error (Unexpected signal)
  let return x s = s,Ok x
  let const x s = {s with empty=true}, Ok x
  let fail err s = s,Error err
  let reject signal s = unexpected signal s

  let eof s = {s with empty=true},Error No_input

  let read : _ parser = fun s ->
    if Xmlm.eoi s.input then eof s
    else {s with empty=false},Ok (Xmlm.input s.input)

  let peek s =
    if Xmlm.eoi s.input then eof s
    else s,Ok (Xmlm.peek s.input)

  let (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser =
    fun p f s ->
    match p s with
    | _,Error _ as err -> err
    | s,Ok r -> f r s

  let (>>|) p f = p >>= fun x -> return (f x)

  let (>>) p q = p >>= fun _ -> q

  let ignore p = p >>| ignore

  let drop = ignore read
  let consumed r =
    drop >>= fun () -> return r

  let dtd = peek >>= function
    | `Dtd x -> consumed x
    | _ -> return None

  let start =
    peek >>= function
    | `El_start (name,attrs) ->
      consumed (name,attrs)
    | s -> reject s

  let close =
    peek >>= function
    | `El_end -> consumed ()
    | s -> reject s


  let tag name = peek >>= function
    | `El_start (tag,attrs) when Name.matches name tag ->
      consumed attrs
    | s -> reject s

  let eoi s = s, Ok (Xmlm.eoi s.input)

  let any s =
    let rec loop level s =
      if Xmlm.eoi s.input then s,Error No_input
      else match Xmlm.input s.input with
        | `Dtd _ | `Data _ -> loop level s
        | `El_end when level = 0 -> s, Ok ()
        | `El_end -> loop (level-1) s
        | `El_start _ -> loop (level+1) s in
    if Xmlm.eoi s.input then {s with empty=true}, Ok ()
    else match Xmlm.peek s.input with
      | `El_start _ ->
        let _ : Xmlm.signal = Xmlm.input s.input in
        loop 0 {s with empty=false}
      | other -> {s with empty=true}, Error (Unexpected other)


  let (>>$) p x = p >> return x
  let (<<) p q = p >>= fun x -> q >> return x


  let matches_attr name ((_,id),_) = String.equal name id

  let attr tname aname =
    tag tname >>= fun attrs ->
    match List.find attrs ~f:(matches_attr aname) with
    | None -> fail (Missing_attribute {tag=tname; attr=aname})
    | Some (_,v) -> return v

  let element name =
    tag name >>= fun attrs ->
    peek >>= function
    | `Data payload -> consumed (attrs,payload) << close
    | signal -> reject signal

  let data =
    peek >>= function
    | `Data payload -> consumed payload
    | signal -> reject signal

  let tree name children =
    tag name >>= fun attrs ->
    children >>= fun child ->
    return (attrs,child)
    << close


  let (<|>) : 'a parser -> 'a parser -> 'a parser = fun p q s ->
    match p {s with empty=true} with
    | _,Ok _ as ok -> ok
    | {empty=true},Error (Unexpected _|Is_required) -> q s
    | _,Error _ as err -> err


  let choice parsers =
    List.reduce_exn parsers ~f:(<|>)

  let fold ?(plus=false) : 'a parser -> ('b -> 'a -> 'b) -> 'b -> 'b parser =
    fun p f xs s ->
    let finish xs empty s =
      {s with empty},
      if empty && plus then Error Is_required else Ok xs in
    let rec loop xs empty s = match p s with
      | {empty=false} as s,Ok x ->
        loop (f xs x) false s
      | {empty=true} as s,Ok x -> finish (f xs x) empty s
      | {empty=true},Error (Unexpected _| Is_required|No_input) ->
        finish xs empty s
      | _,Error _ as err -> err in
    loop xs true {s with empty=true}

  let list_rev ?plus : 'a parser -> 'a list parser = fun p ->
    fold p ?plus (fun xs x -> x::xs) []

  let list ?plus p = list_rev ?plus p >>| List.rev

  let star s = list ~plus:false s
  let many s = star s
  let plus s = list ~plus:true s

  let (!*) = star
  let (!+) = plus

  let required p =
    p >>= function
    | None -> fail Is_required
    | Some x -> return x

  let error msg : 'a parser = fun s -> s,(Error (User_error msg))

  let run : 'a parser -> Xmlm.input -> _ =
    fun p s -> match p {input=s; empty=true} with
      | s,Error rej -> Error (Xmlm.pos s.input,rej)
      | _,(Ok _ as ok) -> ok

  let pp_error ppf ((lin,col),reject) =
    Format.fprintf ppf "Parser failed at line %d, column %d.@\n" lin col;
    match reject with
    | No_input -> Format.fprintf ppf "Unexpected end of input"
    | Unexpected (`El_start (name,_)) ->
      Format.fprintf ppf "An unexpected tag %a" Xmlm.pp_name name
    | Unexpected `El_end ->
      Format.fprintf ppf "An unexpected end-tag"
    | Unexpected `Dtd _ ->
      Format.fprintf ppf "An unexpected start of the document (DTD)"
    | Unexpected `Data data ->
      Format.fprintf ppf "An unexpected element content: %S" data
    | User_error msg ->
      Format.fprintf ppf "%s" msg
    | Missing_attribute {tag: string; attr : string} ->
      Format.fprintf ppf "The attribute %S of the tag '%s' is required"
        attr tag
    | Is_required ->
      Format.fprintf ppf "The element is required"

end

module Action : sig
  type t
  val of_tag : Xmlm.tag -> t
  val pp : Format.formatter -> t -> unit
  val name : t -> KB.Name.t
  val args : t -> unit Theory.Value.t
  val property : (Theory.program, t option) KB.slot
  module Library : sig
    val provide : unit -> unit
  end
  include Base.Comparable.S with type t := t
end = struct
  type action = {
    name : KB.Name.t;
    args : string Map.M(KB.Name).t;
  }
  [@@deriving compare, equal, sexp]

  let xml_name ~default (pkg,name) = match pkg with
    | "" -> KB.Name.create ~package:default name
    | _ -> match Uri.(host@@of_string pkg) with
      | None | Some "" -> KB.Name.create ~package:pkg name
      | Some package -> KB.Name.create ~package name

  let of_tag (name,args) = {
    name = xml_name ~default:"bap" name;
    args = List.map args ~f:(fun (k,v) ->
        xml_name ~default:KB.Symbol.keyword k,v) |>
           Map.of_alist_exn (module KB.Name)
  }

  let name action = action.name

  let slot = KB.Class.property Theory.Value.cls "action-attributes"
      ~package:"bap" @@ KB.Domain.mapping (module KB.Name)
      ~equal:String.equal "attributes"

  let property = KB.Class.property Theory.Program.cls "patterns-action"
      ~package:"bap" @@ KB.Domain.optional ~equal:equal_action "action"

  let args {args} =
    KB.Value.put slot Theory.Value.Top.empty args

  let pp_arg ppf (name,value) =
    Format.fprintf ppf "%a %S"
      KB.Name.pp name value

  let pp_args ppf args =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      pp_arg ppf (Map.to_alist args)

  let pp ppf {name; args} =
    if Map.is_empty args
    then Format.fprintf ppf "(%a)" KB.Name.pp name
    else Format.fprintf ppf "@[<hv2>(%a@ %a)@]"
        KB.Name.pp name pp_args args

  module Library = struct
    open KB.Syntax
    let select_attribute args name =
      let* name = name.?[Sigma.symbol] in
      let args = args.$[slot] in
      Sigma.Effect.return @@
      match Map.find args (KB.Name.read name) with
      | None -> Sigma.Value.nil
      | Some x -> Sigma.Value.symbol x

    let provide_primitive () =
      let types = Primus.Lisp.Type.Spec.(tuple [any; sym] @-> sym) in
      let docs = "(patterns-attribute ATTRS NAME) returns the \
                  attribute NAME of the attributes ATTRS or NIL if no \
                  such attribute is present" in
      Sigma.declare ~types ~docs ~package:"bap" "patterns-attribute"
        ~body:(fun _ -> KB.return @@ fun _lbl args -> match args with
          | [args; name] -> select_attribute args name
          | _ -> Sigma.failp "expects two arguments")

    let provide_signal () =
      let params = Primus.Lisp.Type.Spec.(tuple [sym; int; any]) in
      let docs = "(patterns-action ACTION ADDR ATTRS) is signaled \
                  when ACTION matches at ADDR. The ATTRS is key-value \
                  set of attributes accessible with patterns-attribute" in
      Sigma.signal ~params ~docs property @@ fun lbl action ->
      let action = Option.value_exn action in
      let* addr = lbl-->?Theory.Label.addr in
      KB.return [
        Sigma.Value.symbol (KB.Name.show (name action));
        Sigma.Value.static addr;
        args action;
      ]

    let provide () =
      provide_primitive ();
      provide_signal ()
  end

  type t = action
  include Base.Comparable.Make(struct
      type nonrec t = action [@@deriving compare, sexp]
    end)
end

module Pattern : sig
  type t

  val create : string -> t
  val empty : t
  val equal : t -> t -> bool
  val concat : t -> t -> t
  val length : t -> int
  val weight : t -> int
  val matches : t -> pos:int -> int -> bool
  val pp : Format.formatter -> t -> unit
end = struct
  module Parser = struct
    type mode = Start | Wait | Bin | Hex

    type state = {
      bits : Z.t;
      mask : Z.t;
      size : int;
      mode : mode;
    }

    let init = {
      bits = Z.zero;
      mask = Z.zero;
      size = 0;
      mode = Start
    }

    let switch mode s = {s with mode}

    let bit bit s = {
      mode = Bin;
      size = s.size + 1;
      bits = Z.(s.bits lsl 1 lor of_int bit);
      mask = Z.(s.mask lsl 1 lor one);
    }

    let mask_bit s = {
      mode = Bin;
      size = s.size + 1;
      bits = Z.(s.bits lsl 1);
      mask = Z.(s.mask lsl 1)
    }

    let nib x s = {
      mode = Hex;
      size = s.size + 4;
      bits = Z.(s.bits lsl 4 lor of_string_base 16 (String.of_char x));
      mask = Z.(s.mask lsl 4 lor of_int 0xf)
    }

    let mask_nib s = {
      mode = Hex;
      size = s.size + 4;
      bits = Z.(s.bits lsl 4);
      mask = Z.(s.mask lsl 4)
    }

    let seq s f =
      List.fold f ~init:s ~f:(fun s push -> push s)

    let run = String.fold ~init ~f:(fun s c -> match s.mode,c with
        | Start, '1' -> bit 1 s
        | Start, '0' -> switch Wait s
        | Start, '.' -> mask_bit s
        | Start, _ -> s
        | Wait, '0' -> seq s [bit 0; bit 0]
        | Wait, '1' -> seq s [bit 0; bit 1]
        | Wait, 'x' -> switch Hex s
        | Wait, _ -> switch Start s
        | Bin, '0' -> bit 0 s
        | Bin, '1' -> bit 1 s
        | Bin, '.' -> mask_bit s
        | Bin, _ -> switch Start s
        | Hex, ' ' -> switch Start s
        | Hex, '.' -> mask_nib s
        | Hex, '_' -> s
        | Hex, x -> nib x s)
  end


  type t = {
    repr : string;
    bits : int array;
    mask : int array;
    pops : int;
    size : int;
  } [@@deriving equal]

  type field = t -> Z.t

  let nth_byte size x n =
    let off = (size / 8 - n - 1) * 8 in
    Z.to_int @@
    Z.(x asr off land of_int 0xff)

  let create repr =
    let {Parser.size; bits; mask} = Parser.run repr in
    let pops = Z.popcount mask in
    let bits = Array.init (size/8) ~f:(nth_byte size bits) in
    let mask = Array.init (size/8) ~f:(nth_byte size mask) in
    {bits; mask; size; pops; repr}

  let empty = {
    repr = "";
    bits = [||];
    mask = [||];
    pops = 0;
    size = 0;
  }

  let bits x = x.bits
  let mask x = x.mask
  let length x = x.size / 8
  let weight x = x.pops


  let concat x y = {
    repr = x.repr ^ y.repr;
    bits = Array.append x.bits y.bits;
    mask = Array.append x.mask y.mask;
    pops = x.pops + y.pops;
    size = x.size + y.size;
  }


  let matches {mask; bits} ~pos:n data =
    data land mask.(n) = bits.(n)

  let pp ppf {repr} =
    Format.fprintf ppf "%s" repr
end

module Target = struct

  (* None denotes any *)
  type spec = {
    arch : string;
    order : Theory.Endianness.t option;
    bits : int option;
    variant : string option;
    compiler : string option;
  } [@@deriving compare, sexp, fields]

  type target =
    | Name of (Theory.Target.t [@sexp.opaque])
    | Spec of spec
  [@@deriving compare, sexp]

  type problem =
    | Wrong_endianness
    | Unrecognized_target

  type match_kind = Generic | Specific

  exception Wrong_arch_spec of problem

  let fail problem = raise (Wrong_arch_spec problem)

  let with_default field k =
    match field with
    | "default"|"*" -> None
    | other -> Some (k other)

  let of_parts arch order bits rest =
    let arch = String.lowercase arch in
    let bits = with_default bits Int.of_string in
    let order = with_default order @@ function
      | "LE" | "LEBE" -> Theory.Endianness.le
      | "BE" -> Theory.Endianness.eb
      | _ -> fail Wrong_endianness in
    let variant = match List.hd rest with
      | None -> None
      | Some var -> with_default var ident in
    let compiler = match rest with
      | [_;comp] -> with_default comp ident
      | _ -> None in
    Spec {arch; bits; order; variant; compiler}

  let parse str = match String.split str ~on:':' with
    | arch::order::bits::rest -> of_parts arch order bits rest
    | _ -> match Theory.Target.lookup ~package:"bap" str with
      | None -> fail Unrecognized_target
      | Some t -> Name t

  let field_matches equal value field = match field with
    | None -> true
    | Some field -> equal value field

  let matches target = function
    | Name t -> Theory.Target.belongs t target
    | Spec s ->
      let bits = Theory.Target.bits target in
      let order = Theory.Target.endianness target in
      field_matches equal bits s.bits &&
      field_matches Theory.Endianness.equal order s.order &&
      Theory.Target.matches target s.arch

  type t = target
  include Base.Comparable.Make(struct
      type t = target [@@deriving compare, sexp]
    end)
end

module Rule = struct

  type sizes = {
    total : int;
    post : int;
  }

  type t = {
    sizes : sizes option;
    prepatterns : Pattern.t list;
    postpatterns  : Pattern.t list;
    actions : Action.t list
  }

  let empty = {
    sizes = None;
    prepatterns = [];
    postpatterns = [];
    actions = []
  }

  let sizes total post = {total; post}

  let collapse matches = {
    sizes = List.find_map matches ~f:(fun {sizes} -> sizes);
    prepatterns = List.(matches >>= fun {prepatterns=xs} -> xs);
    postpatterns = List.(matches >>= fun {postpatterns=xs} -> xs);
    actions= List.(matches >>= fun {actions=xs} -> xs);
  }

  let pp_patterns = Format.pp_print_list Pattern.pp
  let pp_actions = Format.pp_print_list Action.pp

  let pp ppf {prepatterns; postpatterns; actions} =
    Format.fprintf ppf "@[<v>\
                        @[<v2>preps {@;%a@]@;}@;\
                        @[<v2>posts {@;%a@]@;}@;\
                        @[<v2>actions {@;%a@]@;}@;\
                        @]"
      pp_patterns prepatterns
      pp_patterns postpatterns
      pp_actions actions
end

module Grammar = struct
  open Parser

  let file = tree "patternfile" data >>| snd

  let file_with_compiler =
    attr "compiler" "id" >>= fun compiler ->
    file >>= fun file ->
    return (compiler,file)
    << close

  let with_compilers id =
    plus file_with_compiler >>| List.map ~f:(fun (comp,file) ->
        sprintf "%s:%s" id comp, file)

  let without_compilers id =
    file >>= fun file ->
    return [id,file]

  let language =
    attr "language" "id" >>= fun id ->
    (with_compilers id <|> without_compilers id) <<
    close

  let files =
    dtd >>
    tag "patternconstraints" >>
    plus language <<
    close

  let data = tree "data" data >>| snd

  let prepatterns =
    tree "prepatterns" (plus data) >>| snd >>| List.map ~f:Pattern.create


  let patternpairs_open_tag =
    tag "patternpairs" >>|
    Attributes.(const (fun x y -> Rule.sizes x y)
                $ int "totalbits" $ int "postbits")

  let action = start << close >>| fun a -> {
      Rule.empty with actions = [Action.of_tag a]
    }

  let data_elt =
    data >>| fun p -> {
      Rule.empty with postpatterns = [Pattern.create p]
    }

  let patterns_with_actions name =
    tag name >>
    !*(data_elt <|> action) >>= fun matches ->
    return (Rule.collapse matches) <<
    close

  let postpatterns =
    patterns_with_actions "postpatterns"

  let patternpair =
    required patternpairs_open_tag >>= fun sizes ->
    prepatterns >>= fun prepatterns ->
    postpatterns >>= fun matcher -> return {
      matcher with prepatterns;
                   sizes = Some sizes;
    } << close

  let singlepattern = patterns_with_actions "pattern"

  let entries =
    star (patternpair <|> singlepattern)

  let patterns =
    dtd >>
    tag "patternlist" >>
    entries <<
    close
end

module Rules = struct
  type case = {
    pattern : Pattern.t;        (* the pattern to match *)
    actions : Set.M(Action).t;  (* the actions to take *)
    shifted : int;              (* the size of the prepattern *)
  } [@@deriving equal]

  type t = {
    cases : case list;          (* all possible cases *)
    known : Set.M(Action).t;    (* all possible actions from all cases *)
  } [@@deriving equal]

  let empty = {
    cases = [];
    known = Set.empty (module Action);
  }

  let slot = KB.Class.property Theory.Unit.cls "pattern-rules"
      ~package:"bap" @@ KB.Domain.flat ~empty ~equal "rules"



  let pp_actions ppf actions =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      Action.pp ppf (Set.to_list actions)

  let pp_case ppf {pattern; actions} =
    Format.fprintf ppf "@[<hv2>%a ->@ %a@]"
      Pattern.pp pattern
      pp_actions actions

  let pp ppf {cases} =
    Format.fprintf ppf "@[<v>%a@]"
      (Format.pp_print_list pp_case) cases

  let is_large_enough sizes prep postp =
    match sizes with
    | None -> true
    | Some {Rule.total; post} ->
      Pattern.weight postp >= post &&
      Pattern.weight postp + Pattern.weight prep >= total

  let empty = {
    cases = [];
    known = Set.empty (module Action);
  }

  let concat x y = {
    cases = x.cases @ y.cases;
    known = Set.union x.known y.known;
  }

  let patterns_product pre post =
    let pre = match pre with
      | [] -> [Pattern.empty]
      | pre -> pre in
    List.cartesian_product pre post

  let create rules =
    List.fold rules ~init:empty ~f:(fun rules (r : Rule.t) ->
        patterns_product r.prepatterns r.postpatterns |>
        List.fold ~init:rules ~f:(fun rules (pre,post) ->
            if is_large_enough r.sizes pre post then
              let actions = Set.of_list (module Action) r.actions in
              let p = Pattern.concat pre post in {
                known = Set.union rules.known actions;
                cases = {
                  actions;
                  pattern=p;
                  shifted=Pattern.length pre;
                } :: rules.cases
              } else rules))

  let apply addr case f x =
    let addr = Addr.nsucc addr case.shifted in
    Set.fold case.actions ~init:x ~f:(fun x action ->
        f addr action x)

  let remove_actions work case = {
    work with known = Set.diff work.known case.actions
  }

  let search cases mem f x =
    let rec outer addr x =
      if Word.(addr < Memory.max_addr mem)
      then inner addr 0 cases x
      else x
    and inner addr pos work x =
      if Set.is_empty work.known || List.is_empty work.cases
      then outer (Addr.succ addr) x
      else match Memory.get ~index:pos ~addr mem with
        | Error _ -> x
        | Ok byte ->
          let byte = Word.to_int_exn byte in
          let x,work =
            List.fold work.cases ~init:(x,{work with cases=[]})
              ~f:(fun (x,work) case ->
                  let hits = Pattern.matches case.pattern ~pos byte in
                  if hits && Pattern.length case.pattern = pos + 1
                  then apply addr case f x, remove_actions work case
                  else x, if hits then {
                      work with cases = case :: work.cases
                    } else work) in
          inner addr (pos+1) work x in
    outer (Memory.min_addr mem) x


end

module Repository : sig
  val enable : string list -> unit
end = struct
  let patternconstraints =
    FileUtil.(And (Is_file, Basename_is "patternconstraints.xml"))

  let collect_patternconstraints root =
    FileUtil.find patternconstraints root (fun xs x -> x::xs) []

  let parse_file file grammar =
    In_channel.with_file file ~f:(fun ch ->
        Xmlm.make_input ~strip:true (`Channel ch) |>
        Parser.run grammar |> function
        | Ok result -> result
        | Error problem ->
          warning "failed to parse file %s: %a"
            file Parser.pp_error problem;
          [])

  let collect roots =
    List.concat_map roots ~f:collect_patternconstraints |>
    List.concat_map ~f:(fun toc ->
        let folder = FilePath.dirname toc in
        parse_file toc Grammar.files |>
        List.concat |> List.map ~f:(fun (target,file) ->
            Target.parse target,
            Filename.concat folder file))

  let dedup selected =
    let empty = Set.empty (module String) in
    List.fold selected ~init:(empty,empty) ~f:(fun (digests,files) file ->
        let digest = Caml.Digest.file file in
        if Set.mem digests digest ||
           Set.mem files file then digests,files
        else Set.add digests digest,
             Set.add files file) |>
    snd |> Set.to_list

  let select roots target =
    collect roots |>
    List.filter_map ~f:(fun (t,file) ->
        Option.some_if
          (Target.matches target t &&
           Sys.file_exists file &&
           not (Sys.is_directory file)) file) |>
    dedup

  let load roots target =
    select roots target |>
    List.concat_map ~f:(fun path ->
        parse_file path Grammar.patterns) |>
    Rules.create

  let enable roots =
    let open KB.Let in
    KB.promise Rules.slot @@ fun unit ->
    let+ target = KB.collect Theory.Unit.target unit in
    load roots target
end

module Analysis : sig
  val enable : unit -> unit
end = struct
  open KB.Syntax
  open KB.Let

  type KB.conflict += Expected of KB.Name.t
                   | Invalid_arity

  module Addr = struct
    include Bitvec_order
    include Bitvec_sexp
    include Bitvec_binprot
  end

  let empty_roots = Set.empty (module Addr)
  let empty_names = Bap_relation.empty Addr.compare String.compare

  type outcome = {
    roots : Set.M(Addr).t;
    names : string Map.M(Addr).t;
  } [@@deriving bin_io, compare, sexp, equal]

  let empty = {
    roots = Set.empty (module Addr);
    names = Map.empty (module Addr);
  }

  let slot = KB.Class.property Theory.Unit.cls "patterns-outcome"
      ~package:"bap"
      ~persistent:(KB.Persistent.of_binable (module struct
                     type t = outcome [@@deriving bin_io]
                   end)) @@
    KB.Domain.flat ~empty ~equal:equal_outcome "outcome"

  let roots = KB.Context.declare ~package:"bap" "patterns-roots"
      !!empty_roots

  let names = KB.Context.declare ~package:"bap" "patterns-names"
      !!empty_names

  let collect_actions rules mem actions =
    actions |>
    Rules.search rules mem @@ fun addr action actions ->
    let action = Set.singleton (module Action) action in
    Map.update actions (Word.to_bitvec addr) ~f:(function
        | None -> action
        | Some actions -> Set.union actions action)

  let (.$[]) v s = KB.Value.get s v
  let (.$[]<-) v s x = KB.Value.put s v x

  let (.?[]) v s = match KB.Value.get s v with
    | None -> KB.fail (Expected (KB.Slot.name s))
    | Some v -> !!v

  let nop = Theory.Effect.empty Theory.Effect.Sort.bot

  let sym str =
    let v = KB.Value.put Sigma.symbol Theory.Value.Top.empty (Some str) in
    KB.Value.put Theory.Semantics.value nop v

  let vec vec =
    KB.Value.put Sigma.static Theory.Value.Top.empty (Some vec)

  let accept c = c >>| fun _ -> sym "t"
  let reject c = c >>| fun _ -> sym "nil"

  let promise_root addr =
    let* addr = addr.?[Sigma.static] in
    accept @@
    KB.Context.update roots (fun roots -> Set.add roots addr)

  let promise_name addr name =
    let* addr = addr.?[Sigma.static] in
    let* name = name.?[Sigma.symbol] in
    let* rel = KB.Context.get names in
    match Bap_relation.(findl rel addr, findr rel name) with
    | [],[] ->
      accept @@
      KB.Context.set names (Bap_relation.add rel addr name)
    | _ ->
      reject @@
      KB.return ()

  let declare_promise_root () =
    let types = Primus.Lisp.Type.Spec.(tuple [int] @-> bool) in
    Sigma.declare ~types ~package:"bap" "promise-function-start"
      ~body:(fun _ -> !!(fun _ args -> match args with
          | [addr] -> promise_root addr
          | _ -> KB.fail Invalid_arity))

  let compute_outcome =
    let* roots = KB.Context.get roots in
    let+ names = KB.Context.get names in {
      roots;
      names = Bap_relation.fold names
          ~init:(Map.empty (module Addr))
          ~f:(fun addr name names ->
              Map.add_exn names addr name)
    }


  let reset_context = KB.sequence [
      KB.Context.set roots empty_roots;
      KB.Context.set names empty_names;
    ]

  let apply_actions unit actions =
    Map.to_sequence actions |>
    KB.Seq.iter ~f:(fun (addr,actions) ->
        Set.to_sequence actions |>
        KB.Seq.iter ~f:(fun action ->
            let* lbl = KB.Object.create Theory.Program.cls in
            KB.sequence [
              KB.provide Lambda.unit lbl (Some unit);
              KB.provide Lambda.addr lbl (Some addr);
              KB.provide Action.property lbl (Some action)
            ] >>= fun () ->
            KB.collect Theory.Semantics.slot lbl >>| ignore))

  let promise_outcome () =
    KB.promise slot @@ fun unit ->
    let* memory = KB.collect Project.memory_slot unit in
    let* rules = KB.collect Rules.slot unit in
    let actions = Memmap.to_sequence memory |>
                  Seq.fold ~f:(fun actions (mem,tag) ->
                      if Value.is Image.code_region tag
                      then collect_actions rules mem actions
                      else actions)
                    ~init:(Map.empty (module Addr)) in
    apply_actions unit actions >>= fun () ->
    compute_outcome >>= fun result ->
    reset_context >>| fun () ->
    result

  let promise_roots () =
    KB.promise Theory.Label.is_subroutine @@ fun lbl ->
    KB.collect Theory.Label.addr lbl >>=? fun addr ->
    KB.collect Theory.Label.unit lbl >>=? fun unit ->
    KB.collect slot unit >>| fun {roots} ->
    Option.some_if (Set.mem roots addr) true



  let enable () =
    promise_outcome ();
    promise_roots ();
    declare_promise_root ()

end

let parse_rule rule file =
  In_channel.with_file file ~f:(fun ch ->
      let src = Xmlm.make_input ~strip:true (`Channel ch) in
      Parser.(run (dtd >> rule)) src)

let parse_patterns = parse_rule Grammar.patterns

let dump_signals file =
  In_channel.with_file file ~f:(fun ch ->
      let src = Xmlm.make_input ~strip:true (`Channel ch) in
      while not (Xmlm.eoi src) do
        Format.printf "%a@\n%!" Xmlm.pp_signal (Xmlm.input src);
      done)

let pp_rules = Format.pp_print_list Rule.pp

let with_rules spec k =
  match parse_patterns spec with
  | Error err ->
    warning "Failed to parse the spec: %a@." Parser.pp_error err
  | Ok rules ->
    let rules = Rules.create rules in
    warning "Applying rules@\n%a@." Rules.pp rules;
    k rules

let print_matches rules mem =
  Rules.search rules mem (fun addr action () ->
      Format.printf "@[<v2>matching %a -> %a@]@\n"
        Addr.pp addr
        Action.pp action) ()

let test_on_file ~spec ~binary =
  with_rules spec @@ fun rules ->
  match Image.create ~backend:"llvm" binary with
  | Error err ->
    warning "Failed to open the binary: %a@." Error.pp err
  | Ok (img,_) ->
    Table.iteri (Image.segments img) ~f:(fun mem seg ->
        Format.printf "Analyzing segment %s:@.%a@."
          (Image.Segment.name seg)
          Memory.pp mem;
        print_matches rules mem)

let test_on_bytes ~spec bytes =
  with_rules spec @@ fun rules ->
  let data = Bigstring.of_string bytes in
  let addr = Word.of_int32 0l in
  match Memory.create LittleEndian addr data with
  | Error err ->
    Format.printf "Wrong memory: %a@." Error.pp err
  | Ok mem -> print_matches rules mem


let paths = Extension.Configuration.parameters
    Extension.Type.dir "path"
    ~doc:"Add the specified path to the list of patterns directories."

let path = Extension.Command.argument Extension.Type.file
let spec = Extension.Command.parameter Extension.Type.file
    "specification" ~aliases:["s"]


let provides = [
  "symbolizer";
  "rooter";
  "function-starts";
]

let () = Extension.Command.(begin
    declare "match-patterns"
      ~doc:"Run the specified patterns file on the input file and \
            prints the matches"
      (args $ spec $ path)
  end) @@
  fun spec binary _ctxt ->
  test_on_file ~spec ~binary;
  Ok ()


let () = Extension.declare ~provides ~doc @@ fun ctxt ->
  let (/) = Filename.concat in
  let paths =
    "/usr/share/ghidra" ::
    Extension.Configuration.datadir / "signatures" ::
    Extension.Configuration.sysdatadir / "signatures" ::
    Extension.Configuration.get ctxt paths in
  Repository.enable paths;
  Analysis.enable ();
  Action.Library.provide ();
  Ok ()
