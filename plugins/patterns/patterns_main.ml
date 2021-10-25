open Core_kernel
open Bap_core_theory

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


module Parser : sig
  type 'a t
  type error
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >> ) : 'a t -> 'b t -> 'b t
  val ( >>$ ) : 'a t -> 'b -> 'b t
  val ( << ) : 'a t -> 'b t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val (!*) : 'a t -> 'a list t
  val (!+) : 'a t -> 'a list t

  val star : 'a t -> 'a list t
  val plus : 'a t -> 'a list t
  val ignore : 'a t -> unit t
  val required : 'a option t -> 'a t

  val dtd : unit t
  val close : unit t
  val tag : string -> Xmlm.attribute list t
  val matches_attr : string -> ('a * string) * 'b -> bool
  val attr : string -> string -> string t
  val data : string -> string t
  val error : string -> 'a t
  val run : 'a t -> Xmlm.input -> ('a,error) result
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
    | `Dtd _ -> consumed ()
    | _ -> return ()

  let close =
    peek >>= function
    | `El_end -> consumed ()
    | s -> reject s


  let tag name = peek >>= function
    | `El_start (tag,attrs) when Name.matches name tag ->
      consumed attrs
    | s -> reject s

  let matches_attr name ((_,id),_) = String.equal name id

  let attr tname aname =
    tag tname >>= fun attrs ->
    match List.find attrs ~f:(matches_attr aname) with
    | None -> fail (Missing_attribute {tag=tname; attr=aname})
    | Some (_,v) -> return v

  let data name =
    tag name >>= fun _ ->
    peek >>= function
    | `Data payload -> consumed payload
    | signal -> reject signal

  let (>>$) p x = p >> return x
  let (<<) p q = p >>= fun x -> q >> return x

  let (<|>) : 'a parser -> 'a parser -> 'a parser = fun p q s ->
    match p s with
    | _,Ok _ as ok -> ok
    | s,Error (Unexpected _|Is_required) -> q s
    | _,Error _ as err -> err

  let many ?(plus=false) : 'a parser -> 'a list parser = fun p s ->
    let finish xs s = {
      s with empty = List.is_empty xs;
    }, if List.is_empty xs && plus
      then Error Is_required
      else Ok (List.rev xs) in

    let rec loop xs s =
      match p s with
      | {empty=false} as s,Ok x -> loop (x::xs) s
      | {empty=true} as s,Ok x -> finish (x::xs) s
      | s,Error Unexpected _ -> finish xs s
      | _,Error _ as err -> err in
    loop [] s

  let star s = many ~plus:false s
  let plus s = many ~plus:true s

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


module Action = struct
  type t =
    | Align of {mark : int; bits : int}
    | Setcontext of {name : string; value : string}
    | Funcstart
    | Codeboundary
    | Possiblefuncstart

  let align mark bits = Align {mark; bits}
  let setcontext name value = Setcontext {name; value}
  let funcstart = Funcstart
  let codeboundary = Codeboundary
  let possiblefuncstart = Possiblefuncstart
end

module Pattern = struct
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
    bits : Z.t;
    mask : Z.t;
    pops : int;
    size : int;
  }

  type token = {
    pat : t;
    pos : int;
  }

  let create input =
    let {Parser.size; bits; mask} = Parser.run input in
    {bits; mask; size; pops = Z.popcount mask}

  let bits x = x.bits
  let mask x = x.mask
end

module Target = struct
  type spec = {
    arch : string;
    order : Theory.endianness;
    bits : int;
    rest : string option;
  }

  type problem =
    | Expects_4tuple
    | Wrong_endianness

  type match_kind = Generic | Specific

  exception Wrong_arch_spec of problem

  let fail problem = raise (Wrong_arch_spec problem)

  let create arch order bits rest =
    let arch = String.lowercase arch in
    let bits = Int.of_string bits in
    let order = match order with
      | "LE" -> Theory.Endianness.le
      | "BE" -> Theory.Endianness.eb
      | "LEBE" -> Theory.Endianness.bi
      | _ -> fail Wrong_endianness in
    let rest = match rest with
      | "default" | "*" -> None
      | other -> Some other in
    {arch; bits; order; rest}

  let parse str = match String.split str ~on:':' with
    | [arch; order; bits; rest] -> create arch order bits rest
    | _ -> fail Expects_4tuple

  let matches target specs =
    let bits = Theory.Target.bits target in
    let order = Theory.Target.endianness target in
    List.filter specs ~f:(fun s ->
        bits = s.bits &&
        Theory.Endianness.equal order s.order &&
        Theory.Target.matches target s.arch)

end

module Rule = struct
  type sizes = {
    total : int;
    post : int;
  }
  type t = {
    sizes : sizes option;
    prepatterns : string list;
    postpatterns  : string list;
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
end



module Grammar = struct
  open Parser

  let language =
    attr "language" "id" >>= fun id ->
    data "patternfile" >>= fun data ->
    return (id,data)
    << close
    << close

  let files =
    dtd >>
    tag "patternconstraints" >>
    plus language <<
    close

  let data = data "data" << close


  let prepatterns =
    tag "prepatterns" >>
    plus data <<
    close

  let align =
    tag "align" >>|
    Attributes.(const Action.align $ int "mark" $ int "bits")
    << close

  let setcontext =
    tag "setcontext" >>|
    Attributes.(const Action.setcontext $ str "name" $ str "value")
    << close

  let patternpairs_open_tag =
    tag "patternpairs" >>|
    Attributes.(const (fun x y -> Rule.sizes x y)
                $ int "totalbits" $ int "postbits")

  let simple_action name repr =
    tag name >>$ Some repr << close

  let funcstart =
    simple_action "funcstart" Action.funcstart
  let codeboundary =
    simple_action "codeboundary" Action.codeboundary
  let possiblefuncstart =
    simple_action "possiblefuncstart" Action.possiblefuncstart

  let action =
    required align <|>
    required setcontext <|>
    required funcstart <|>
    required codeboundary <|>
    required possiblefuncstart

  let action_elt =
    action >>| fun a -> {
      Rule.empty with actions = [a]
    }

  let data_elt =
    data >>| fun p -> {
      Rule.empty with postpatterns = [p]
    }

  let patterns_with_actions name =
    tag name >>
    !*(action_elt <|> data_elt) >>= fun matches ->
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

  let patternlist =
    dtd >>
    tag "patternlist" >>
    entries <<
    close

  let foo = tag "foo" >>$ `foo << close
  let bar = tag "bar" >>$ `bar << close

  let test =
    tag "test" >>
    plus foo <|> plus bar <<
    close

end

let test file rule =
  In_channel.with_file file ~f:(fun ch ->
      let src = Xmlm.make_input ~strip:true (`Channel ch) in
      Parser.(run (dtd >> rule)) src)

let dump_signals file =
  In_channel.with_file file ~f:(fun ch ->
      let src = Xmlm.make_input ~strip:true (`Channel ch) in
      while not (Xmlm.eoi src) do
        Format.printf "%a@\n%!" Xmlm.pp_signal (Xmlm.input src);
      done)
