open Printf

module type Bap_main_interface = sig

  module Type : sig
    type 'a t
    val string : string t
    val int : int t
  end

  type ctxt
  type ('f,'r) t
  type 'a param

  val declare : string -> ('f,unit) t -> (ctxt -> 'f) -> unit

  val args : 'a param -> ('a -> 'b,'b) t
  val ($) : ('a, 'b -> 'c) t -> 'b param -> ('a,'c) t

  val rest : 'a param -> 'a list param

  val argument :
    ?docv:string ->
    ?doc:string -> 'a Type.t -> 'a param


  val param :
    ?docv:string ->
    ?doc:string ->
    ?short:char ->
    string ->
    'a Type.t ->
    'a param

  val flag :
    ?docv:string ->
    ?doc:string ->
    ?short:char ->
    string -> bool param

  val run : string array -> unit
end

module Main : Bap_main_interface = struct
  open Cmdliner
  module Type = struct
    type 'a t = 'a Arg.converter * 'a
    let string = Arg.string, ""
    let int = Arg.int,0
  end

  type ctxt = Whatever

  type len = Fin of int | Inf

  type 'a param =
    | Pos : 'a Type.t * Arg.info -> 'a param
    | All : 'a param -> 'a list param
    | Key : 'a Term.t -> 'a param


  let argument (type a) ?(docv="ARG") ?doc t : a param =
    Pos (t, Arg.info ~docv ?doc [])

  let param ?docv ?doc ?short:_ name (t,d) : 'a param =
    Key Arg.(value & opt t d & info ?docv ?doc [name])

  let flag ?docv ?doc ?short:_ name : bool param =
    Key Arg.(value & flag & info ?docv ?doc [name])

  let rest p = All p

  type ('a,'b) t = {
    run : 'a -> 'b Term.t;
    len : len;
  }

  let add_len (type a) len (b : a param) = match len,b with
    | Fin len, Pos _ -> Fin (len + 1)
    | Fin _, All _ -> Inf
    | Inf, All _ ->
      invalid_arg "can't use `rest' twice in the same list of arguments"
    | _ -> len

  let one (t,d) name p = Arg.(value & pos p t d name)
  let all (t,d) name p =
    if p = 0
    then Arg.(value & pos_all t [] name)
    else Arg.(value & pos_right (p-1) t [] name)

  let args : type a. a param -> (a -> 'b,'b) t = fun a -> {
      len = add_len (Fin 0) a;
      run = fun f -> Term.(pure f $ match a with
        | Pos (t,i) -> one t i 0
        | All (Pos (t,i)) -> all t i 0
        | Key t -> t
        | All _ -> assert false)}


  let ($) (type a) args (b : a param) = {
    len = add_len args.len b;
    run = fun f -> Term.(args.run f $ match b with
      | Key t -> t
      | _ -> match args.len with
        | Inf -> assert false
        | Fin n -> match b with
          | Pos (t,i) -> one t i n
          | All (Pos (t,i)) -> all t i n
          | _ -> assert false)
  }

  type command = unit Term.t * Term.info

  let default = Term.const (),
                Term.info "bap"

  let commands : command list ref = ref []


  let declare
    : string -> ('f,unit) t -> (ctxt -> 'f) -> unit =
    fun name {run} f ->
    commands := (run (f Whatever), Term.info name) :: !commands


  let run argv =
    match Term.eval_choice ~argv default !commands with
    | `Ok () ->
      printf "Ok!\n";
    | `Version|`Help ->
      printf "--version or --help were requested\n"
    | `Error _  ->
      printf "an error has happened!\n"
end

open Main

let file = argument ~docv:"INPUT" Type.string
let output = argument ~docv:"OUTPUT" Type.string

let off = param "offset" Type.int ~short:'o'
let depth = param "depth" Type.int ~short:'d'
let verbose = flag "verbose" ~short:'v'

let () = declare "parse" (args file $ off $ verbose) @@
  fun _ctxt file off verbose ->
  if verbose then printf "parsing %s from %d\n" file off;
  printf "Done\n"

let () = declare "read" (args file) @@ fun _ file ->
  printf "Reading %s\n" file


let () = declare "print" (args verbose $ off $ file $ output) @@
  fun _ verbose off input output ->
  if verbose
  then printf "copying from %S to %S from %d\n" input output off;
  printf "cat %S > %S\n" input output


let () = declare "copy" (args verbose $ off $ file $ rest output) @@
  fun _ verbose off input outputs ->
  if verbose
  then printf "copying from %S to %d outputs from %d\n"
      input (List.length outputs) off;
  printf "cp %S " input;
  List.iter (printf "%S ") outputs;
  printf "\n"

let () = declare "list" (args verbose $ depth $ rest file) @@
  fun _ verbose depth dirs ->
  if verbose
  then printf "Listing %d directories (max recursion = %d)\n"
      (List.length dirs) depth;
  printf "ls --depth=%d " depth;
  List.iter (printf "%S ") dirs;
  printf "\n"

let () = declare "list" (args verbose $ depth $ rest file) @@
  fun _ verbose depth dirs ->
  if verbose
  then printf "Listing %d directories (max recursion = %d)\n"
      (List.length dirs) depth;
  printf "ls --depth=%d " depth;
  List.iter (printf "%S ") dirs;
  printf "\n"
