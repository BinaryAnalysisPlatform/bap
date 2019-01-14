open Core_kernel
open Format

module Lisp = struct
  module Def = Bap_lisp__def
  module Program = Bap_lisp__program
end

module type Element = sig
  type t
  val pp : formatter -> t -> unit
end

module Category = String
module Name = String
module Descr = String

type index = (string * (string * string) list) list


let unquote s =
  if String.is_prefix s ~prefix:{|"|} &&
     String.is_suffix s ~suffix:{|"|}
  then String.sub s ~pos:1 ~len:(String.length s - 2)
  else s

let dedup_whitespace str =
  let buf = Buffer.create (String.length str) in
  let push = Buffer.add_char buf in
  String.fold str ~init:`white ~f:(fun state c ->
      let ws = Char.is_whitespace c in
      if not ws then push c;
      match state,ws with
      | `white,true  -> `white
      | `white,false -> `black
      | `black,true  -> push c; `white
      | `black,false -> `black) |> ignore;
  Buffer.contents buf

let normalize_descr s =
  dedup_whitespace (unquote (String.strip s))

let normalize xs =
  List.Assoc.map xs ~f:normalize_descr |>
  String.Map.of_alist_reduce ~f:(fun x y ->
      if x = "" then y else if y = "" then x
      else if x = y then x
      else sprintf "%s\nOR\n%s" x y) |>
  Map.to_alist


let describe prog item =
  Lisp.Program.get prog item |> List.map ~f:(fun x ->
      Lisp.Def.name x, Lisp.Def.docs x) |> normalize


let generate_index p (* signals *) = Lisp.Program.Items.[
    "Macros", describe p macro;
    "Substitutions", describe p subst;
    "Constants", describe p const;
    "Functions", describe p func;
    "Methods", describe p meth;
    "Parameters", describe p para;
    "Primitives", describe p primitive;
    (* "Signals", normalize signals; *)
  ]
