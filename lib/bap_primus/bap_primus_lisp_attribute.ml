open Core_kernel
open Bap_core_theory
open Bap.Std

open Bap_primus_lisp_types

module Name = KB.Name

type cls
let cls : (cls,unit) KB.cls = KB.Class.declare  "attributes" ()
    ~package:"primus"
    ~public:true

type attrs = (cls,unit) KB.cls KB.Value.t
type set = attrs

type error = ..

exception Unknown_attr of string * tree
exception Failure of error * tree list


type 'a attr = {
  slot : (cls,'a) KB.slot;
  parse : tree list -> 'a;
}

type 'a t = 'a attr

type parser = Parser of (set -> tree list -> set)

let parsers : (Name.t, parser) Hashtbl.t =
  Hashtbl.create (module Name)

module Parse = struct
  type nonrec tree = tree
  type nonrec error = error = ..

  type error += Expect_atom | Expect_list



  let atom = function
    | {data=Atom s} -> Some s
    | _ -> None
  let list = function
    | {data=List ts} -> Some ts
    | _ -> None

  let tree ~atom ~list = function
    | {data=Atom s} -> atom s
    | {data=List ts} -> list ts

  let fail err ts = raise (Failure (err,ts))
end

type Parse.error += Conflict of KB.conflict

let make_parser {parse; slot} attrs tree =
  let merge = KB.Domain.join @@ KB.Slot.domain slot in
  match merge (KB.Value.get slot attrs) (parse tree) with
  | Ok value -> KB.Value.put slot attrs value
  | Error err -> Parse.fail (Conflict err) tree


let declare ?desc ?package ~domain ~parse name =
  let slot = KB.Class.property cls name domain
      ?package ?desc ~public:true in
  let attr = {slot; parse} in
  let parser = Parser (make_parser attr) in
  let name = KB.Slot.name slot in
  Hashtbl.add_exn parsers ~key:name ~data:parser;
  attr

let expected_parsers () =
  Hashtbl.keys parsers |> List.map ~f:Name.show |>
  String.concat ~sep:" | "

let parse s attrs name values = match Hashtbl.find parsers name with
  | None -> raise (Unknown_attr (Name.show name,s))
  | Some (Parser run) -> run attrs values

let parse attrs = function
  | {data=List ({data=Atom name} as s :: values)} ->
    let name = Name.read ~package:"core" name in
    parse s attrs name values
  | {data=Atom name} as s ->
    let name = Name.read ~package:"core" name in
    parse s attrs name []
  | _ -> attrs


module Set = struct
  let get {slot} = KB.Value.get slot
  module Self = (val KB.Value.derive cls)

  let slot = KB.Class.property Theory.Program.cls "primus-attrs" Self.domain
      ~public:true
      ~package:"primus"
      ~persistent:(KB.Persistent.of_binable (module Self))

  include Self
end
