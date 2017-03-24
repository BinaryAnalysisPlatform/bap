open Core_kernel.Std
open Bap.Std
open Primus.Std
open Format

module Words = struct
  type t = String.Set.t [@@deriving bin_io, compare, sexp]

  let max = 80

  let pp ppf set =
    let words = Set.to_list set |> String.concat ~sep:", " in
    let words = if String.length words < max then words
        else String.subo ~len:max words in
    fprintf ppf "%s" (String.escaped words)

  let to_string set = asprintf "%a" pp set
end

type words = Words.t


let chars = Value.Tag.register (module Words)
    ~uuid:"ff83ee29-1f58-4dc4-840c-4249de04a977"
    ~name:"beagle-chars"

let words = Value.Tag.register (module Words)
    ~uuid:"08e1ca88-eab9-4ac3-8fa8-3b08735a30e5"
    ~name:"beagle-words"


let strings = Value.Tag.register (module Words)
    ~uuid:"386efa37-85b0-4b48-b04d-8bafd5160670"
    ~name:"beagle-strings"



type t = {
  terms : tid seq;
  chars : string;
}


let create terms chars = {terms; chars}
let terms t = t.terms
let data t = t.chars

let inspect_prey {chars} = Sexp.Atom chars

let detected,finished =
  Observation.provide ~inspect:inspect_prey "beagle-prey"

let inspect (prey,words) =
  Sexp.List [
    inspect_prey prey;
    String.Set.sexp_of_t words;
  ]

let caught,catch = Observation.provide ~inspect "beagle"
