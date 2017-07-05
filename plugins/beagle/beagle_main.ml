open Core_kernel.Std
open Bap.Std
open Regular.Std
open Monads.Std
open Bap_primus.Std
open Bap_strings.Std
open Bap_future.Std
open Format

module type Alphabet = Strings.Unscrambler.Alphabet

include Self()

module Param = struct
  open Config;;

  manpage [
    `S "DESCRIPTION";

    `P "Beagle is an obfuscated string solver that uses CPU emulation
    for discovering and decoding strings. Beagle can also be used as
    an advanced data cross-referrence tool. Beagle combines
    Microexecution with digital signal processing techniques and
    efficient dictionary search. Data that pass through a CPU emulator
    is processed with a low-pass filter and string-like patterns are
    detected. At the final step, words are recovered from a detected
    shuffled sequence of characters. For this we employ a
    scrabble-like algorithm, that will detect all possible words, that
    can be built from a given sequence of characters. The algorithm is
    parameterized by a dictionary. It can be just an English
    dictionary, but it can be also a dictionary built from signatures
    obtained from a known compromised software. The dictionary search
    algorithm uses a special trie data structure with a search
    procedure that doesn't depend on the size of a dictionary or the
    size of a character sequence.  " ];;

  let no_strings = flag "ignore-strings"
      ~doc:"don't put static strings into the initial dictionary"

  let dicts = param (list file) "dictionary"
      ~doc:"Add dictionary file(s)."

  let words = param (list string) "words"
      ~doc:"Add specified words to the dictionary."

  let pwords = flag "print-words"
      ~doc:"Print all buildable words."

  let pchars = flag "print-chars" ~doc:"Print all observed letters."

  let pstrings = flag "print-strings" ~doc:"Print static strings"

  let no_words = flag "no-words"
      ~doc:"Don't try to build words from a dictionary"

  let pchar = param float "noise" ~default:0.05
      ~doc:"expected amount of noise characters"

  let alpha = param float "alpha" ~default:0.05
      ~doc:"desired probability of a false positive"

  let beta = param float "beta" ~default:1e-3
      ~doc:"desired probability of a false negative"

  let p1 = param float "text-probability" ~default:0.1
      ~doc:"a prior probability of the search data"

  let alphabet :
    (module Alphabet) param =
    param Strings.Unscrambler.(enum [
        "printable", (module Ascii.Printable : Alphabet);
        "alpha", (module Ascii.Alpha);
        "alpha.caseless", (module Ascii.Alpha.Caseless);
        "alphanum", (module Ascii.Alphanum);
        "alphanum.caseless", (module Ascii.Alphanum.Caseless);
        "digits", (module Ascii.Digits);
      ]) "alphabet"
      ~doc:"Build words from the specified alphabet"

  let get p = match Future.peek (determined p) with
    | Some x -> x
    | None -> failwith "this should never happen"

end

let make_alphabet (module A : Alphabet) =
  Seq.range 0 256 |> Seq.fold ~init:Char.Set.empty ~f:(fun alpha c ->
      let c = Char.of_int_exn c in
      let i = A.index c in
      if i < 0 || i >= A.length then alpha
      else Set.add alpha c)


type prey = {
  chars : string list Tid.Map.t;
  strings : string list Tid.Map.t;
}



(** global info for all hunters, don't hunt for addresses!  *)
type hunters_club = {
  addrs : Addr.Set.t;
  checked : Bil.Result.Id.Set.t;
  cache : String.Set.t String.Map.t;
}

type beagle = Beagle of tid Strings.Detector.t
type hunter = Hunter of (string -> String.Set.t)

let hunters_club =
  Primus.Machine.State.declare
    ~uuid:"1fa35cfe-f720-473c-a5ac-50fa8fa9f1fd"
    ~name:"beagle-hunters-club"
    (fun _ -> {
         addrs = Addr.Set.empty;
         checked = Bil.Result.Id.Set.empty;
         cache  = String.Map.empty;
       })


let beagle =
  Primus.Machine.State.declare
    ~uuid:"5d404352-722d-4ea1-bddb-aa4e4dc65c1d"
    ~name:"beagle"
    (fun _ ->
       let open Param in
       Beagle (Strings.Detector.create
                 ~alpha:(get alpha)
                 ~beta:(get beta)
                 ~p1:(get p1)
                 ~ps:(get pchar)
                 (make_alphabet (get alphabet))))

let hunter =
  Primus.Machine.State.declare
    ~uuid:"14d5eef5-4b04-418d-8a9f-3a20f52a5e10"
    ~name:"beagle-hunter"
    (fun _ ->
       let (!!) = Param.get in
       let module Alphabet = (val !!Param.alphabet) in
       let module Unscrambler = Strings.Unscrambler.Make(Alphabet) in
       let dict = Unscrambler.of_files !!Param.dicts in
       let dict = List.fold !!Param.words
           ~f:Unscrambler.add_word ~init:dict in
       let build input =
         Unscrambler.build dict input |>
         Seq.fold ~init:String.Set.empty ~f:Set.add in
       Hunter (build))


module Hunter(Machine : Primus.Machine.S) = struct
  open Machine.Syntax


  module Eval = Primus.Interpreter.Make(Machine)

  let save_address addr =
    Machine.Global.update hunters_club ~f:(fun t ->
        {t with addrs = Set.add t.addrs addr})

  let process_byte char =
    Eval.pos >>| Primus.Pos.tid >>= fun curr ->
    Machine.Local.get beagle >>= fun (Beagle d) ->
    let d = Strings.Detector.step d curr char in
    Strings.Detector.when_decided d (Machine.return ())
      ~f:(fun d ->
          let terms = Strings.Detector.data ~rev:false d in
          let chars = Strings.Detector.chars d in
          let prey = Beagle_prey.create (Seq.of_list terms) chars in
          Machine.Observation.make Beagle_prey.finished prey) >>=
    fun () ->
    Machine.Local.put beagle (Beagle d)

  let got_prey d =
    let terms = Strings.Detector.data ~rev:false d in
    let chars = Strings.Detector.chars d in
    let prey = Beagle_prey.create (Seq.of_list terms) chars in
    Machine.Observation.make Beagle_prey.finished prey

  let gohome () =
    Machine.Local.get beagle >>= fun (Beagle b) ->
    match Strings.Detector.abort b with
    | None -> Machine.return ()
    | Some d -> got_prey d

  let process_word w =
    Eval.pos >>| Primus.Pos.tid >>= fun curr ->
    Machine.Local.get beagle >>= fun (Beagle d) ->
    Word.enum_chars w LittleEndian |>
    Machine.Seq.fold ~init:d ~f:(fun d char ->
        match char with
        | '\255' | '\000'..'\010' -> Machine.return d
        | char ->
          let d = Strings.Detector.step d curr char in
          Strings.Detector.when_decided d (Machine.return ())
            ~f:got_prey >>= fun () ->
          Machine.return d) >>= fun d ->
    Machine.Local.put beagle (Beagle d)


  let process_memory (a,w) = process_word w

  let process_variable (_,r) = match Bil.Result.value r with
    | Bil.Mem _ | Bil.Bot -> Machine.return ()
    | Bil.Imm w ->
      let id = Bil.Result.id r in
      Machine.Global.get hunters_club >>= fun {checked} ->
      if Set.mem checked id
      then Machine.return ()
      else process_word w >>= fun () ->
        Machine.Global.update hunters_club ~f:(fun club ->
            {club with checked = Set.add club.checked id})


  let hunt prey =
    let key = Beagle_prey.data prey in
    Machine.Global.get hunters_club >>= fun {cache} ->
    match Map.find cache key with
    | None ->
      Machine.Local.get hunter >>= fun (Hunter hunt) ->
      let data = hunt key in
      Machine.Global.update hunters_club ~f:(fun club ->
          {club with cache = Map.add club.cache ~key ~data}) >>= fun () ->
      Machine.Observation.make Beagle_prey.catch (prey,data)
    | Some data ->
      Machine.Observation.make Beagle_prey.catch (prey,data)

  let print_prey prey =
    if Param.(get pchars)
    then printf "chars: %s\n%!" (Beagle_prey.data prey);
    Machine.return ()


  let print_result (prey,words) =
    if Param.(get pwords)
    then Set.iter words ~f:(printf "word: %s\n");
    Machine.return ()


  let init () =
    Machine.sequence Primus.Memory.[
        address_access   >>> save_address;
        (* variable_read    >>> process_variable; *)
        (* variable_written >>> process_variable; *)
        address_written >>> process_memory;
        Primus.Machine.finished >>> gohome;
        Beagle_prey.detected >>> print_prey;
        Beagle_prey.detected >>> hunt;
        Beagle_prey.caught >>> print_result;
      ]
end

let main proj =
  Primus.Machine.add_component (module Hunter)

let () = (Config.when_ready (fun _ ->
    Project.register_pass' ~deps:["strings-collect"] main))
