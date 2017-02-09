open Core_kernel.Std
open Bap.Std
open Microx.Std
open Regular.Std
open Format

open Beagle_prey

module Trapper = Beagle_trapper

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

  let pchars =flag "print-chars" ~doc:"Print all observed letters."

  let pstrings = flag "print-strings" ~doc:"Print static strings"

  let no_words = flag "no-words"
      ~doc:"Don't try to build words from a dictionary"

  let alphabet :
    (module Trapper.Alphabet) param =
    param Trapper.(enum [
      "printable", (module Ascii.Printable : Trapper.Alphabet);
      "ascii", (module Ascii);
      "alpha", (module Ascii.Alpha);
      "alpha.caseless", (module Ascii.Alpha.Caseless);
      "alphanum", (module Ascii.Alphanum);
      "alphanum.caseless", (module Ascii.Alphanum.Caseless);
      "digits", (module Ascii.Digits);
    ]) "alphabet"
      ~doc:"Build words from the specified alphabet";

end


let memory_lookup proj addr =
  let memory = Project.memory proj in
  Memmap.lookup memory addr |> Seq.hd |> function
  | None -> None
  | Some (mem,_) -> match Memory.get ~addr mem with
    | Ok w -> Some w
    | _ -> None

let register_lookup proj =
  let arch = Project.arch proj in
  let width = Arch.addr_size arch |> Size.in_bits in
  let mem_start = Word.of_int64 ~width 0x40000000L in
  let module Target = (val target_of_arch arch) in
  fun var -> Option.some_if (Target.CPU.is_sp var) mem_start


module Strings = struct
  type state =
    | String of addr * int * char list
    | Data

  let to_ascii word = match Word.to_int word with
    | Error _ -> assert false
    | Ok n -> match Char.of_int_exn n with
      | '\x00' -> Some '\x00'
      | ch when Char.is_print ch || Char.is_whitespace ch -> Some ch
      | _ -> None

  let make_string len chars =
    let bytes = Bytes.create len in
    List.iteri chars ~f:(fun i c -> bytes.[len-i-1] <- c);
    Bytes.to_string bytes

  let scan (mem,sec) =
    let addr = Memory.min_addr mem in
    let rec next strings state disp =
      match Memory.get mem ~disp ~addr with
      | Error _ -> strings
      | Ok byte -> match to_ascii byte,state with
        | None,_ -> next strings Data (disp+1)
        | Some '\x00',Data -> next strings Data (disp+1)
        | Some '\x00',String (_,len,_) when len < 4 ->
          next strings Data (disp+1)
        | Some '\x00',String (base,len,chars) ->
          let data = make_string len chars in
          next (Map.add strings ~key:base ~data) Data (disp+1)
        | Some ch,Data ->
          let base = Addr.nsucc addr disp in
          next strings (String (base,1,[ch])) (disp+1)
        | Some ch,String (base,n,chars) ->
          next strings (String (base,n+1,ch::chars)) (disp+1) in
    next Addr.Map.empty Data 0

  let union = Map.merge ~f:(fun ~key -> function
      | `Both (s1,s2) ->
        Option.some @@
        if String.length s1 > String.length s2 then s1 else s2
      | `Left s | `Right s -> Some s)

  let extract memmap =
    let ms = Memmap.to_sequence memmap in
    Seq.(ms >>| scan |> reduce ~f:union) |> function
    | None -> Addr.Map.empty
    | Some m -> m

end

module Beagle = struct
  let len = 8

  let p0 = 0.0
  let p1 = 1. /. float Trapper.Ascii.Alphanum.length
  let threshold = p1
  let alpha = 0.1

  type target = {
    len : int;
    tids  : tid list;
    chars : char list;
  }

  type hypot = {
    target : target;
    accepted : bool;
  }

  type t = {
    temp : float;
    targets : target list;
    hypot : hypot;
  }


  let empty_target = {tids=[]; chars=[]; len=0}
  let h0 = {target = empty_target; accepted=false}

  let empty = {
    temp = p0;
    hypot = h0;
    targets = [];
  }

  let targets t = t.targets
  let uniform _ = p1
  let pdf c = uniform c

  let smells ch = Char.is_print ch

  let phi c = match Char.of_int c with
    | Some ch when smells ch -> Some (ch,1. -. pdf ch)
    | _ -> None

  let smooth t x = t *. (1. -. alpha) +. alpha *. x

  type action =
    | Done
    | Keep
    | Lock

  let classify temp hypot =
    if hypot.accepted then
      if temp < threshold then Done else Keep
    else
      if temp >= threshold then Lock else Keep


  let step_char t tid char =
    let p,hypot = match phi char with
      | None -> 0.0, t.hypot
      | Some (char,p) ->
        (if t.hypot.target.len = 3 * len then 0.0 else p), {
            t.hypot with target = {
            len = t.hypot.target.len + 1;
            tids = tid :: t.hypot.target.tids;
            chars = char :: t.hypot.target.chars;
          }} in
    let temp = max p0 (smooth t.temp p) in
    match classify temp hypot with
    | Done -> {temp; hypot=h0; targets=hypot.target::t.targets}
    | Keep -> {t with temp; hypot}
    | Lock -> {t with temp; hypot = {
        accepted=true;
        target = {
          len = 8;
          tids = List.take hypot.target.tids 8;
          chars = List.take hypot.target.chars 8;
        }}}

  let step t tid x =
    if Word.bitwidth x = 1 then t
    else Word.enum_bytes x LittleEndian |>
         Seq.map ~f:(fun w -> ok_exn (Word.to_int w)) |>
         Seq.fold ~init:t ~f:(fun t c -> step_char t tid c)

end

module Interpreter = struct
  open Monad.State.Monad_infix

  let max_steps = 100
  let max_loop = 10

  class context prog = object
    inherit Conqueror.context ~max_steps ~max_loop prog
    val beagle = Beagle.empty
    val strings : string list Tid.Map.t = Tid.Map.empty
    method with_beagle b = {< beagle = b >}
    method beagle = beagle
    method strings = strings
    method with_strings s = {< strings = s >}
  end

  let main proj strings =
    let prog = Project.program proj in
    let memory = memory_lookup proj in
    let lookup = register_lookup proj in
    let arch = Project.arch proj in
    let endian = Arch.endian arch in
    let size = (Arch.addr_size arch :> size) in
    let module Target = (val target_of_arch arch) in
    let mem = Bil.var Target.CPU.mem in
    object(self)
      constraint 'a = #context
      inherit ['a] Conqueror.main prog as  super
      inherit! ['a] Concretizer.main ~memory ~lookup ()

      method! eval_def def =
        super#eval_def def >>= fun () ->
        self#sniff_def (Def.lhs def)

      method! eval_arg arg =
        super#eval_arg arg >>= fun () ->
        self#sniff_def (Arg.lhs arg)


      method! eval_load ~mem ~addr e s =
        self#update_strings addr >>= fun () ->
        super#eval_load ~mem ~addr e s

      method private sniff_def var =
        super#lookup var >>| Bil.Result.value >>= function
        | Bil.Imm x ->
          self#update_context (fun ctxt tid ->
              ctxt#with_beagle (Beagle.step ctxt#beagle tid x)) >>= fun () ->
          self#update_strings (Bil.Int x)
        | _ -> Monad.State.return ()

      method private update_strings addr =
        super#eval_exp addr >>| Bil.Result.value >>= function
        | Bil.Mem _ | Bil.Bot -> Monad.State.return ()
        | Bil.Imm addr -> match Map.find strings addr with
          | Some data -> self#update_with_string data
          | None ->
            super#eval_load ~mem ~addr:(Bil.Int addr) endian size >>|
            Bil.Result.value >>= function
            | Bil.Mem _ | Bil.Bot -> Monad.State.return ()
            | Bil.Imm addr -> match Map.find strings addr with
              | Some data -> self#update_with_string data
              | None -> Monad.State.return ()

      method private update_with_string data =
            self#update_context (fun ctxt tid ->
                Map.add_multi ctxt#strings ~key:tid ~data |>
                ctxt#with_strings)

      method private update_context f =
        Monad.State.update (fun ctxt -> match List.hd ctxt#trace with
        | None -> assert false
        | Some tid -> f ctxt tid)

    end
end

type prey = {
  chars : string list Tid.Map.t;
  strings : string list Tid.Map.t;
}

let collect_chars targets ctxt =
  Beagle.targets ctxt#beagle |>
  List.fold ~init:targets ~f:(fun targets {Beagle.tids; chars} ->
      let data = String.of_char_list chars in
      List.fold ~init:targets tids ~f:(fun targets key ->
          Map.add_multi targets ~key:key ~data))

let collect_strings existing ctxt =
  Map.merge existing ctxt#strings ~f:(fun ~key -> function
      | `Both (s1,s2) -> Some (s1 @ s2)
      | `Left s | `Right s -> Some s)


let run proj strings =
  let prog = Project.program proj in
  let interp = Interpreter.main proj strings in
  let start = new Interpreter.context prog in
  let init = {chars = Tid.Map.empty; strings = Tid.Map.empty} in
  Term.enum sub_t prog |>
  Seq.fold ~init ~f:(fun prey sub ->
      let ctxt = Monad.State.exec (interp#eval_sub sub) start in
      let prey = {
        chars = collect_chars prey.chars ctxt;
        strings = collect_strings prey.strings ctxt
      } in
      Term.enum blk_t sub |> Seq.fold ~init:prey ~f:(fun prey blk ->
          let ctxt = Monad.State.exec (interp#eval_blk blk) start in
          {
            chars = collect_chars prey.chars ctxt;
            strings = collect_strings prey.strings ctxt
          }))

let create_marker {Config.get=(!!)} statics {chars=cs; strings=ss} =
  let module Alphabet = (val !!Param.alphabet) in
  let module Trapper = Trapper.Make(Alphabet) in
  let add_statics trapper =
    Map.fold statics ~init:trapper ~f:(fun ~key ~data dict ->
        Trapper.add_word dict data) in
  let dict = Trapper.of_files !!Param.dicts in
  let dict = if !!Param.no_strings then dict else add_statics dict in
  let dict = List.fold !!Param.words ~f:Trapper.add_word ~init:dict in
  let add_chars t =
    match Map.find cs (Term.tid t) with
    | None -> t
    | Some sets ->
      let sets = String.Set.of_list sets in
      let t = Term.set_attr t chars sets in
      let ws =
        Set.fold sets ~init:String.Set.empty ~f:(fun words cs ->
            Trapper.build dict cs |>
            Seq.filter ~f:(fun w -> String.length w > 3) |>
            Seq.fold ~init:words ~f:Set.add) in
      if Set.is_empty ws then t
      else Term.set_attr t words ws in
  let add_string t = match Map.find ss (Term.tid t) with
    | None -> t
    | Some ss ->
      String.Set.of_list ss |>
      Term.set_attr t strings in
  object(self)
    inherit Term.mapper as super
    method! map_term cls t =
      let t = super#map_term cls t in
      add_chars t |>
      add_string

  end

let print_strings = Map.iteri ~f:(fun ~key ~data ->
    if String.length data > 3 then printf "%s\n" (String.escaped data))


let create_printer {Config.get=(!!)} =
  let pchars = !!Param.pchars
  and pwords = !!Param.pwords
  and pstrings = !!Param.pstrings in

  let pr guard tag t =
    if guard then Option.iter (Term.get_attr t tag)
        ~f: (fun t -> printf "%s\n" @@ Words.to_string t) in
  object
    inherit [unit] Term.visitor as self
    method! enter_term cls t () =
      pr pchars chars t;
      pr pwords words t;
      pr pstrings strings t;
    method print_program prog =
      if pchars || pwords || pstrings then
        self#run prog ()

    method print_strings strings =
      if pstrings then print_strings strings
  end

let main conf proj =
  let strings = Strings.extract (Project.memory proj) in
  let printer = create_printer conf in
  printer#print_strings strings;
  let preys = run proj strings in
  let marker = create_marker conf strings preys in
  let prog = marker#run (Project.program proj) in
  printer#print_program prog;
  Project.with_program proj prog




let () = Config.when_ready (fun conf -> Project.register_pass (main conf))
