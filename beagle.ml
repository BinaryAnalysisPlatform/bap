open Core_kernel.Std
open Bap.Std
open Microx.Std
open Regular.Std
open Format

include Self()

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


module Alphabet = Trapper.Ascii


module Strings = struct
  type state =
    | String of addr * int * char list
    | Data

  let to_ascii word = match Word.to_int word with
    | Ok n when n < 128 -> Char.of_int n
    | _ -> None

  let make_string len chars =
    let bytes = Bytes.create len in
    List.iteri chars ~f:(fun i c -> bytes.[len-i-1] <- c);
    Bytes.to_string bytes

  let scan mem =
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
    Seq.(ms >>| fst >>| scan |> reduce ~f:union) |> function
    | None -> Addr.Map.empty
    | Some m -> m

end

module Beagle = struct
  let len = 8

  let p0 = 0.0
  let p1 = 1. /. float Trapper.Ascii.Printable.length
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

  let phi c = match Char.of_int c with
    | Some ch when Char.is_print ch -> Some (ch,1. -. pdf ch)
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


module Escaped = struct
  type t = string [@@deriving bin_io, compare, sexp]
  let max = 60

  let ppcut ppf str =
    fprintf ppf "%S...%d+" (String.subo str ~len:max)
      (max - String.length str)

  let pp ppf str =
    if String.length str < 60
    then fprintf ppf "%S" str
    else ppcut ppf str
end


module Words = struct
  type t = String.Set.t [@@deriving bin_io, compare, sexp]

  let max = 16

  let escape x = asprintf "%a" Escaped.pp x

  let pp ppf set =
    let words = Set.to_sequence ~order:`Decreasing set in
    if Seq.length_is_bounded_by ~max words
    then Seq.pp Escaped.pp ppf words
    else
      let rest = sprintf "...,+%d more" (Seq.length words - max) in
      let words = Seq.(take words max >>| escape) in
      Seq.pp String.pp ppf Seq.(append words (singleton  rest))
end



let chars = Value.Tag.register (module Words)
    ~uuid:"ff83ee29-1f58-4dc4-840c-4249de04a977"
    ~name:"beagle-chars"

let words = Value.Tag.register (module Words)
    ~uuid:"08e1ca88-eab9-4ac3-8fa8-3b08735a30e5"
    ~name:"beagle-words"


let strings = Value.Tag.register (module Words)
    ~uuid:"386efa37-85b0-4b48-b04d-8bafd5160670"
    ~name:"beagle-strings"


module Trapper = Trapper.Make(Alphabet)

let marker dict {chars=cs; strings=ss} =
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
    if String.length data > 3 then
      printf "%a: %S\n" Addr.pp key data)

let add_strings trapper strings =
  Map.fold strings ~init:trapper ~f:(fun ~key ~data dict ->
    Trapper.add_word dict data)

let main dicts proj =
  let strings = Strings.extract (Project.memory proj) in
  let preys = run proj strings in
  let words = add_strings (Trapper.of_files dicts) strings in
  let marker = marker words preys in
  Project.with_program proj (marker#run (Project.program proj))


let () =
  let dicts =
    Config.(param (list file) "dictionary"
              ~doc:"Add dictionary file(s)") in
  Config.when_ready (fun {Config.get=(!)} ->
      Project.register_pass (main !dicts))
