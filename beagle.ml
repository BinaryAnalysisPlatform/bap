open Core_kernel.Std
open Bap.Std
open Microx.Std
open Monads.Std

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


module Beagle = struct
  (* xxx: the following values implicitly hardcode 4 byted words *)

  let p = 1e-4
  let threshold = 0.5
  let alpha = 1. /. 8.
  let phi = 1. -. p

  type t = {
    temp : float;
    targets : tid list;
  }

  let empty = {
    temp = p;
    targets = [];
  }

  let targets t = t.targets

  let byte = Word.of_int ~width:8
  let is_ascii = Word.between ~low:(byte 32) ~high:(byte 64)
  let is_null = Word.is_zero

  let is_target x = is_ascii x || is_null x

  let found tid t =
    let temp = t.temp *. (1. -. alpha) +. alpha *. phi in
    if temp > threshold then {temp; targets = tid :: t.targets}
    else {t with temp}

  let loose tid t =
    let temp = t.temp *. (1. -. alpha) in
    {t with temp = max temp p}

  let smell t tid x =
    if is_null x then t
    else Word.enum_bytes x LittleEndian |>
         Seq.for_all ~f:is_target |> function
         | true  -> found tid t
         | false -> loose tid t
end

module Interpreter = struct
  open Monad.State.Syntax

  let max_steps = 100
  let max_loop = 10

  class context prog = object
    inherit Conqueror.context ~max_steps ~max_loop prog
    val beagle = Beagle.empty
    method with_beagle b = {< beagle = b >}
    method beagle = beagle
  end

  class ['a] main proj =
    let prog = Project.program proj in
    let memory = memory_lookup proj in
    let lookup = register_lookup proj in
    object(self)
      constraint 'a = #context
      inherit ['a] Conqueror.main prog
      inherit! ['a] Concretizer.main ~memory ~lookup () as super

      method! eval_int x =
        Monad.State.update (fun ctxt -> match List.hd ctxt#trace with
        | None -> ctxt
        | Some tid ->
          ctxt#with_beagle (Beagle.smell ctxt#beagle tid x)) >>= fun () ->
        super#eval_int x
    end

end

let run proj =
  let prog = Project.program proj in
  let interp = new Interpreter.main proj in
  let ctxt = new Interpreter.context prog in
  Term.enum sub_t prog |>
  Seq.fold ~init:Tid.Set.empty ~f:(fun targets sub ->
      let ctxt = Monad.State.exec (interp#eval_sub sub) ctxt in
      Set.union targets @@
      Tid.Set.of_list (Beagle.targets ctxt#beagle))

let prey = Value.Tag.register (module Unit)
    ~uuid:"c5f284a2-09e2-417a-ad2f-241efd5bd149"
    ~name:"beagle-prey"

let marker preys = object
  inherit Term.mapper as super
  method! map_term cls t =
    let t = super#map_term cls t in
    if Set.mem preys (Term.tid t)
    then Term.set_attr t prey ()
    else t
end

let main proj =
  let preys = run proj in
  let marker = marker preys in
  Project.with_program proj (marker#run (Project.program proj))


let () =
  Config.when_ready (fun {Config.get=(!)} ->
      Project.register_pass main)
