let doc = {|
# DESCRIPTION




|}

open Core_kernel
open Bap_main
open Bap.Std
open Bap_primus.Std


let is_keyword = String.is_prefix ~prefix:":"
let is_numeral s =
  String.length s > 0 && Char.is_digit s.[0]

let parse_exp ~width s =
  if is_numeral s then
    let m = Bitvec.modulus width in
    let x = Bitvec.(bigint (Z.of_string s) mod m) in
    Bil.int (Word.create x width)
  else Bil.var (Var.create s (Type.Imm width))

module Generator = struct

  type region =
    | Default
    | Address of string
    | Section of string
    | Interval of string * string

  type predicate =
    | Any
    | Var of string list
    | Mem of region

  type name =
    | Static
    | Random
  [@@deriving sexp]

  type generator = {
    predicate : predicate;
    distribution : name;
    parameters : string list;
  }

  let atom = function
    | Sexp.Atom x -> x
    | xs ->
      invalid_argf "Expected a flat list of atoms, got %s"
        (Sexp.to_string_hum xs) ()

  let atoms = List.map ~f:atom

  let predicate_of_sexp : Sexp.t -> predicate = function
    | Atom "_" -> Any
    | List [Atom "var"; Atom "_"] -> Var []
    | List (Atom "var" :: vars) -> Var (atoms vars)
    | List [Atom "mem"; Atom "_"] -> Mem Default
    | List [Atom "mem"; Atom loc] ->
      Mem (if is_keyword loc then Section loc else Address loc)
    | List [Atom "mem"; Atom lower; Atom upper] ->
      Mem (Interval (lower,upper))
    | other -> invalid_argf "Expected a generator specification got %s"
                 (Sexp.to_string_hum other) ()

  let of_sexp : Sexp.t -> generator = function
    | Atom ("static"|"random") as dis -> {
        predicate = Any;
        distribution = name_of_sexp dis;
        parameters=[]
      }
    | List ((Atom ("static"|"random") as dis) :: ps) -> {
        predicate = Any;
        distribution = name_of_sexp dis;
        parameters = atoms ps
      }
    | List (pred :: dis :: pars) -> {
        predicate = predicate_of_sexp pred;
        distribution = name_of_sexp dis;
        parameters = atoms pars;
      }
    | other ->
      invalid_argf "expected layout specification, got %s"
        (Sexp.to_string_hum other) ()

  let pp_name ppf = function
    | Static -> Format.fprintf ppf "Static"
    | Random -> Format.fprintf ppf "Random"

  let pp_strings = Format.pp_print_list Format.pp_print_string
      ~pp_sep:Format.pp_print_space

  let pp_predicate ppf = function
    | Any -> Format.fprintf ppf "_"
    | Var [] -> Format.fprintf ppf "(var _)"
    | Var vars -> Format.fprintf ppf "(var %a)" pp_strings vars
    | Mem Default -> Format.fprintf ppf "(mem _)"
    | Mem (Address str | Section str) ->
      Format.fprintf ppf "(mem %s)" str
    | Mem (Interval (lower,upper)) ->
      Format.fprintf ppf "(mem %s %s)" lower upper

  let pp ppf {predicate=p; distribution=d; parameters=ps} =
    Format.fprintf ppf "(%a %a %a)"
      pp_predicate p
      pp_name d
      pp_strings ps

  let to_string = Format.asprintf "%a" pp

  let from_file filename =
    Sexp.load_sexps_conv_exn filename of_sexp

  let parse str =
    Sexp.of_string str |> of_sexp

  let default_generator = {
    predicate=Any;
    distribution=Random;
    parameters=[]
  }

  let t =
    Extension.Type.define
      ~name:"GEN" ~parse ~print:to_string default_generator


  let list = Extension.Type.(list t =? [default_generator])

  let create_uniform ?(seed=0) ?width ps =
    let min, max = match ps with
      | [] -> None, None
      | [max] -> None, Some (int_of_string max)
      | [min; max] ->
        Some (int_of_string min),
        Some (int_of_string max)
      | _ ->
        invalid_argf "Generator Uniform expects less \
                      than 3 arguments" () in
    Primus.Generator.Random.lcg seed
      ?width ?min ?max

  let create_const ?width = function
    | [] -> Primus.Generator.static ?width 0
    | [x] -> Primus.Generator.static ?width (int_of_string x)
    | xs ->
      let xs = Array.of_list_map xs ~f:Bitvec.of_string in
      let min = Array.min_elt xs ~compare:Bitvec.compare in
      let max = Array.max_elt xs ~compare:Bitvec.compare in
      Primus.Generator.of_iterator (module struct
        type t = int
        type dom = Bitvec.t
        let min = Option.value_exn min
        let max = Option.value_exn max
        let next i = i
        let value i = xs.(i mod Array.length xs)
      end) 0 ~to_bitvec:ident ?width


  let create ?seed ?width n ps = match n with
    | Random -> create_uniform ?seed ?width ps
    | Static -> create_const ?width ps

  let first_match ?seed ?width matches = List.find_map ~f:(fun s ->
      if matches s.predicate
      then Some (create ?seed ?width s.distribution s.parameters)
      else None)

  let for_var ?seed ?width v = first_match ?seed ?width @@ function
    | Var [] | Any -> true
    | Var names -> List.mem names ~equal:String.equal (Var.name v)
    | _ -> false
end

let generators = Extension.Configuration.parameter
    Generator.list "generators"
    ~doc:"Random number generator"

let init = Extension.Configuration.parameters
    Extension.Type.(list file) "init"
    ~doc:"A list of generator initialization scripts."

let seed = Extension.Configuration.parameter
    Extension.Type.int "seed"
    ~doc:"The seed that will be used to initialize all generators"

type arg_generators = {
  args : Primus.Generator.t Var.Map.t
}


let debug_msg,post_msg = Primus.Observation.provide "random-debug"
    ~inspect:sexp_of_string

module Debug(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  let msg fmt = Format.kasprintf (fun msg ->
      let msg = Format.asprintf "%s" msg in
      Machine.Observation.make post_msg msg) fmt
end

let main ctxt =
  let open Extension.Syntax in
  let generators =
    ctxt-->generators @
    List.concat_map (ctxt-->init) ~f:(fun files ->
        List.concat_map files ~f:Generator.from_file) in
  let seed = ctxt-->seed in
  let args = Primus.Machine.State.declare
      ~uuid:"2d9a70fb-8433-4a53-a750-3151f9366cb6"
      ~name:"generators-for-arguments" @@ fun proj ->
    let prog = Project.program proj in
    Term.enum sub_t prog |>
    Seq.fold ~init:{args=Var.Map.empty} ~f:(fun init sub ->
        Term.enum arg_t sub |>
        Seq.fold ~init ~f:(fun {args} arg ->
            if Arg.intent arg = Some In then {args}
            else match Var.typ (Arg.lhs arg) with
              | Unk | Mem _ -> {args}
              | Imm width ->
                let var = Arg.lhs arg in
                match Generator.for_var ~seed ~width var generators with
                | None -> {args}
                | Some gen -> {args = Map.set args var gen})) in
  let module RandomizeEnvironment(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Env = Primus.Env.Make(Machine)

    let vars_collector = object
      inherit [Set.M(Var).t] Term.visitor
      method! enter_var v vs = Set.add vs v
    end

    let randomize_vars =
      Machine.get () >>= fun proj ->
      vars_collector#run (Project.program proj) Var.Set.empty |>
      Set.to_sequence |>
      Machine.Seq.iter ~f:(fun var -> match Var.typ var with
          | Mem _ | Unk -> Machine.return ()
          | Imm width ->
            match Generator.for_var ~seed ~width var generators with
            | None -> Machine.return ()
            | Some gen -> Env.add var gen)
    let init () = randomize_vars
  end in

  let module TrapPageFault(Machine : Primus.Machine.S) = struct
    module Code = Primus.Linker.Make(Machine)
    let exec =
      Code.unlink (`symbol Primus.Interpreter.pagefault_handler)
  end in


  let module RandomizeMemory(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Eval = Primus.Interpreter.Make(Machine)
    module Memory = Primus.Memory.Make(Machine)
    module Linker = Primus.Linker.Make(Machine)

    let allocate ?upper ~width lower generator =
      let eval s =
        Eval.exp (parse_exp ~width s) >>| Primus.Value.to_word in
      eval lower >>= fun lower ->
      match upper with
      | None -> Memory.allocate ~generator lower 1
      | Some upper ->
        eval upper >>= fun upper ->
        let diff = Word.(upper - lower) in
        match Word.to_int diff with
        | Ok diff -> Memory.allocate ~generator lower (diff+1)
        | Error _ ->
          invalid_argf "The specified interval (%s) is too large and \
                        is currently not supported by Primus Random"
            (Word.to_string diff) ()


    (* in memory the last added layer has precedence over the
       previously added, so we start with the last specified region
       and then add more specific on top of it.
    *)
    let initialize_regions =
      Machine.arch >>= fun arch ->
      let width = Arch.addr_size arch |> Size.in_bits in
      List.rev generators |>
      Machine.List.iter ~f:(function
          | {Generator.predicate=Mem Default|Any} -> Machine.return ()
          | {predicate = Mem region; distribution; parameters} ->
            let generator =
              Generator.create ~seed ~width distribution parameters in
            let lower,upper = match region with
              | Address lower -> lower,None
              | Section name ->
                sprintf "bap%s-lower" name,
                Some (sprintf "bap%s-upper" name)
              | Interval (lower,upper) -> lower, Some upper
              | Default -> assert false in
            allocate ~width lower ?upper generator
          | _ -> Machine.return ())

    let default_page_size = 4096

    let map_page ?generator already_mapped addr =
      let rec map len =
        let last = Addr.nsucc addr (len - 1) in
        already_mapped last >>= function
        | true -> map (len / 2)
        | false ->
          Memory.allocate ?generator addr len in
      map default_page_size

    let trap () =
      Linker.link ~name:Primus.Interpreter.pagefault_handler
        (module TrapPageFault)

    let pagefault ?generator x =
      Memory.is_mapped x >>= function
      | false -> map_page ?generator Memory.is_mapped x >>= trap
      | true ->
        Memory.is_writable x >>= function
        | false -> map_page ?generator Memory.is_writable x >>= trap
        | true -> Machine.return ()

    let init () =
      Machine.arch >>= fun arch ->
      let width = Arch.addr_size arch |> Size.in_bits in
      let generator = Generator.first_match ~seed ~width (function
          | Mem Default | Any -> true
          | _ -> false) generators in
      Machine.sequence [
        Primus.Interpreter.pagefault >>> pagefault ?generator;
        initialize_regions;
      ]
  end in

  let module PertubeOutputs(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Eval = Primus.Interpreter.Make(Machine)
    module Val = Primus.Value.Make(Machine)
    module Env = Primus.Env.Make(Machine)
    module Mem = Primus.Memory.Make(Machine)
    module Gen = Primus.Generator.Make(Machine)
    module Debug = Debug(Machine)

    let pertube_var gen v = match Var.typ v with
      | Mem _ | Unk -> Machine.return ()
      | Imm m ->
        let n = Primus.Generator.width gen in
        Gen.word gen n >>|
        Word.extract_exn ~hi:(m-1) >>=
        Val.of_word >>= Eval.set v

    let is_entry sub blk = match Term.first blk_t sub with
      | Some entry -> Term.same blk entry
      | None -> false

    let pertube gen : exp -> unit Machine.t = function
      | Var v -> pertube_var gen v
      | Load (_,ptr,endian,size) ->
        Eval.exp ptr >>= fun ptr ->
        Gen.word gen (Size.in_bits size) >>=
        Val.of_word >>= fun value ->
        Eval.store ptr value endian size
      | exp ->
        Exp.free_vars exp |>
        Set.to_sequence |>
        Machine.Seq.iter ~f:(pertube_var gen)

    let exec =
      Debug.msg "the unresolved handler is called" >>= fun () ->
      Eval.pos >>= function
      | Primus.Pos.Jmp {up={me=blk; up={me=sub}}}
        when is_entry sub blk ->
        Machine.Local.get args >>= fun {args} ->
        Term.enum arg_t sub |>
        Machine.Seq.iter ~f:(fun arg ->
            match Map.find args (Arg.lhs arg) with
            | Some gen ->
              Debug.msg "pertubing argument %a" Arg.pp arg >>= fun () ->
              pertube gen (Arg.rhs arg)
            | None ->
              Debug.msg
                "skipping %a as there is no matching generator"
                Arg.pp arg
              >>= fun () ->
              Machine.return ())
      | p ->
        Debug.msg "called in a wrong position: %s"
          (Primus.Pos.to_string  p)

  end in

  let module HandleUnresolvedCalls(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Linker = Primus.Linker.Make(Machine)

    let name = Primus.Linker.unresolved_handler

    let init () =
      Linker.link ~name (module PertubeOutputs)
  end in

  Primus.Components.register_generic "var-randomizer"
    (module RandomizeEnvironment) ~package:"bap"
    ~desc:"Randomizes registers.";

  Primus.Components.register_generic "mem-randomizer"
    (module RandomizeMemory) ~package:"bap"
    ~desc:"Randomizes process memory.";

  Primus.Components.register_generic "arg-randomizer"
    (module HandleUnresolvedCalls) ~package:"bap"
    ~desc:"Randomizes output arguments of unbound procedures, as well
      as prevents failures when such procedures are called by trapping
      the unresolved handler.";

  Ok ()

let () = Extension.declare main
