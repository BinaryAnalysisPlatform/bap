open Core_kernel
open Bap_main
open Bap.Std
open Bap_primus.Std

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

  let is_keyword = String.is_prefix ~prefix:":"

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

  let t =
    Extension.Type.define
      ~name:"GEN" ~parse ~print:to_string
      {predicate=Any; distribution=Static; parameters=[]}

  let list = Extension.Type.(list t)

  let create_uniform ?width ps =
    let min, max = match ps with
      | [] -> None, None
      | [max] -> None, Some (int_of_string max)
      | [min; max] ->
        Some (int_of_string min),
        Some (int_of_string max)
      | _ ->
        invalid_argf "Generator Uniform expects less \
                      than 3 arguments" () in
    Primus.Generator.Random.Seeded.lcg ()
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


  let of_spec ?width n ps = match n with
    | Random -> create_uniform ?width ps
    | Static -> create_const ?width ps

  let first_match ?width matches = List.find_map ~f:(fun s ->
      if matches s.predicate
      then Some (of_spec ?width s.distribution s.parameters)
      else None)

  let for_var ?width v = first_match ?width @@ function
    | Var [] -> true
    | Var names -> List.mem names ~equal:String.equal (Var.name v)
    | _ -> false
end

let generators = Extension.Configuration.parameter
    Generator.list "generators"
    ~doc:"Random number generator"

let init = Extension.Configuration.parameters
    Extension.Type.(list file) "init"
    ~doc:"A list of generator initialization scripts."

let main ctxt =
  let open Extension.Syntax in
  let generators =
    ctxt-->generators @
    List.concat_map (ctxt-->init) ~f:(fun files ->
        List.concat_map files ~f:Generator.from_file) in
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
          | Imm width -> match Generator.for_var ~width var generators with
            | None -> Machine.return ()
            | Some gen -> Env.add var gen)
    let init () = randomize_vars
  end in

  Primus.Components.register_generic "randomize-environment"
    (module RandomizeEnvironment) ~package:"bap"
    ~desc:"Randomizes registers.";
  Ok ()

let () = Extension.declare main
