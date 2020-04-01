open Core_kernel
open Bap_main
open Bap.Std
open Bap_primus.Std

module Generator = struct
  type predicate =
    | Any
    | Var of string
    | Mem of Bitvec_sexp.t list
  [@@deriving sexp]

  type name =
    | Const
    | Uniform
  [@@deriving sexp]

  type generator = predicate * name * string list [@@deriving sexp]

  let from_file filename =
    Sexp.load_sexps_conv_exn filename generator_of_sexp

  let t =
    Extension.Type.define
      ~name:"GEN"
      ~parse:(fun str ->
          generator_of_sexp @@ Sexp.of_string str)
      ~print:(fun gen ->
          Sexp.to_string_hum (sexp_of_generator gen))
      (Any,Const,[])

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
    | Uniform -> create_uniform ?width ps
    | Const -> create_const ?width ps

  let first_match ?width matches = List.find_map ~f:(fun (p,g,ps) ->
      if matches p then Some (of_spec ?width g ps)
      else None)

  let for_var ?width v = first_match ?width @@ function
    | Var n -> (String.equal (Var.name v) n)
    | _ -> false

  let for_mem ?width p = first_match ?width @@ function
    | Mem [p'] -> Bitvec.equal p p'
    | Mem [lower; upper] ->
      Bitvec.(p >= lower) && Bitvec.(p <= upper)
    | _ -> invalid_argf
             "The MEM predicate expects one or two parameters" ()
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
