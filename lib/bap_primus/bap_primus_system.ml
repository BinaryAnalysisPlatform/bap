open Bap.Std
open Bap_primus_types
open Bap_knowledge
open Core_kernel

module Name = Knowledge.Name
module Info = Bap_primus_info
module Observation = Bap_primus_observation
module Machine = Bap_primus_machine


let package = "bap"

let fini,finish =
  Observation.provide ~inspect:sexp_of_unit "fini"
    ~desc:"Occurs when machine finishes."


let init,inited =
  Observation.provide ~inspect:sexp_of_unit "init"
    ~desc:"Occurs when all components of the machine are initialized."

let start = Bap_primus_machine.start
let stop = Bap_primus_machine.stop

type component_specification = Name.t
type system_specification = Name.t


type t = {
  name : Name.t;
  desc : string;
  depends_on : system_specification list;
  components : component_specification list;
}

type system = t

module Repository = struct
  let self = Hashtbl.create (module Name)

  exception Not_found

  let add sys =
    if Hashtbl.mem self sys.name
    then invalid_argf "System named %s is already present in the repository"
        (Name.show sys.name) ();
    Hashtbl.add_exn self sys.name sys

  let require name =
    match Hashtbl.find self name with
    | Some sys -> sys
    | None -> invalid_argf "Unknown system %s" (Name.show name) ()

  let get ?package name =
    require @@ Name.create ?package name

  let find = Hashtbl.find self

  let update ?package name ~f =
    let name = Name.create ?package name in
    Hashtbl.set self name (f (require name))

  let list () =
    Hashtbl.data self |> List.map ~f:(fun {desc; name}  ->
        Info.create ~desc name)
end

let pp_names =
  Format.pp_print_list ~pp_sep:Format.pp_print_space Name.pp

let pp ppf {name; components; depends_on; desc} =
  Format.fprintf ppf
    "@[<v2>(defsystem %a@\n\
     :description \"@[<hov>%a@]\"@\n\
     :depends-on (@[<v>%a@])@\n\
     :components (@[<v>%a@]))@]"
    Name.pp name
    Format.pp_print_text desc
    pp_names depends_on
    pp_names components

let component = Name.create
let depends_on = Name.create

let define
    ?(desc="")
    ?(depends_on=[])
    ?(components=[])
    ?package name = {
  name = Name.create ?package name; desc;
  components; depends_on
}

let name t = t.name

let add_component ?package s c = {
  s with components = Name.create ?package c :: s.components
}

let add_dependency ?package s c = {
  s with depends_on = Name.create ?package c :: s.depends_on
}

let rec components sys =
  let init = Set.of_list (module Name) sys.components in
  List.fold sys.depends_on ~init ~f:(fun comps sys ->
      Set.union comps @@ components (Repository.require sys))

module Components = struct
  open Bap_primus_types

  type 'a item = {init : 'a; desc : string; hide : bool}

  let generics = Hashtbl.create (module Name)
  let analyses = Hashtbl.create (module Name)

  let add_component ns table
      ?(internal=false) ?(desc="") ?(package="user") name init =
    let name = Name.create ~package name in
    if Hashtbl.mem table name
    then invalid_argf
        "A %s component named %s is already registered, \
         please choose a unique name" ns (Name.show name) ();
    Hashtbl.add_exn table name {init; desc; hide=internal}


  let register_generic = add_component "generic" generics
  let register = add_component "specialized" analyses

  let long name =
    let uses =
      Hashtbl.data Repository.self |>
      List.filter_map ~f:(fun sys ->
          if Set.mem (components sys) name
          then Some (Format.asprintf "- %a" Name.pp sys.name)
          else None) in
    let pp_uses = Format.pp_print_list Format.pp_print_string in
    Format.asprintf "@[<v2>Used in the following systems:@\n%a@]"
      pp_uses uses


  let info ~spec repo =
    Hashtbl.to_alist repo |> List.filter_map ~f:(fun (name,{desc;hide}) ->
        if hide then None
        else
          let desc = sprintf "[%s]\n%s"
              (if spec then "analysis" else "generic") desc in
          Some (Info.create ~desc name ~long:(fun () -> long name)))

  let list () = info ~spec:true analyses @
                info ~spec:false generics

  module Generic(Machine : Machine) = struct
    open Machine.Syntax
    module Lisp = Bap_primus_lisp.Make(Machine)
    module Context = Bap_primus_lisp_context

    let inited () =
      Machine.Observation.make inited ()

    let finish () =
      Machine.Observation.make finish ()

    let do_init s loaded =
      let comps = Set.diff (components s) loaded in
      let context = Context.create [
          "system", [Knowledge.Name.show s.name];
          "component", Set.to_list (components s) |>
                       List.map ~f:Knowledge.Name.show
        ] in
      Lisp.refine context >>= fun () ->
      Set.to_list comps |>
      Machine.List.iter ~f:(fun name ->
          match Hashtbl.find generics name with
          | Some {init=(module Gen : Component)} ->
            let module Comp = Gen(Machine) in
            Comp.init ()
          | None ->
            failwithf "failed to find a component %s \
                       required for system %s"
              (Name.show name)
              (Name.show s.name)
              ())

    let init_system s =
      do_init s (Set.empty (module Name))

    let run_internal ~boot ~init ~fini ~start =
      Machine.run ~boot
        ~init:(Lisp.typecheck >>= Lisp.optimize >>= fun () ->
               init >>= inited)
        ~fini:(fini >>= finish)
        start


    let run
        ?envp
        ?args
        ?(init=Machine.return ())
        ?(fini=Machine.return ())
        ?(start=Machine.return ())
        sys proj =
      run_internal (Name.show sys.name) proj
        ?envp ?args ~boot:(init_system sys) ~init ~fini ~start

  end

  module Machine = struct
    type 'a m = 'a Knowledge.t
    include Machine.Make(Knowledge)
  end

  module Generics = Generic(Machine)
  open Machine.Syntax

  let init_system system =
    components system |>
    Set.to_list |>
    Machine.List.fold ~init:(Set.empty (module Name))
      ~f:(fun loaded name -> match Hashtbl.find analyses name with
          | Some {init} -> init >>| fun () -> Set.add loaded name
          | None -> Machine.return loaded) >>=
    Generics.do_init system

  let result_t =
    Knowledge.Domain.optional "primus-computation-result"
      ~equal:(fun (_,p1) (_,p2) ->
          phys_equal p1 p2)

  let system = Knowledge.Class.declare "system"
      ~package:"primus" ()

  let result = Knowledge.Class.property system "result" result_t
      ~public:false
      ~package:"primus"

  let run ?envp ?args
      ?(init=Machine.return ())
      ?(fini=Machine.return ())
      ?(start=Machine.return ())
      sys proj state =
    let comp =
      let open Knowledge.Syntax in
      Knowledge.Object.create system >>= fun obj ->
      Generics.run_internal (Name.show sys.name) proj
        ?envp
        ?args
        ~boot:(init_system sys)
        ~init
        ~fini
        ~start >>= fun x ->
      Knowledge.provide result obj (Some x) >>| fun () ->
      obj in
    match Knowledge.run system comp state with
    | Ok (v,state) ->
      let (status,proj) =
        Option.value_exn (Knowledge.Value.get result v) in
      Ok (status, proj, state)
    | Error c -> Error c
end


module Parser = struct
  type expectation =
    | Defsystem
    | Literal_description
    | List_of_components
    | List_of_systems
    | Keyword
    | Known_keyword
    | Component

  type error = {expects : expectation; got : Sexp.t}
  type error_with_loc = {
    file : string;
    system : string;
    error : error;
  }

  let with_no_loc error = {
    file = "unknown";
    system = "unknown";
    error;
  }

  let in_system system error = {
    file = "unknown";
    system;
    error;
  }

  let in_file file error = {
    error with file
  }

  let empty name = {name; desc=""; components=[]; depends_on=[]}

  let comp name s =
    {s with components = Name.read ~package name :: s.components}

  let deps name s =
    {s with depends_on = Name.read ~package name :: s.depends_on}

  let rec parse_specs push : t -> Sexp.t list -> (t,error) Result.t =
    fun sys -> function
      | [] -> Ok sys
      | Atom name :: comps -> parse_specs push (push name sys) comps
      | other :: _ ->
        Error {expects=Component; got=other}

  let rec parse_items : t -> Sexp.t list -> (t,error) Result.t =
    fun sys -> function
      | [] -> Ok sys
      | List _ as list :: _ ->
        Error {expects=Keyword; got=list}
      | Atom ":description" :: Atom desc :: items ->
        parse_items {sys with desc} items
      | Atom ":description" :: list :: _ ->
        Error {expects=Literal_description; got=list}
      | Atom ":description" :: [] ->
        Error {expects=Literal_description; got=List []}
      | Atom ":components" :: List comps :: items ->
        Result.bind (parse_specs comp sys comps)
          ~f:(fun sys -> parse_items sys items)
      | Atom ":depends-on" :: List comps :: items ->
        Result.bind (parse_specs deps sys comps)
          ~f:(fun sys -> parse_items sys items)
      | Atom ":components" :: (Atom _ as atom) :: _ ->
        Error {expects=List_of_components; got=atom}
      | Atom ":components" :: [] ->
        Error {expects=List_of_components; got=List []}
      | Atom ":depends-on" :: (Atom _ as atom) :: _ ->
        Error {expects=List_of_systems; got=atom}
      | Atom ":depends-on" :: [] ->
        Error {expects=List_of_systems; got=List []}
      | Atom _ as unknown :: _ ->
        Error {expects=Known_keyword; got=unknown}

  let of_sexp : Sexp.t -> (t,error_with_loc) Result.t = function
    | List (Atom "defsystem" :: Atom name :: items) ->
      parse_items (empty (Name.read ~package name)) items |>
      Result.map_error ~f:(in_system name)
    | other -> Error (in_system "unknown" {expects=Defsystem; got=other})

  let of_sexps sexps =
    List.map sexps ~f:of_sexp |>
    Result.all

  let from_file name =
    Sexp.load_sexps name |> of_sexps |>
    Result.map_error ~f:(in_file name)

  let pr ppf s = Format.fprintf ppf s

  let pp_error ppf = function
    | {expects=Defsystem; got} ->
      pr ppf
        "expected (defsystem <system-designator> <system-definition>)\
         got %a" Sexp.pp got
    | {expects=Literal_description; got=List []} ->
      pr ppf "expected :description <string>, got nothing"
    | {expects=Literal_description; got=other} ->
      pr ppf "expected: :description <string>, got %a" Sexp.pp other
    | {expects=List_of_components; got=List []} ->
      pr ppf "expects a list of components, got nothing"
    | {expects=List_of_systems; got=List []} ->
      pr ppf "expects a list of systems, got nothing"
    | {expects=List_of_components; got=atom} ->
      pr ppf "expects a list of components, got an atom %a\
              (add parentheses)" Sexp.pp atom
    | {expects=List_of_systems; got=atom} ->
      pr ppf "expects a list of systems, got an atom %a\
              (add parentheses)" Sexp.pp atom
    | {expects=Keyword; got=list} ->
      pr ppf "expects an option name, got list %a" Sexp.pp list
    | {expects=Known_keyword; got=unknown} ->
      pr ppf "expects option ::= :description | :components, \
              got an unknown option %a" Sexp.pp unknown
    | {expects=Component; got=problem} ->
      pr ppf "expects component ::= <ident>, \
              got %a" Sexp.pp problem

  let pp_error_with_loc ppf {file; system; error} =
    pr ppf "In file %S, in the definition of the system %S, %a"
      file system pp_error error
end


module Job = struct
  type t = {
    name : string;
    desc : string;
    envp : string array;
    args : string array;
    init : unit Machine.Make(Knowledge).t;
    fini : unit Machine.Make(Knowledge).t;
    start : unit Machine.Make(Knowledge).t;
    system : system;
  } [@@deriving fields]

  module Analysis = Components.Machine

  let create
      ?(name="unnamed")
      ?(desc="")
      ?(envp=[||]) ?(args=[||])
      ?(init=Analysis.return ())
      ?(fini=Analysis.return ())
      ?(start=Analysis.return ())
      system = {name; desc; envp; args; init; fini; start; system}
end

module Jobs = struct
  let jobs : Job.t Queue.t = Queue.create ()
  let enqueue ?name ?desc ?envp ?args ?init ?fini ?start sys =
    Queue.enqueue jobs @@ Job.create ?name ?desc ?envp ?args ?init ?fini ?start sys

  let pending () = Queue.length jobs

  type result = {
    project : project;
    failures : (Job.t * Knowledge.conflict) list;
    jobs : Job.t list;
    state : Knowledge.state
  }

  type action = Stop | Continue

  let knowledge t = t.state
  let project t = t.project
  let failures t = List.rev t.failures
  let finished t = List.rev t.jobs

  let success job project state result = {
    result with project;
                jobs = job :: result.jobs;
                state;
  }

  let conflict job conflict result = {
    result with
    jobs = job :: result.jobs;
    failures = (job,conflict) :: result.failures;
  }

  let run
      ?(on_failure = fun _ _ _ -> Continue)
      ?(on_success = fun _ _ _ _ -> Continue) =
    let rec process result ({Job.envp; args; init; fini; start; system} as job) =
      Components.run system result.project result.state
        ~envp ~args ~init ~fini ~start |> function
      | Ok (status,proj,state) ->
        handle_success job status state @@
        success job proj state result
      | Error problem ->
        handle_failure job problem @@
        conflict job problem result
    and handle_success job status state result =
      match on_success job status state result with
      | Continue -> continue result
      | Stop -> result
    and handle_failure job problem result =
      match on_failure job problem result with
      | Continue -> continue result
      | Stop -> result
    and continue result = match Queue.dequeue jobs with
      | None -> result
      | Some job -> process result job in
    fun project state -> continue {
        project;
        state;
        failures = [];
        jobs = [];
      }
end

let run = Components.run
module Generic = Components.Generic


type parse_error = Parser.error_with_loc
let pp_parse_error = Parser.pp_error_with_loc

let from_file = Parser.from_file

let () = Components.register_generic
    "load-binary"
    (module Bap_primus_interpreter.LinkBinaryProgram)
    ~package:"bap"
    ~desc: "Links the binary program into the Primus machine. \
            All symbols of the binary program are linked weakly, \
            i.e., if a symbol is already linked, then this component \
            will not override it."
