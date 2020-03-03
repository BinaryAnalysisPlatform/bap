open Bap.Std
open Bap_primus_types
open Bap_knowledge
open Core_kernel

module Name = Knowledge.Name
module Observation = Bap_primus_observation

let fini,finish =
  Observation.provide ~inspect:sexp_of_unit "fini"

let init,inited =
  Observation.provide ~inspect:sexp_of_unit "init"

type component_specification = {
  name : Name.t;
  drop : bool;
}

type t = {
  name : Name.t;
  desc : string;
  components : component_specification list;
}

type system = t



let pp_components =
  Format.pp_print_list ~pp_sep:Format.pp_print_space @@ fun ppf c ->
  if c.drop
  then Format.fprintf ppf "(exclude %a)" Name.pp c.name
  else Format.fprintf ppf "%a" Name.pp c.name

let pp ppf {name; components; desc} =
  Format.fprintf ppf
    "@[<v2>(defsystem %a@\n\
     :description %s@\n\
     :components @[<v2>(%a)@])@]"
    Name.pp name desc pp_components components

let component ?package name = {
  drop = false;
  name = Name.create ?package name;
}

let exclude item = {item with drop=true}

let all_components =
  component ~package:"keyword" "all-components"

let define
    ?(desc="")
    ?(components=[all_components])
    ?package name = {name = Name.create ?package name; desc; components}

let default = define "default-system"
    ~package:"bap-primus"
    ~desc:"the default system that includes everything"

let name t = t.name

let has_component {components} name =
  List.exists components ~f:(fun c ->
      Name.equal c.name all_components.name ||
      Name.equal c.name name)
  && List.for_all components ~f:(fun c ->
      not (Name.equal c.name name && c.drop))

module Components = struct
  open Bap_primus_types

  type 'a item = {init : 'a; desc : string}

  let generics = Hashtbl.create (module Name)
  let analyses = Hashtbl.create (module Name)

  let add_component ns table ?(desc="") ?(package="user") name init =
    let name = Name.create ~package name in
    if Hashtbl.mem table name
    then invalid_argf
        "A %s component named %s is already registered,\
         please choose a unique name" ns (Name.show name) ();
    Hashtbl.add_exn table name {init; desc}


  let register_generic = add_component "generic" generics
  let register = add_component "specialized" analyses

  module Generic(Machine : Machine) = struct
    open Machine.Syntax
    module Lisp = Bap_primus_lisp.Make(Machine)


    let inited () =
      Machine.Observation.make inited ()

    let finish () =
      Machine.Observation.make finish ()

    let do_init system loaded =
      Hashtbl.to_alist generics |>
      Machine.List.iter ~f:(fun (name,{init=(module Gen : Component)}) ->
          if has_component system name
          && not (Set.mem loaded name)
          then
            let module Comp = Gen(Machine) in
            Comp.init ()
          else Machine.return ())
    let init s = do_init s (Set.empty (module Name))

    let run_components components = Machine.run @@begin
        components >>= fun () ->
        Lisp.typecheck >>= fun () ->
        Lisp.optimize () >>= fun () ->
        Machine.catch (inited ()) (fun exn ->
            finish () >>= fun () ->
            Machine.raise exn) >>= fun () ->
        finish ()
      end

    let run ?(env=[||]) ?(argv=[||]) sys proj =
      Machine.run (init sys) proj env argv
  end

  module Machine = struct
    type 'a m = 'a Knowledge.t
    include Bap_primus_machine.Make(Knowledge)
  end

  module Generics = Generic(Machine)
  open Machine.Syntax

  let init system =
    Hashtbl.to_alist analyses |>
    Machine.List.fold ~init:(Set.empty (module Name))
      ~f:(fun loaded (name,{init}) ->
          if has_component system name
          then init >>| fun () -> Set.add loaded name
          else Machine.return loaded) >>=
    Generics.do_init system

  let result_t =
    Knowledge.Domain.optional "primus-computation-result"
      ~equal:(fun (_,p1) (_,p2) ->
          phys_equal p1 p2)

  let system = Knowledge.Class.declare "system"
      ~package:"primus" ()

  let result = Knowledge.Class.property system "result" result_t
      ~package:"primus"

  let run ?(env=[||]) ?(argv=[||]) sys proj state =
    let comp =
      let open Knowledge.Syntax in
      Knowledge.Object.create system >>= fun obj ->
      Generics.run_components (init sys) proj env argv >>= fun x ->
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

  let empty name = {name; desc=""; components=[]}

  let push_name ?(drop=false) name s =
    {s with components = {name=Name.read name; drop} :: s.components}

  let rec parse_components : t -> Sexp.t list -> (t,error) Result.t =
    fun sys -> function
      | [] -> Ok sys
      | Atom name :: comps -> parse_components (push_name name sys) comps
      | List [Atom "exclude"; Atom name] :: comps ->
        parse_components (push_name ~drop:true name sys) comps
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
        Result.bind (parse_components sys comps)
          ~f:(fun sys -> parse_items sys items)
      | Atom ":components" :: (Atom _ as atom) :: _ ->
        Error {expects=List_of_components; got=atom}
      | Atom ":components" :: [] ->
        Error {expects=List_of_components; got=List []}
      | Atom _ as unknown :: _ ->
        Error {expects=Known_keyword; got=unknown}

  let of_sexp : Sexp.t -> (t,error_with_loc) Result.t = function
    | List (Atom "defsystem" :: Atom name :: items) ->
      parse_items (empty (Name.read name)) items |>
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
    | {expects=List_of_components; got=atom} ->
      pr ppf "expects a list of components, got an atom %a\
              (add parentheses)" Sexp.pp atom
    | {expects=Keyword; got=list} ->
      pr ppf "expects an option name, got list %a" Sexp.pp list
    | {expects=Known_keyword; got=unknown} ->
      pr ppf "expects option ::= :description | :components, \
              got an unknown option %a" Sexp.pp unknown
    | {expects=Component; got=problem} ->
      pr ppf "expects component ::= <name> | (exclude <name>), \
              got %a" Sexp.pp problem

  let pp_error_with_loc ppf {file; system; error} =
    pr ppf "In file %S, in the definition of the system %S, %a"
      file system pp_error error
end


module Jobs = struct
  let jobs = Queue.create ()
  let enqueue sys = Queue.enqueue jobs sys
  let pending () = Queue.length jobs

  type result = {
    project : project;
    conflicts : (system * Knowledge.conflict) list;
    systems : system list;
    state : Knowledge.state
  }

  type action = Stop | Continue

  let knowledge t = t.state
  let project t = t.project
  let conflicts t = List.rev t.conflicts
  let systems t = List.rev t.systems

  let success system project state result = {
    result with project;
                systems = system :: result.systems;
                state;
  }

  let conflict system conflict result = {
    result with
    systems = system :: result.systems;
    conflicts = (system,conflict) :: result.conflicts
  }

  let run ?env ?argv
      ?(on_conflict = fun _ _ -> Continue)
      ?(on_success = fun _ _ _ -> Continue) =
    let rec process result system =
      match Components.run ?env ?argv system result.project result.state with
      | Ok (status,proj,state) ->
        handle_success system status proj state result
      | Error conflict ->
        handle_conflict system conflict result
    and handle_success system status proj state result =
      match on_success system status state with
      | Continue -> continue (success system proj state result)
      | Stop -> result
    and handle_conflict system problem result =
      match on_conflict system problem with
      | Continue -> continue (conflict system problem result)
      | Stop -> result
    and continue result = match Queue.dequeue jobs with
      | None -> result
      | Some job -> process result job in
    fun project state -> continue {
        project;
        state;
        conflicts = [];
        systems = [];
      }
end

let run = Components.run
module Generic = Components.Generic


type parse_error = Parser.error_with_loc
let pp_parse_error = Parser.pp_error_with_loc

let from_file = Parser.from_file

let () = Components.register_generic
    "binary-program"
    (module Bap_primus_interpreter.LinkBinaryProgram)
    ~package:"primus"
    ~desc:"links the binary program into the Primus machine"
