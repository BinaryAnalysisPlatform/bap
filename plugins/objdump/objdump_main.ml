let doc = {|
# DESCRIPTION

This plugin provides a symbolizer based on objdump.
Note that we parse objdump output, thus this symbolizer
is potentially fragile to changes in objdumps output.

# EXAMPLES

To view the symbols after running the plugin:

```
bap ./exe -dsymbols
```

To view symbols without this plugin:

```
bap ./exe --no-objdump -dsymbols
```

# SEE ALSO

$(b,bap-plugin-ida)(1)
|}


open Bap_core_theory
open Core_kernel[@@warning "-D"]
open Objdump_config
open Bap_main

module Log = Bap_main_event.Log
module Unix = Caml_unix

open KB.Syntax

let default_objdump_opts = "-rd --no-show-raw-insn"

include Bap_main_event.Log.Create()

let demangler = Extension.Configuration.parameter
    Extension.Type.(some string) "demangler"
    ~doc:"Specify the demangler name. \
          Set to $(i,disabled) to disable demangling."


let objdump_cmds demangler=
  String.Set.stable_dedup_list objdumps|>
  List.map ~f:(fun cmd ->
      sprintf "%s %s %s" cmd default_objdump_opts @@
      match demangler with
      | Some "disabled" -> ""
      | None -> "-C"
      | Some other -> "--demangle="^other)

module Regexp = struct
  let parse =
    let func_start_re = {|([0-9A-Fa-f]+?) <(.*?)(@plt)?>:|} in
    Re.Pcre.re func_start_re |> Re.compile |> Re.exec

  let parse_addr input ~start ~stop =
    Bitvec.of_substring_base 16 input ~pos:start ~len:(stop - start)
end

(* func_start ::=
    | addr,space, "<", name, ">", ":"
    | addr,space, "<", name, "@plt", ">", ":" *)
let parse_func_start input accept init =
  try
    let groups = Regexp.parse input in
    let addr = Regexp.parse_addr input
        ~start:(Re.Group.start groups 1)
        ~stop:(Re.Group.stop groups 1)
    and name = Re.Group.get groups 2 in
    info "%s => %s" (Bitvec.to_string addr) name;
    accept init addr name
  with _ -> init

let run cmd ~f ~init : _ Base.Continue_or_stop.t =
  let env = Unix.environment () in
  let stdin,stdout,stderr = Unix.open_process_full cmd env in
  let data = In_channel.fold_lines stdin ~f ~init in
  match Unix.close_process_full (stdin,stdout,stderr) with
  | Unix.WEXITED 0 -> Stop data
  | Unix.WEXITED n ->
    info "`%s' has failed with %d" cmd n;
    Continue data
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    (* a signal number is internal to OCaml, so don't print it *)
    info "command `%s' was terminated by a signal" cmd;
    Continue data

let with_objdump_output demangler ~file ~f ~init =
  objdump_cmds demangler |>
  List.fold_until ~init ~f:(fun data objdump ->
      let cmd = sprintf "%s %S" objdump file in
      run cmd ~f ~init:data)
    ~finish:Fn.id

let agent =
  KB.Agent.register ~package:"bap" "objdump-symbolizer"
    ~desc:"extracts symbols objdump"
    ~reliability:KB.Agent.doubtful

module Repository : sig
  type t
  type info
  val create : (string -> (info -> Bitvec.t -> string -> info) -> info -> info) -> t
  val name : t -> ?size:int -> ?bias:Bitvec.t -> path:string -> Bitvec.t -> string option
  val aliases : t -> ?size:int -> ?bias:Bitvec.t -> path:string -> Bitvec.t -> Set.M(String).t option
end = struct
  type info = (Bitvec.t, String.t) Bap_relation.t

  type table = {
    names   : string Map.M(Bitvec_order).t;
    aliases : Set.M(String).t Map.M(Bitvec_order).t;
  }

  let empty_table = {
    names   = Map.empty (module Bitvec_order);
    aliases = Map.empty (module Bitvec_order);
  }

  type t = {
    parse : string -> (info -> Bitvec.t -> string  -> info) -> info -> info;
    files : (string, table) Hashtbl.t
  }

  let empty = Bap_relation.empty
      Bitvec.compare
      String.compare

  let create parse = {
    parse;
    files = Hashtbl.create (module String);
  }

  let string_of_addrs addrs =
    String.concat ~sep:", " @@ List.map addrs ~f:Bitvec.to_string

  let string_of_names names =
    String.concat ~sep:", " names

  let of_info symbols =
    let add_aliases names t addr = {
      t with aliases = Map.update t.aliases addr ~f:(function
        | Some s -> Set.union s names
        | None -> names)
    } in
    Bap_relation.matching symbols empty_table
      ~saturated:(fun key data t -> {
            t with names = Map.add_exn t.names ~key ~data;
          })
      ~unmatched:(fun reason t -> match reason with
          | Non_injective_fwd (addrs, name) ->
            info "skipping addresses (%s) that has the same name %S"
              (string_of_addrs addrs) name;
            let names = Set.singleton (module String) name in
            List.fold addrs ~init:t ~f:(add_aliases names)
          | Non_injective_bwd (names, addr) ->
            info "skipping names (%s) that has the same address %a"
              (string_of_names names) Bitvec.pp addr;
            let names = Set.of_list (module String) names in
            add_aliases names t addr)

  let lookup {parse; files} path =
    match Hashtbl.find files path with
    | Some info -> info
    | None ->
      let info = parse path Bap_relation.add empty in
      if Bap_relation.is_empty info
      then warning "failed to obtain symbols";
      let t = of_info info in
      Hashtbl.set files path t;
      t

  let to_real size = function
    | None -> Fn.id
    | Some bias -> fun addr ->
      Bitvec.((addr - bias) mod modulus size)

  let of_real size = function
    | None -> Fn.id
    | Some bias -> fun addr ->
      Bitvec.((addr + bias) mod modulus size)

  let name repo ?(size=32) ?bias ~path addr =
    Map.find (lookup repo path).names (to_real size bias addr)

  let aliases repo ?(size=32) ?bias ~path addr =
    Map.find (lookup repo path).aliases (to_real size bias addr)
end

let create_repo ctxt =
  let demangler = Extension.Configuration.get ctxt demangler in
  Repository.create (fun file accept init ->
      with_objdump_output demangler ~file ~init
        ~f:(fun info line -> parse_func_start line accept info))

let provide_function_starts_and_names repo =
  let declare name input output =
    KB.Rule.(declare ~package:"bap" name |>
             dynamic ["objdump"] |>
             require input |>
             provide output |>
             comment @@ sprintf "extracts %s from objdump" name) in
  let property promise slot key_slot f =
    promise slot @@ fun label ->
    KB.collect Theory.Label.unit label >>=? fun unit ->
    KB.collect Theory.Unit.path unit >>=? fun path ->
    KB.collect Theory.Unit.bias unit >>= fun bias ->
    KB.collect Theory.Unit.target unit >>|
    Theory.Target.code_addr_size >>= fun size ->
    KB.collect key_slot label >>|? fun key ->
    f (Repository.name repo ~size ?bias ~path key) in
  let is_known = function
    | None -> None
    | Some _ -> Some true in
  let open Theory.Label in
  declare "subroutines"  addr is_subroutine;
  declare "names" addr possible_name;
  property KB.promise is_subroutine addr is_known;
  property (KB.propose agent) possible_name addr Fn.id

let provide_aliases repo =
  let open Theory.Label in
  KB.Rule.(declare ~package:"bap" "aliases" |>
           dynamic ["objdump"] |>
           require addr |>
           provide aliases |>
           comment "extracts aliases from objdump");
  let empty = Set.empty (module String) in
  KB.promise aliases @@ fun label ->
  KB.collect unit label >>= function
  | None -> !!empty
  | Some unit ->
    KB.collect Theory.Unit.path unit >>= function
    | None -> !!empty
    | Some path ->
      KB.collect Theory.Unit.bias unit >>= fun bias ->
      KB.collect Theory.Unit.target unit >>|
      Theory.Target.code_addr_size >>= fun size ->
      KB.collect addr label >>| function
      | None -> empty
      | Some addr ->
        Repository.aliases repo ~size ?bias ~path addr |> function
        | Some aliases -> aliases
        | None -> empty

let main ctxt =
  let repo = create_repo ctxt in
  provide_function_starts_and_names repo;
  provide_aliases repo;
  Ok ()

let () = Extension.declare main
    ~doc ~provides:["symbolizer"; "rooter"]
