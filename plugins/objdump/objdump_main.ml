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
open Core_kernel
open Objdump_config
open Bap_main

module Log = Bap_main_event.Log

open KB.Syntax

let default_objdump_opts = "-rd --no-show-raw-insn"

include Bap_main_event.Log.Create()

let demangler = Extension.Configuration.parameter
    Extension.Type.(some string) "demangler"
    ~doc:"Specify the demangler name. \
          Set to $(i,disabled) to disable demangling."


let objdump_cmds demangler=
  objdump ::
  List.map targets ~f:(fun p -> p^"-objdump") |>
  String.Set.stable_dedup_list |>
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
    accept name addr init
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
    ~finish:ident

let agent =
  KB.Agent.register ~package:"bap" "objdump-symbolizer"
    ~desc:"extracts symbols objdump"

module Repository : sig
  type t
  type info
  val create : (string -> (string -> Bitvec.t -> info -> info) -> info -> info) -> t
  val name : t -> ?bias:Bitvec.t -> path:string -> Bitvec.t -> string option
  val addr : t -> ?bias:Bitvec.t -> path:string -> string -> Bitvec.t option
end = struct
  type info = {
    names : string Map.M(Bitvec_order).t;
    addrs : Bitvec.t list Map.M(String).t;
  }

  type t = {
    parse : string -> (string -> Bitvec.t -> info -> info) -> info -> info;
    files : (string, info) Hashtbl.t
  }

  let create parse = {
    parse;
    files = Hashtbl.create (module String);
  }

  let lookup {parse; files} path =
    match Hashtbl.find files path with
    | Some info -> info
    | None ->
      let accept name addr {names; addrs} = {
        names = Map.set names addr name;
        addrs = Map.add_multi addrs name addr;
      } in
      let info = parse path accept {
          names = Map.empty (module Bitvec_order);
          addrs = Map.empty (module String);
        } in
      if Map.is_empty info.names
      then warning "failed to obtain symbols";
      Hashtbl.set files path info;
      info

  let to_real = function
    | None -> ident
    | Some bias -> fun addr -> Bitvec.M32.(addr - bias)

  let of_real = function
    | None -> ident
    | Some bias -> fun addr -> Bitvec.M32.(addr + bias)

  let name repo ?bias ~path addr =
    let {names} = lookup repo path in
    Map.find names (to_real bias addr)

  let addr repo ?bias ~path name =
    let {addrs} = lookup repo path in
    match Map.find addrs name with
    | Some [addr] -> Some (of_real bias addr)
    | _ -> None
end


let provide_function_starts_and_names ctxt : unit =
  let demangler = Extension.Configuration.get ctxt demangler in
  let repo = Repository.create (fun file accept init ->
      with_objdump_output demangler ~file ~init
        ~f:(fun info line -> parse_func_start line accept info)) in
  let declare name input output =
    KB.Rule.(declare ~package:"bap" name |>
             dynamic ["objdump"] |>
             require input |>
             provide output |>
             comment @@ sprintf "extracts %s from objdump" name) in
  let property lookup promise slot key_slot f =
    promise slot @@ fun label ->
    KB.collect Theory.Label.unit label >>=? fun unit ->
    KB.collect Theory.Unit.path unit >>=? fun path ->
    KB.collect Theory.Unit.bias unit >>= fun bias ->
    KB.collect key_slot label >>|? fun key ->
    f (lookup repo ?bias ~path key) in
  let is_known = function
    | None -> None
    | Some _ -> Some true in
  let open Theory.Label in
  declare "subroutines"  addr is_subroutine;
  declare "names" addr possible_name;
  declare "addrs" name addr;
  property Repository.name KB.promise is_subroutine addr is_known;
  property Repository.name (KB.propose agent) possible_name addr ident;
  property Repository.addr KB.promise addr name ident

let main ctxt =
  provide_function_starts_and_names ctxt;
  Ok ()

let () = Extension.declare main
    ~doc ~provides:["symbolizer"; "rooter"]
