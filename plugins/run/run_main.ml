open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
include Self()
open Format

type value = Primus.Generator.t

type parameters = {
  argv  : string list;
  envp  : (string * string) list;
  entry : string option;
  memory : (addr * value) list;
  variables : (string * value) list;
}


module Param = struct
  open Config;;

  manpage [
    `S "DESCRIPTION";
    `P "Run a program in the Primus emulator. ";
  ];;

  let argv = param (array string)  "argv"
      ~doc:"Program command line arguments";;

  let envp = param (array string) "env"
      ~doc:"Program environemt as a comma separated list of VAR=VAL pairs";;

  let entry = param (list string) "entry-points"
      ~doc:

        "Can a list of entry points or a special keyword
      $(b,all-subroutines). An entry point is either a string denoting
      a function name, a tid starting with the $(b,%) percent, or an
      address in a hexadecimal format starting prefixed with $(b,0x).
      When the option is specified, the Primus Machine will start the
      execution from the specified entry point(s). Otherwise the
      execution will be started from all program terms that are marked
      with the [entry_point] attribute. If there are several entry
      points, then the execution will be started from all of them in
      parallel, i.e., by forking the machine and starting each machine
      from its own entry point. Consider enabling corresponding
      scheduler. If neither the argument nor there any entry points in
      the program, then a function called $(b,_start) is called.";;

end


let pp_id = Monad.State.Multi.Id.pp

module Machine = struct
  type 'a m = 'a
  include Primus.Machine.Make(Monad.Ident)
end
open Machine.Syntax

module Main = Primus.Machine.Main(Machine)
module Interpreter = Primus.Interpreter.Make(Machine)
module Linker = Primus.Linker.Make(Machine)

let string_of_name = function
  | `symbol s -> s
  | `tid t -> Tid.to_string t
  | `addr x -> Addr.string_of_value x

let name_of_entry arch entry =
  let width = Arch.addr_size arch |> Size.in_bits in
  if String.is_empty entry
  then invalid_arg "An entry point should be a non-empty string"
  else match entry.[0] with
    | '%' -> `tid (Tid.from_string_exn entry)
    | '0' -> `addr (Addr.of_int64 ~width (Int64.of_string entry))
    | _ -> `symbol entry

let entry_point_collector = object
  inherit [Primus.Linker.name list] Term.visitor
  method! enter_term cls t entries =
    if Term.has_attr t Sub.entry_point then
      Term.proj cls t ~sub:(fun s -> Some (Sub.name s)) |> function
      | None -> `tid (Term.tid t) :: entries
      | Some name -> `symbol name :: entries
    else entries
end

let all_subroutines prog =
  Term.enum sub_t prog |> Seq.map ~f:(fun t ->
      `symbol (Sub.name t)) |>
  Seq.to_list

let entry_points proj entry = match entry with
  | ["all-subroutines"] -> all_subroutines (Project.program proj)
  | [] -> entry_point_collector#run (Project.program proj) []
  | xs -> List.map ~f:(name_of_entry (Project.arch proj)) xs

let exec x =
  Machine.current () >>= fun cid ->
  info "Fork %a: starting from the %s entry point"
    pp_id cid (string_of_name x);
  Machine.catch (Linker.exec x)
    (fun exn ->
       info "execution from %s terminated with: %s "
         (string_of_name x)
         (Primus.Exn.to_string exn);
       Machine.return ())

let run_entries = function
  | [] -> exec (`symbol "_start")
  | x :: xs ->
    Machine.List.iter xs ~f:(fun x ->
        Machine.current () >>= fun pid ->
        if pid = Machine.global
        then
          Machine.fork () >>= fun () ->
          Machine.current () >>= fun cid ->
          if cid = Machine.global
          then Machine.return ()
          else
            Machine.switch pid >>= fun () ->
            Machine.current () >>= fun xid ->
            exec x
        else Machine.return ()) >>= fun () ->
    Machine.current () >>= fun id ->
    if id = Machine.global
    then exec x
    else Machine.return ()


let main {Config.get=(!)} proj =
  let open Param in
  entry_points proj !entry |> run_entries |>
  Main.run ~envp:!envp ~args:!argv proj |> function
  | (Primus.Normal,proj)
  | (Primus.Exn Primus.Interpreter.Halt,proj) ->
    info "Ok, we've terminated normally";
    proj
  | (Primus.Exn exn,proj) ->
    info "program terminated by a signal: %s









" (Primus.Exn.to_string exn);
    proj

let () =
  Config.when_ready (fun conf ->
      Project.register_pass (main conf))
