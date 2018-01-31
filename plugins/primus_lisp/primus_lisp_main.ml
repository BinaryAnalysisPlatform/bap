open Core_kernel
open Bap.Std
open Bap_primus.Std
open Format
include Self()

let deps = [
  "trivial-condition-form"
]

module Lisp_config = Primus_lisp_config


let load_program paths features project =
  match Primus.Lisp.Load.program ~paths project features with
  | Ok prog -> prog
  | Error err ->
    eprintf "%a@\n" Primus.Lisp.Load.pp_error err;
    exit 2

let dump_program prog =
  let margin = get_margin () in
  set_margin 64;
  printf "%a@\n%!" Primus.Lisp.Load.pp_program prog;
  set_margin margin

type state = {
  addrs : addr Tid.Map.t;
}

let update_addr t m = match Term.get_attr t address with
  | None -> m
  | Some addr -> Map.add m ~key:(Term.tid t) ~data:addr

let compute_addresses prog = (object
  inherit [addr Tid.Map.t] Term.visitor
  method! enter_sub = update_addr
  method! enter_blk = update_addr
end)#run prog Tid.Map.empty

let state = Primus.Machine.State.declare
    ~inspect:sexp_of_opaque
    ~uuid:"23D4C1C4-784E-42DC-B8C9-B4C4268B0688"
    ~name:"stdlib-signals"
    (fun proj -> {addrs = compute_addresses (Project.program proj)})

module Signals(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)

  let value = Machine.return
  let word = Value.of_word

  let sym x = Value.Symbol.to_value x
  let var v = sym (Var.name v)

  let one f x = [f x]
  let pair (f,g) (x,y) = [f x; g y]

  let cond j = match Jmp.cond j with
    | Bil.Var v -> Env.get v
    | Bil.Int x -> word x
    | _ -> failwith "TCF violation"

  let jmp (cond,dst) j = [cond j;dst j]

  let name = Value.b0
  let args = []

  let parameters name args =
    sym name ::
    List.map args ~f:Machine.return

  let call make_pars (name,args) = make_pars name args

  let label j = match Jmp.kind j with
    | Call c -> Some (Call.target c)
    | Goto l | Ret l -> Some l
    | Int _  -> None

  let addr j = match label j with
    | None -> Machine.return None
    | Some (Direct tid) ->
      Machine.Local.get state >>| fun {addrs} ->
      Map.find addrs tid
    | Some (Indirect (Bil.Int x)) -> Machine.return (Some x)
    | _ -> Machine.return None

  let dst j = addr j >>= function
    | None -> Value.b0
    | Some x -> word x

  let signal obs kind proj =
    Lisp.signal obs @@ fun arg ->
    Machine.List.all (proj kind arg)

  let init = Machine.sequence Primus.Interpreter.[
      signal loaded (value,value) pair;
      signal stored (value,value) pair;
      signal read  (var,value) pair;
      signal written (var,value) pair;
      signal pc_change word one;
      signal enter_jmp (cond,dst) jmp;
      signal Primus.Linker.Trace.call parameters call;
      signal Primus.Linker.Trace.return parameters call;
    ]

end

let main dump paths features project =
  let prog = load_program paths features project in
  if dump then dump_program prog;

  let module Loader(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Lisp = Primus.Lisp.Make(Machine)
    module Signals = Signals(Machine)

    let print_message msg =
      Machine.return (info "%a" Primus.Lisp.Message.pp msg)


    let init () = Machine.sequence [
        Lisp.link_program prog;
        Signals.init;
        Primus.Lisp.message >>> print_message;
      ]
  end in
  Primus.Machine.add_component (module Loader)

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "Installs and registers the Primus Lisp library. The library
      implements stdlib interface in Lisp. Only $(i,init) module is
      loaded automatically.";
    `P
      "The plugin also provides an interface through which it is
  possible to load new libraries and modules, or to load existing
  modules.";

    `S "SEE ALSO";
    `P "$(b,bap-primus)(3) $(b,bap-run)(1)"
  ];

  let dump =
    Config.(flag ~doc:"dumps generated AST" "dump") in

  let libs =
    Config.(param (list dir) ~doc:"paths to lisp libraries" "add") in

  let features =
    Config.(param (list string) ~doc:"load specified module" "load"
              ~default:["posix"]) in

  Config.when_ready (fun {Config.get=(!)} ->
      let paths = [Filename.current_dir_name] @ !libs @ [Lisp_config.library] in
      let features = "init" :: !features in
      Project.register_pass' (main !dump paths features))
