open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
open Format
include Self()

module Lisp_config = Primus_lisp_config
module Primitives = Primus_lisp_primitives
module Channels = Primus_lisp_io

let load_program paths features project =
  match Primus.Lisp.Load.program ~paths project features with
  | Ok prog -> prog
  | Error err ->
    let err = asprintf "%a" Primus.Lisp.Load.pp_error err in
    invalid_arg err

let dump_program prog =
  let margin = get_margin () in
  set_margin 64;
  printf "%a@\n%!" Primus.Lisp.Load.pp_program prog;
  set_margin margin

module Documentation = struct
  let pp_index ppf index =
    List.iter index ~f:(fun (cat,elts) ->
        fprintf ppf "* %a@\n" Primus.Lisp.Doc.Category.pp cat;
        List.iter elts ~f:(fun (name,desc) ->
            fprintf ppf "** %a@\n%a@\n"
              Primus.Lisp.Doc.Name.pp name
              Primus.Lisp.Doc.Descr.pp desc))

  let print proj =
    let module Machine = struct
      type 'a m = 'a
      include Primus.Machine.Make(Monad.Ident)
    end in
    let open Machine.Syntax in
    let module Doc = Primus.Lisp.Doc.Make(Machine) in
    let module Main = Primus.Machine.Main(Machine) in
    let print =
      Doc.generate_index >>| fun index ->
      printf "%a@\n%!" pp_index index in
    ignore (Main.run proj print)
end

module Signals(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  module Eval = Primus.Interpreter.Make(Machine)
  module Env = Primus.Env.Make(Machine)
  module Lisp = Primus.Lisp.Make(Machine)

  let value = Machine.return
  let word = Value.of_word
  let int x =
    Machine.arch >>= fun arch ->
    let word_size = Arch.addr_size arch in
    let width = Size.in_bits word_size in
    word (Word.of_int ~width x)

  let sym x = Value.Symbol.to_value x
  let var v = sym (Var.name v)
  let unit f () = [f ()]
  let one f x = [f x]
  let pair (f,g) (x,y) = [f x; g y]
  let cond j = Eval.exp (Jmp.cond j)
  let name = Value.b0
  let args = []

  let parameters name args =
    sym name ::
    List.map args ~f:Machine.return

  let call make_pars (name,args) = make_pars name args

  let signal obs kind proj =
    Lisp.signal obs @@ fun arg ->
    Machine.List.all (proj kind arg)

  let init = Machine.sequence Primus.Interpreter.[
      signal loaded (value,value) pair;
      signal stored (value,value) pair;
      signal read  (var,value) pair;
      signal written (var,value) pair;
      signal pc_change word one;
      signal jumping (value,value) pair;
      signal Primus.Linker.Trace.call parameters call;
      signal Primus.Linker.Trace.return parameters call;
      signal interrupt int one;
      Lisp.signal Primus.Machine.init (fun () -> Machine.return []);
      Lisp.signal Primus.Machine.finished (fun () -> Machine.return [])
    ]
end

let load_lisp_program dump paths features =
  let module Loader(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Lisp = Primus.Lisp.Make(Machine)
    let init () =
      Machine.get () >>= fun project ->
      let prog = load_program paths features project in
      if dump then dump_program prog;
      Lisp.link_program prog;
  end in
  Primus.Machine.add_component (module Loader)

module LispCore(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Signals = Signals(Machine)
  let print_message msg =
    Machine.return (info "%a" Primus.Lisp.Message.pp msg)

  let init () = Machine.sequence [
      Signals.init;
      Primus.Lisp.message >>> print_message;
    ]
end

module Redirection = struct
  type t = string * string

  let known_channels = Primus_lisp_io.standard_channels


  let make oldname newname =
    if String.length oldname < 1 ||
       String.length newname < 1
    then `Error ("bad redirection: expected two non-empty names")
    else if String.get oldname 0 = '<'
    then if List.mem known_channels ~equal:String.equal oldname
      then `Ok (oldname,newname)
      else `Error (sprintf "unknown channel %s, expected one of %s"
                     oldname (String.concat ~sep:", " known_channels))
    else `Ok (oldname,newname)

  let parse str = match String.split str ~on:':' with
    | [oldname; newname] -> make oldname newname
    | _ -> `Error (sprintf "bad redirection, expected <old>:<new> got %s" str)

  let print ppf (oldname,newname) =
    Format.fprintf ppf "%s:%s" oldname newname

  let convert = Config.converter parse print ("none","none")
end

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

  let documentation =
    Config.(flag ~doc:"outputs Primus Lisp documentation") "documentation" in

  let dump =
    Config.(flag ~doc:"dumps generated AST" "dump") in

  let libs =
    Config.(param (list dir) ~doc:"paths to lisp libraries" "add") in

  let features =
    Config.(param (list string) ~doc:"load specified module" "load"
              ~default:["posix"]) in

  let redirects =
    let doc = sprintf
        "establishes a redirection between an emulated file path and a
       file path on a host system. Each redirection should be of form
       <emu-name>:<real-name>, where <emu-name> could be a path or a
       a name of one of the standard channels, i.e., %s."
        (String.concat ~sep:" or " Redirection.known_channels) in
    Config.(param (list Redirection.convert) ~doc "channel-redirect") in

  Config.when_ready (fun {Config.get=(!)} ->
      if !documentation then
        Project.register_pass' ~autorun:true Documentation.print;
      let paths = [Filename.current_dir_name] @ !libs @ [Lisp_config.library] in
      let features = "init" :: !features in
      Primus.Machine.add_component (module LispCore);
      Channels.init !redirects;
      Primitives.init ();
      load_lisp_program !dump paths features)
