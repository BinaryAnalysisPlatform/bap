open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self()

type channel = {
  input : In_channel.t option;
  output : Out_channel.t option;
}

type state = {
  redirections : string String.Map.t;
  channels : channel Int.Map.t;
}

let default_channels = Int.Map.of_alist_exn [
    0, {
      input = Some In_channel.stdin;
      output = None;
    };
    1, {
      input = None;
      output = Some Out_channel.stdout;
    };

    2, {
      input = None;
      output = Some Out_channel.stderr;
    }
  ]

let standard_channels = [
  "<stdin>";
  "<stdout>";
  "<stderr>";
]


let fd_of_name name =
  List.find_mapi standard_channels ~f:(fun i chan ->
      if String.equal name chan then Some i else None)


let init_channels =
  List.fold ~init:default_channels ~f:(fun chans (chan,name) ->
      match fd_of_name chan with
      | None -> chans
      | Some fd ->
        info "redirecting fd %d to %s" fd name;
        let chan = if fd > 0 then {
            input = None;
            output = Some (Out_channel.create name);
          } else {
            output = None;
            input = Some (In_channel.create name);
          } in
        Map.set chans ~key:fd ~data:chan)

let init_redirections =
  List.fold ~init:String.Map.empty ~f:(fun redirs (oldname,newname) ->
      match fd_of_name oldname with
      | Some _ -> redirs
      | None -> Map.set redirs ~key:oldname ~data:newname)


let init redirs = {
  redirections = init_redirections redirs;
  channels = init_channels redirs;
}

let try_open path = Or_error.try_with (fun () -> {
      input = Some (In_channel.create path);
      output = Some (Out_channel.create ~append:true path)
    })

let try_flush {output} = Or_error.try_with @@ fun () ->
  Option.iter ~f:Out_channel.flush output

let input_byte chan = Or_error.try_with @@ fun () ->
  In_channel.input_byte chan

let next_fd channels = match Map.max_elt channels with
  | None -> 0
  | Some (fd,_) -> fd + 1


let state = Primus.Machine.State.declare
    ~name:"io-channels"
    ~uuid:"f326a07d-16ed-42ec-958d-76ef584cb341"
    (fun _ -> {
         redirections = String.Map.empty;
         channels = Int.Map.empty;
       })


let init redirections =

  let module Lib(Machine : Primus.Machine.S) = struct
    include Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    let addr_width =
      Machine.arch >>| Arch.addr_size >>| Size.in_bits
    let nil = Value.b0
    let error = addr_width >>= fun w -> Value.of_word (Word.ones w)
    let ok = addr_width >>= Value.zero
    let int x = addr_width >>= fun width -> Value.of_int ~width x

    let value_to_fd fd =
      Value.to_word fd |> Word.to_int |> function
      | Error _ -> None
      | Ok n -> Some n

    let string_of_charp ptr =
      let rec loop chars ptr =
        Eval.load ptr LittleEndian `r8 >>= fun c ->
        let c = (* [load p e `r8] must return a byte *)
          Char.of_int_exn @@
          Word.to_int_exn @@
          Value.to_word c in
        if Char.(c = '\000')
        then Machine.return (String.of_char_list (List.rev chars))
        else Value.succ ptr >>= loop (c::chars)  in
      loop [] ptr
  end in

  let module Open(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]

    let run [path] =
      string_of_charp path >>= fun path ->
      Machine.Local.get state >>= fun {redirections} ->
      match Map.find redirections path with
      | None -> error
      | Some path -> match try_open path with
        | Error _ -> error
        | Ok channel ->
          Machine.Local.get state >>= fun s ->
          let fd = next_fd s.channels in
          Machine.Local.put state {
            s with
            channels = Map.set s.channels
                ~key:(next_fd s.channels)
                ~data:channel
          } >>= fun () ->
          addr_width >>= fun width ->
          Value.of_int ~width fd
  end in

  let module Close(Machine : Primus.Machine.S) = struct
    include Lib(Machine)

    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        if Map.mem s.channels fd
        then Machine.Local.put state {
            s with channels = Map.remove s.channels fd;
          } >>= fun () -> ok
        else error
  end in

  let module Output(Machine : Primus.Machine.S) = struct
    include Lib(Machine)

    [@@@warning "-P"]
    let run (fd :: xs) =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun {channels} ->
        match Map.find channels fd with
        | Some {output=Some ch} ->
          List.iter xs ~f:(fun w ->
              Word.enum_chars (Value.to_word w) LittleEndian |>
              Seq.hd |> Option.iter ~f:(Out_channel.output_char ch));
          ok
        | _ -> error
  end in

  let module Flush(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        match Map.find s.channels fd with
        | None -> error
        | Some chan -> match try_flush chan with
          | Error _ -> error
          | Ok () -> ok
  end in

  let module Input(Machine : Primus.Machine.S) = struct
    include Lib(Machine)
    [@@@warning "-P"]
    let run [fd] =
      value_to_fd fd |> function
      | None -> error
      | Some fd ->
        Machine.Local.get state >>= fun s ->
        match Map.find s.channels fd with
        | None -> error
        | Some {input=None} -> error
        | Some {input=Some ch} -> match input_byte ch with
          | Error _ -> error
          | Ok None -> error
          | Ok (Some ch) -> int ch
  end in

  let module Primitives(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Lisp = Primus.Lisp.Make(Machine)
    module Env = Primus.Env.Make(Machine)
    module Value = Primus.Value.Make(Machine)
    module Lib = Lib(Machine)

    let setup_standard_channels =
      let set name descr =
        Lib.addr_width >>= fun width ->
        let v = Var.create name (Type.imm width) in
        Value.of_int ~width descr >>= Env.set v in
      Machine.sequence [
        set "*standard-input*" 0;
        set "*standard-output*" 1;
        set "*error-output*" 2; (* CL name *)
        set "*standard-error*" 2; (* conventional name *)
      ]

    let setup_redirections =
      Machine.Local.put state (init redirections)


    let init () =
      let open Primus.Lisp.Type.Spec in
      let def name types closure docs =
        Lisp.define ~docs ~types name closure in
      Machine.sequence [
        setup_standard_channels;
        setup_redirections;
        def "channel-open"   (one int // all int @-> int) (module Open)
          {|(channel-open PTR) creates a new channel that is
            associated with a null-terminated path pointed by PTR.
            Returns a non-negative channel descriptor, if the channel
            subsystem have a mapping from the obtained path to a
            physical file and this file is accessible. Otherwise returns
            a negative value.
          |} ;
        def "channel-close"  (one int @-> int) (module Close)
          {|(channel-close DESCR) closes a channel that has the
            specified descriptor DESCR. If no such channel exists,
            then returns -1. Otherwise returns 0. The descriptor of the
            closed channel will be reused by the consequent calls
            to `channel-open'. If the channel had any data associated
            with it and not yet flushed, then the data is discarded. |};
        def "channel-flush"  (one int @-> int) (module Flush)
          {|(channel-flush DESCR) forces data that were written to a
            channel that has the descriptor DESCR to be outputted to the
            associated destination. Returns -1 if no such channel exists or
            if in case of an IO error.|};
        def "channel-input"  (one int @-> int) (module Input)
          {|(channel-input DESC) reads one byte from a channel that
            has the descriptor DESC. Returns -1 if no such channel
            exists, or if any IO error occurs, if the channel is not
            readable, or if the channel is in the end-of-file condition.|};
        def "channel-output" (one int // all byte @-> int) (module Output)
          {|(channel-output DESCR CHAR ...) outputs one or more
            characters to a channel that has the descriptor
            DESCR. Returns -1 if no such channel exits, if a channel
            is not writable, or if any IO error occurs in an
            associated physical file. Otherwise, returns 0.
            Note: the channel system is buffered, and the actual IO
            operation (as well as errors) could be delayed until
            (channel-flush DESCR) is called. |};
      ]
  end in
  Primus.Machine.add_component (module Primitives) [@warning "-D"];
  Primus.Components.register_generic "lisp-basic-io" (module Primitives)
    ~package:"bap"
    ~desc:"Provides basic IO primitives to Primus Lisp."
