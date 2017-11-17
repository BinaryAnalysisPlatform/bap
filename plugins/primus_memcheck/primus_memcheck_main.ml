open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Format
include Self()


type pos = Primus.pos [@@deriving sexp_of]
type value = Primus.value [@@deriving sexp_of]

type 'a place = {
  pos : pos;
  event : 'a;
} [@@deriving sexp_of]

type allocation = Alloc of value [@@deriving sexp_of]
type free = Free of value [@@deriving sexp_of]

type violation =
  | Use_after_free
  | Write_after_free
  | Double_free
  | Dangling
  | Corrupted
[@@deriving sexp]


type t = {
  allocated : allocation place Primus.Value.Map.t;
  dead_heap : free place Primus.Value.Map.t;
  violation : violation place Primus.Value.Map.t;
} [@@deriving fields]

let state = Primus.Machine.State.declare
    ~uuid:"bf39007b-c94c-4b9f-9da6-27d8db553d72"
    ~name:"memcheck"
    (fun _ -> {
         allocated = Primus.Value.Map.empty;
         dead_heap = Primus.Value.Map.empty;
         violation = Primus.Value.Map.empty;
       })


let sexp_of_pos p = match Primus.Pos.get address p with
  | None -> sexp_of_tid (Primus.Pos.tid p)
  | Some addr -> Sexp.Atom (asprintf "%a" Addr.pp_hex addr)


let acquire,acquired =
  Primus.Observation.provide
    ~inspect:[%sexp_of : pos * (value * value)]
    "memcheck-acquire"

let release,released =
  Primus.Observation.provide
    ~inspect:[%sexp_of : pos * (value * value)]
    "memcheck-release"

let violate,violated =
  Primus.Observation.provide
    ~inspect:[%sexp_of : pos * (value * violation)]
    "memcheck-violate"

module Kernel(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Eval = Primus.Interpreter.Make(Machine)
  module Value = Primus.Value.Make(Machine)

  let add field ptr data =
    Machine.Local.update state ~f:(fun s ->
        Field.fset field s
        @@ Map.add (Field.get field s) ptr data)

  let remove field ptr =
    Machine.Local.update state ~f:(fun s ->
        Field.fset field s @@ Map.remove (Field.get field s) ptr)

  let allocate ptr size =
    Eval.pos >>= fun pos ->
    add Fields.allocated
      ptr
      {pos; event = Alloc size} >>= fun () ->
    Machine.Observation.make acquired (pos,(ptr,size)) >>= fun () ->
    Machine.return ptr

  let violation ptr event =
    Eval.pos >>= fun pos ->
    Machine.Observation.make violated (pos,(ptr,event)) >>= fun () ->
    add Fields.violation ptr {pos; event}

  let process_free ptr =
    Machine.Local.get state >>= fun s ->
    Eval.const Word.b0 >>= fun void ->
    if Value.is_zero ptr then Machine.return void
    else
      match Map.find s.allocated ptr with
      | None -> violation ptr Corrupted >>| fun () -> void
      | Some {event=Alloc size} ->
        Eval.pos >>= fun pos ->
        Machine.Observation.make released (pos,(ptr,size)) >>= fun () ->
        match Map.find s.dead_heap ptr with
        | Some _ -> violation ptr Double_free >>| fun () -> void
        | None ->
          let n = Value.to_word size |> Word.to_int_exn in
          let loc = {pos; event = Free size} in
          Machine.Seq.iter (Seq.range 0 n) ~f:(fun off ->
              Value.nsucc ptr off >>= fun ptr ->
              add Fields.dead_heap ptr loc) >>| fun () -> void

  let check event ptr =
    Machine.Local.get state >>= fun s ->
    match Map.find s.dead_heap ptr with
    | Some _ -> violation ptr event
    | None -> match Map.max_elt s.allocated with
      | None -> Machine.return ()
      | Some (upper_bound,{event=Alloc size}) ->
        Machine.return ()

  let init () = Machine.sequence Primus.Interpreter.[
      loading >>> check Use_after_free;
      storing >>> check Write_after_free;
    ]
end

module Primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  module Kernel = Kernel(Machine)


  let register_allocation = function
    | [ptr; size] -> Kernel.allocate ptr size
    | _ -> Lisp.failf "memcheck-acquire expects 2 arguments" ()

  let register_free = function
    | [ptr] -> Kernel.process_free ptr
    | _ -> Lisp.failf "memcheck-release expectrs 1 argument" ()

  let make_primitive (name, code, docs) =
    Primus.Lisp.Primitive.create ~docs name code

  let defs () = List.map ~f:make_primitive [
      "memcheck-acquire", register_allocation,
      "PTR SIZE - remembers that the memory region [pointer,
      pointer+size) was allocated by the checked memory allocator";

      "memcheck-release", register_free,
      "registers the memory chunk pointed by P as deallocated. Any
       consequent access to it will be registered as UAF. If P points to a
       memory chunk that wasn't previously allocated then it is
       registered as a memory corruption"
    ]

end

module Main(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  module Kernel = Kernel(Machine)
  let init () = Machine.sequence [
      Kernel.init ();
      Lisp.link_primitives (module Primitives)
    ]
end

let main () =
  Primus.Machine.add_component (module Main)


module Cmdline = struct
  open Config

  let enabled = flag "enable" ~doc:"enable the Primus memory checker"

  let () = Config.when_ready @@ fun {get=(!!)} ->
    if !!enabled then main ()
end
