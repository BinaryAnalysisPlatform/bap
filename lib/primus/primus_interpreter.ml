open Core_kernel.Std
open Bap.Std

open Primus_types

module Context = Primus_context
module Observation = Primus_observation
module Iterator = Primus_iterator
module Random  = Primus_random
module Generator = Primus_generator


let sexp_of_tid t = Sexp.Atom (Tid.name t)

let enter_term, term_entered =
  Observation.provide ~inspect:sexp_of_tid "enter-term"
let leave_term, term_left =
  Observation.provide ~inspect:sexp_of_tid "leave-term"

let sexp_of_level level = Sexp.Atom (Context.Level.to_string level)

let enter_level,level_entered =
  Observation.provide ~inspect:sexp_of_level "enter-level"

let leave_level, level_left =
  Observation.provide ~inspect:sexp_of_level "leave-level"

let sexp_of_term term = sexp_of_tid (Term.tid term)

let enter_sub,sub_entered =
  Observation.provide ~inspect:sexp_of_term "enter-sub"
let enter_arg,arg_entered =
  Observation.provide ~inspect:sexp_of_term "enter-arg"
let enter_blk,blk_entered =
  Observation.provide ~inspect:sexp_of_term "enter-blk"
let enter_phi,phi_entered =
  Observation.provide ~inspect:sexp_of_term "enter-phi"
let enter_def,def_entered =
  Observation.provide ~inspect:sexp_of_term "enter-def"
let enter_jmp,jmp_entered =
  Observation.provide ~inspect:sexp_of_term "enter-jmp"
let enter_top,top_entered =
  Observation.provide ~inspect:sexp_of_term "enter-top"


let leave_sub,sub_left =
  Observation.provide ~inspect:sexp_of_term "leave-sub"
let leave_arg,arg_left =
  Observation.provide ~inspect:sexp_of_term "leave-arg"
let leave_blk,blk_left =
  Observation.provide ~inspect:sexp_of_term "leave-blk"
let leave_phi,phi_left =
  Observation.provide ~inspect:sexp_of_term "leave-phi"
let leave_def,def_left =
  Observation.provide ~inspect:sexp_of_term "leave-def"
let leave_jmp,jmp_left =
  Observation.provide ~inspect:sexp_of_term "leave-jmp"
let leave_top,top_left =
  Observation.provide ~inspect:sexp_of_term "leave-top"

let variable_access,variable_will_be_looked_up =
  Observation.provide ~inspect:sexp_of_var "variable-access"

(* TODO: add sexp_of to the Bil.Result.t *)
let sexp_of_bil_result r = Sexp.Atom (Bil.Result.to_string r)
type bil_result = Bil.Result.t

let variable_read,variable_was_read =
  Observation.provide ~inspect:[%sexp_of: var * bil_result] "variable-read"

let variable_written,variable_was_written =
  Observation.provide ~inspect:[%sexp_of: var * bil_result] "variable-written"

let address_access,address_will_be_read =
  Observation.provide ~inspect:sexp_of_addr "address-access"

let address_read,address_was_read =
  Observation.provide ~inspect:[%sexp_of: addr * bil_result] "address-read"

let address_written,address_was_written =
  Observation.provide ~inspect:[%sexp_of: addr * word]
    "address-written"

module Memory = struct

  type error += Segmentation_fault of addr
  type error += Stack_overflow of addr

  let sexp_of_segmentation_fault (Segmentation_fault addr) =
    Sexp.List [Sexp.Atom "Segmentation fault"; sexp_of_addr addr]

  let sexp_of_stack_overflow (Stack_overflow addr) =
    Sexp.List [Sexp.Atom "Stack overflow"; sexp_of_addr addr]

  let segmentation_fault, segfault =
    Observation.provide ~inspect:sexp_of_segmentation_fault
      "segmentation-fault"

  let stack_overflow, stackoverflow =
    Observation.provide ~inspect:sexp_of_stack_overflow
      "stack-overflow"

  module type S = sig
    type t
    type ('a,'e) m

    val add_rodata : mem -> (unit,'e) m
    val add_rwdata : mem -> (unit,'e) m
    val add_bss : addr -> int -> (unit,'e) m

    val program_break : unit -> (addr,'e) m
    val brk : addr -> (unit,'e) m
  end

  module Make(Machine : Machine) = struct
    open Machine.Syntax

    type 'e context = 'e constraint 'e = #Context.t
    type ('a,'e) m = ('a,'e) Machine.t
    type rng = {next : 'e . unit -> (word,'e context) m}
    type chunk = {base : addr; len : int}
    type memory = {mem : mem; readonly : bool}
    type layer =
       | Static of chunk * word
       | Random of chunk * rng
       | Memory of memory
       | Rwdata of word Addr.Map.t

    type data = Data of layer list

    type state = {
      data  : data;
      brkp  : addr;
      rngs  : Univ_map.t;
    }

    type t = state
    let zero = Word.of_int ~width:8 0

    let sexp_of_t = sexp_of_opaque
    let state = Machine.Local.create ~inspect:sexp_of_t ~name:"memory"
        (fun ctxt ->
           let proj = ctxt#project in
           let arch = Project.arch proj in
           let addr_size = Arch.addr_size arch in
           {
             data = Data [];
             brkp = Addr.zero (Size.in_bits addr_size);
             rngs = Univ_map.empty;
           })


    let inside {base;len} addr =
      Addr.(addr >= base) && Addr.(base ++ len < addr)

    let update state f =
      Machine.Local.get state >>= fun s ->
      Machine.Local.put state (f s)

    let rec read addr = function
      | [] -> Machine.fail (Segmentation_fault addr)
      | Static (chunk,c) :: layers when inside chunk addr ->
        Machine.return addr
      | Random (chunk,rng) :: layers when inside chunk addr ->
        rng.next ()
      | Memory {mem} :: layers -> memory addr mem layers
      | Rwdata mapping :: layers -> rwdata addr mapping layers
      | _ :: layers -> read addr layers
    and rwdata addr mapping layers = match Map.find mapping addr with
      | None -> read addr layers
      | Some v -> Machine.return v
    and memory addr mem layers = match Memory.get ~addr mem with
      | Ok v -> Machine.return v
      | _ -> read addr layers

    let is_mapped addr layers =
      let lookup = read addr layers >>| fun _ -> true in
      Machine.catch lookup @@ function
      | Segmentation_fault _  -> Machine.return false
      | err -> Machine.fail err

    let rec is_writable addr = function
      | [] -> false
      | Static (chk,_) :: ls | Random (chk,_) :: ls ->
        inside chk addr || is_writable addr ls
      | Memory {mem;readonly=false} :: ls ->
        Memory.contains mem addr || is_writable addr ls
      | Memory {mem;readonly=true} :: ls ->
        not (Memory.contains mem addr) || is_writable addr ls
      | _ :: ls -> is_writable addr ls

    let rec write addr value = function
      | [] -> Machine.fail (Segmentation_fault addr)
      | Static (chunk,_) :: layers | Random (chunk,_) :: layers
        when inside chunk addr ->
        Machine.return (Rwdata (Addr.Map.singleton addr value) :: layers)
      | Memory m :: layers when Memory.contains m.mem addr ->
        Machine.return (Rwdata (Addr.Map.singleton addr value) :: layers)
      | Rwdata mapping :: layers ->
        let mapping = Map.add mapping ~key:addr ~data:value in
        Machine.return (Rwdata mapping :: layers)
      | layer :: layers -> write addr value layers >>| fun layers ->
        layer :: layers

    let write addr value layers =
      if is_writable addr layers
      then write addr value layers
      else Machine.fail (Segmentation_fault addr)

    let create_memory_generator (type rng)
        (module Rng : Iterator.Infinite.S
          with type t = rng and type dom = int) rng =
      let module Rngs = Univ_map.With_default in
      let key = Rngs.Key.create
          ~default:rng  ~name:"rng" sexp_of_opaque in {
        next = fun () ->
          Machine.Local.get state >>= fun s ->
          let rng = Rngs.find s.rngs key in
          let x = Rng.value rng in
          Machine.Local.put state {
            s with rngs = Rngs.set s.rngs key (Rng.next rng)
          } >>= fun () ->
          Machine.return (Word.of_int ~width:8 x)
      }

    let add_layer layer (Data layers) = Data (layer :: layers)
    let (++) = add_layer


    let uniform_generator =
      create_memory_generator (module Random.LCG) (Random.LCG.create 0)

    let set_stack ?(size=8*1024*1024) ?(rng=uniform_generator) base =
      update state @@ fun s -> {
        s with
        data = Random ({base; len=size},rng) ++ s.data;
      }

    (* the brk should be set explicitly by the ABI specific code. *)
    let add_text mem = update state @@ fun s -> {
        s with
        data = Memory {mem; readonly=true} ++ s.data;
      }

    let add_data mem = update state @@ fun s -> {
        s with
        data = Memory {mem; readonly=false} ++ s.data
      }

    let load addr =
      Machine.Local.get state >>= fun {data = Data layers} ->
      read addr layers

    let store addr value =
      Machine.Local.get state >>= fun {data = Data layers} ->
      write addr value layers >>= fun layers ->
      update state @@ fun s -> {
        s with data = Data layers
      }
  end
end

module Make (Machine : Machine) = struct
  open Machine.Syntax

  type ('a,'e) state = ('a,'e) Machine.t
  type 'a r = (Bil.result,'a) state
  type 'a u = (unit,'a) Machine.t
  module Expi = Expi.Make(Machine)
  module Biri = Biri.Make(Machine)

  let make_observation = Machine.Observation.make

  class ['e] t  =
    object
      inherit ['e] Biri.t as super
      constraint 'e = #Context.t

      method! enter_term cls t =
        super#enter_term cls t >>= fun () ->
        Machine.get () >>= fun ctxt ->
        make_observation term_entered (Term.tid t) >>= fun () ->
        match Context.Level.next ctxt#level cls t with
        | Error err -> Machine.fail err
        | Ok next ->
          Machine.put (ctxt#with_level next) >>= fun () ->
          make_observation level_left ctxt#level >>= fun () ->
          make_observation level_entered next >>= fun () ->
          Term.switch cls t
            ~program:(make_observation top_entered)
            ~sub:(make_observation sub_entered)
            ~arg:(make_observation arg_entered)
            ~blk:(make_observation blk_entered)
            ~phi:(make_observation phi_entered)
            ~def:(make_observation def_entered)
            ~jmp:(make_observation jmp_entered)

      method! leave_term cls t =
        super#leave_term cls t >>= fun () ->
        make_observation term_left (Term.tid t) >>= fun () ->
        Term.switch cls t
          ~program:(make_observation top_left)
          ~sub:(make_observation sub_left)
          ~arg:(make_observation arg_left)
          ~blk:(make_observation blk_left)
          ~phi:(make_observation phi_left)
          ~def:(make_observation def_left)
          ~jmp:(make_observation jmp_left)

      method! lookup var =
        make_observation variable_will_be_looked_up var >>= fun () ->
        super#lookup var >>= fun r ->
        make_observation variable_was_read (var,r) >>= fun () ->
        Machine.return r

      method! update var r =
        super#update var r >>= fun () ->
        make_observation variable_was_written (var,r)

      (* we need to overload storage, so that
         all memory accesses will be driven via the context. The
         Primus machine and its interperter is the specialization of
         the general Bil interperter.*)
      method! load mem addr =
        make_observation address_will_be_read addr >>= fun () ->
        super#load mem addr >>= fun r ->
        make_observation address_was_read (addr,r) >>= fun () ->
        Machine.return r

      method! store mem addr data =
        super#store mem addr data >>= fun r ->
        make_observation address_was_written (addr,data) >>= fun () ->
        Machine.return r

    end
end
