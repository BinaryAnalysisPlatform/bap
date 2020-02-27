open Core_kernel
open Bap.Std
open Bap_primus.Std

include Self()

type state = {
  consts : Primus.Value.Id.Set.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"caf680aa-ef54-4468-9a24-4a125de6ace9"
    ~name:"constant-tracker"
    (fun _ -> {consts = Primus.Value.Id.Set.empty})

let is_const x {consts} =
  Set.mem consts (Primus.Value.id x)

let all_consts xs s =
  List.for_all xs ~f:(fun x -> is_const x s)

let declare_const v {consts} = {
  consts = Set.add consts (Primus.Value.id v)
}

module IsConst(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  let run inputs =
    Machine.Local.get state >>| all_consts inputs >>= function
    | true -> Value.b1
    | false -> Value.b0
end

module IsConstBuffer(Machine : Primus.Machine.S) = struct
  open Machine.Syntax
  module Value = Primus.Value.Make(Machine)
  module Memory = Primus.Memory.Make(Machine)

  let points_to_static_data s ptr len =
    let rec check ptr len =
      if Word.is_zero len
      then Value.b1
      else
        Memory.get ptr >>= fun v ->
        if is_const v s
        then check (Addr.succ ptr) (Word.pred len)
        else Value.b0 in
    check (Value.to_word ptr) (Value.to_word len)


  [@@@warning "-P"]
  let run [ptr; len] =
    Machine.Local.get state >>= fun s ->
    points_to_static_data s ptr len
end


module Primitives(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let init () = Machine.sequence [
      Lisp.define "all-static-constant" (module IsConst)
        ~types:(all any @-> bool)
        ~docs:"(all-static-constant X Y ..) is true if X,Y,... are static constants.
    A value is a static constant if it was initialized from a constant
    value or computed from static constant values. ";
      Lisp.define "points-to-static-data" (module IsConstBuffer)
        ~types:(tuple [int; int] @-> bool)
        ~docs: "(points-to-static-data PTR LEN) is true iff
          (all-static-constant *PTR .. *(PTR+LEN-1))"
    ]
end

module Tracker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let intro v = Machine.Local.update state ~f:(declare_const v)

  let propagate inputs output =
    Machine.Local.get state >>= fun s ->
    if all_consts inputs s
    then Machine.Local.put state (declare_const output s)
    else Machine.return ()

  let prop_binop ((_,x,y),z) = propagate [x;y] z
  let prop_unop ((_,x),z) = propagate [x] z
  let prop_cast ((_,_,x),z) = propagate [x] z
  let prop_extract ((_,_,x),z) = propagate [x] z
  let prop_concat ((x,y),z) = propagate [x;y] z


  let init () = Machine.sequence Primus.Interpreter.[
      const >>> intro;
      binop >>> prop_binop;
      unop  >>> prop_unop;
      cast  >>> prop_cast;
      extract >>> prop_extract;
      concat >>> prop_concat;
    ]
end

let enable = Config.flag "enable"

let () = Config.when_ready (fun {Config.get} ->
    Primus.Components.register_generic "constant-tracker-primitives"
      (module Primitives)
      ~package:"bap"
      ~desc:"exposes the constant tracker to Primus Lisp";
    if get enable then
      Primus.Components.register_generic "constant-tracker"
        (module Tracker)
        ~package:"bap"
        ~desc:"tracks constants")
