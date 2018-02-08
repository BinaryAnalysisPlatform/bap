open Core_kernel.Std
open Bap_primus.Std
open Bap_taint.Std
open Format

module Support(Id : sig val name : string end)
    (Machine : Primus.Machine.S) = struct
  module Policy = Taint.Propagation.Policy.Make(Machine)
  module Taint = Taint.Tracker(Machine)
  module Value = Primus.Value.Make(Machine)
  open Machine.Syntax

  let name =
    Value.Symbol.to_value Id.name >>=
    Policy.of_value

  let (-->) rs rd srcs dst =
    name >>= Policy.kinds >>= fun ks ->
    Set.to_sequence ks |> Machine.Seq.iter ~f:(fun k ->
        Taint.transfer k rs rd srcs dst)
end


module Computation(Machine : Primus.Machine.S) = struct
  open Taint.Rel
  module Id = struct
    let name = "propagate-by-computation"
  end
  module Eval = Primus.Interpreter.Make(Machine)
  module Taint = Taint.Tracker(Machine)
  module Value = Primus.Value.Make(Machine)
  module Support = Support(Id)(Machine)
  open Machine.Syntax
  open Support

  let one f (x,y) = f [x] y

  let loaded = one (indirect --> direct)
  let computed = one (direct --> direct)
  let concat ((x,y),r) = (direct --> direct) [x;y] r
  let binop ((_op,x,y),r) = (direct --> direct) [x;y] r
  let unop ((_op,x),r) = computed (x,r)
  let extract ((_,_,x),r) = computed (x,r)
  let cast ((_,_,x),r) = computed (x,r)

  let stored (x,y) =
    Taint.lookup x indirect >>= fun ts ->
    Taint.detach x indirect ts >>= fun () ->
    (direct --> indirect) [y] x

  let init () = Machine.sequence Primus.[
      Interpreter.loaded  >>> loaded;
      Interpreter.stored  >>> stored;
      Interpreter.binop   >>> binop ;
      Interpreter.unop    >>> unop;
      Interpreter.extract >>> extract;
      Interpreter.concat  >>> concat;
      Interpreter.cast    >>> cast;
    ]
end

module Exact(Machine : Primus.Machine.S) = struct
  open Taint.Rel
  module Id = struct
    let name = "propagate-exactly"
  end
  module Eval = Primus.Interpreter.Make(Machine)
  module Taint = Taint.Tracker(Machine)
  module Value = Primus.Value.Make(Machine)
  module Support = Support(Id)(Machine)

  open Machine.Syntax
  open Support

  let name = Value.Symbol.to_value

  let one f (x,y) = f [x] y

  let loaded = one (indirect --> direct)
  let computed = one (direct --> direct)
  let concat ((x,y),r) = (direct --> direct) [x;y] r
  let extract ((_,_,x),r) = computed (x,r)
  let cast ((_,_,x),r) = computed (x,r)

  let stored (x,y) =
    Taint.lookup x indirect >>= fun ts ->
    Taint.detach x indirect ts >>= fun () ->
    (direct --> indirect) [y] x

  let init () = Machine.sequence Primus.[
      Interpreter.loaded  >>> loaded;
      Interpreter.stored  >>> stored;
      Interpreter.extract >>> extract;
      Interpreter.concat  >>> concat;
      Interpreter.cast    >>> cast;
    ]
end

let components : Primus.Machine.component list = [
  (module Exact);
  (module Computation)
]


let init () =
  List.iter ~f:Primus.Machine.add_component components
