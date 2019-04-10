open Core_kernel
open Bap_knowledge

type t = Knowledge.state ref
let state = ref Knowledge.empty
let locked = ref false
let t = state
let set s = state := !s
let reset () = state := Knowledge.empty
let current () = !state

let is_locked () = locked.contents

let lock () =
  if is_locked ()
  then failwith "Unable to lock the BAP runtime";
  locked := true

let unlock () =
  locked := false

let run cls exp =
  lock ();
  protect ~finally:unlock ~f:(fun () ->
      match Knowledge.run cls exp !state with
      | Ok (value,state') ->
        state := state';
        Ok value
      | Error err -> Error err)


exception Internal_runtime_error of Knowledge.conflict



let run_or_fail cls exp =
  match run cls exp with
  | Ok r -> r
  | Error conflict ->
    raise (Internal_runtime_error conflict)
