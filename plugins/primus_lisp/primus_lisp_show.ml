let doc = "Shows the static semantics of Primus Lisp definitions.

# DESCRIPTION

Accepts a list of definition names and prints their static
reifications (semantics). To load a specific program (feature) use
the default mechanism ($(b,--primus-lisp-load)).


# EXAMPLE

Assuming that the following Lisp program

```
(defun foo ()
  (set R0 (/= 1 2 3 4)))
```

is stored in the file $(b,demo.lisp) that is located in the current
folder, use the following parameters to print the reification,


```
bap show foo --primus-lisp-load=demo -tarmv5+le
```

Notice, that the target specification is necessary when a definition
is not closed, i.e., when it references global variables (registers).

To print only specific slot of the value, use $(b,--slot) (short
version $(b,o)), e.g., the following will print only the BIL representation,

```
bap show foo --primus-lisp-load=demo -tarmv5+le -obap:bil
```

"

open Core_kernel
open Bap_core_theory
open Bap_main
open Bap.Std
open Extension.Syntax
open KB.Syntax

open Bap_primus.Std

type error = Conflict of KB.Conflict.t

type Extension.Error.t += Failed of error

let fail err = Error (Failed err)

module Spec = struct
  open Extension
  open Command

  let bitvec = Type.define Bitvec.zero
      ~parse:Bitvec.of_string
      ~print:Bitvec.to_string

  let unknown = KB.Name.create ":unknown"
  let name = Type.define unknown
      ~parse:KB.Name.read
      ~print:KB.Name.show

  let target = Type.define Theory.Target.unknown
      ~print:Theory.Target.to_string
      ~parse:(Theory.Target.get ~package:"bap")

  let names = Command.arguments name
  let target = Command.parameter target "target"
      ~aliases:["t"; "arch"]
  let slots = Command.parameters Type.(list string) "slots"
      ~aliases:["s"; "o"]
  let addr = Command.parameter Type.(some bitvec) "address"
      ~aliases:["a"]
  let t = args $ target $ names $ slots $ addr
end


let show target slots addr name =
  let pp = match List.concat slots with
    | [] -> KB.Value.pp
    | ss -> KB.Value.pp_slots ss in
  Toplevel.try_exec @@ begin
    Primus.Lisp.Unit.create target >>= fun unit ->
    KB.Object.scoped Theory.Program.cls @@ fun obj ->
    KB.sequence [
      KB.provide Theory.Label.unit obj (Some unit);
      KB.provide Theory.Label.addr obj addr;
      KB.provide Primus.Lisp.Semantics.name obj (Some name);
    ] >>= fun () ->
    KB.collect Theory.Semantics.slot obj >>| fun sema ->
    Format.eprintf "%a:@ %a@." KB.Name.pp name pp sema
  end

let () = Extension.Command.declare ~doc "show-lisp" Spec.t @@
  fun target names slots addr _ctxt ->
  List.map names ~f:(show target slots addr) |>
  Result.all_unit |>
  Result.map_error ~f:(fun err ->
      Failed (Conflict err))

let string_of_error = function
  | Conflict err ->
    Format.asprintf "Failed to reify the program: %a"
      KB.Conflict.pp err

let () = Extension.Error.register_printer @@ function
  | Failed err -> Some (string_of_error err)
  | _ -> None
