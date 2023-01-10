let provides = [
  "pass";
  "analysis";
  "flatten";
  "tac"; "3ac";
  "unroll"
]

let doc = {|
# DESCRIPTION

Flatten all BIR in the program.

# EXAMPLE

```
  ;; input
  #10 := 11 * (#9 + 13) - 17
  ;; output
  #11 := #9 + 13
  #12 := 11 * #11
  #10 := #12 - 17
```
|}


open Bap.Std

let main = Project.map_program ~f:(Term.map sub_t ~f:Sub.flatten)

let () = Bap_main.Extension.declare ~doc ~provides @@ fun _ ->
  Project.register_pass main;
  Ok ()
