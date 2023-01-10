let provides = ["pass"; "analysis"; "ssa"]
let doc = {|
# DESCRIPTION

Translates the whole program into the SSA form.

]|}

open Bap.Std

let main = Project.map_program ~f:(Term.map sub_t ~f:Sub.ssa)
let () = Bap_main.Extension.declare ~doc ~provides @@ fun _ ->
  Project.register_pass main;
  Ok ()
