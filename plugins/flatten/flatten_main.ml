open Bap.Std
open Core_kernel

include Self()

let main = Project.map_program ~f:(Term.map sub_t ~f:Sub.flatten)

;;
Config.manpage [
  `S "DESCRIPTION";
  `P "Flatten all AST in the program.";
  `S "EXAMPLE";
  `Pre {|
  ;; input 
  #10 := 11 * (#9 + 13) - 17
  ;; output
  #11 := #9 + 13
  #12 := 11 * #11 
  #10 := #12 - 17
  |}

]

let () = Config.when_ready (fun _ -> Project.register_pass main);;
