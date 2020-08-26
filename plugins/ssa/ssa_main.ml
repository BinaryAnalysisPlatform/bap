open Bap.Std
include Self()

let main = Project.map_program ~f:(Term.map sub_t ~f:Sub.ssa)

;;
Config.manpage [
  `S "SYNOPSIS";
  `Pre "
    $(b,mname)
";
  `S "DESCRIPTION";
  `P "Translates the whole program into the SSA form";
]

let () = Config.when_ready (fun _ -> Project.register_pass main);;
