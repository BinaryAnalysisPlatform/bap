open Bap.Std
include Self()

let main proj =
  Project.with_program proj @@
  Term.map sub_t (Project.program proj) ~f:Sub.ssa

let () = Project.register_pass main;;

Config.manpage [
  `S "SYNOPSIS";
  `Pre "
    $(b,$mname)
";
  `S "DESCRIPTION";
  `P "Translates the whole program into the SSA form";
]
