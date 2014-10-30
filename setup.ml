(* setup.ml generated for the first time by OASIS v0.4.4 *)

(* OASIS_START *)
(* OASIS_STOP *)

let def_piqic (() : unit) : unit =
  let chop str = try
      Filename.chop_extension str
    with _ -> str in
  let piqic_path = BaseCheck.prog_best "piqic_path" ["piqic-ocaml"; "piqic"] () in
  BaseEnv.var_define "piqic_flags" (fun () ->
      if chop (Filename.basename (BaseEnv.var_get "piqic"))
         = "piqic"
      then "ocaml"
      else "") |> ignore;
  BaseEnv.var_define "piqic" (fun () ->
      if (BaseStandardVar.os_type () = "Cygwin") || (BaseStandardVar.os_type () = "Win32")
      then String.concat " "
          (OASISExec.run_read_output ~ctxt:!BaseContext.default "cygpath" [piqic_path])
      else piqic_path) |> ignore

let () =
  def_piqic ();
  setup ();;
