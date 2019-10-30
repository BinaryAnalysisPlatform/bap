
let () =
  let args = Array.of_list @@ match Array.to_list Sys.argv with
    | [] | [_] -> ["bap"; "mc"]
    | _ :: args -> "bap" :: "mc" :: args in
  Unix.execvp "bap" args
