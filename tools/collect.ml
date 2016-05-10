let () = match Array.to_list Sys.argv with
  | _ :: ext :: features -> features |> List.iter (fun feature ->
      let file = feature ^ "." ^ ext ^ ".in" in
      if Sys.file_exists file then print_endline file)
  | _ -> failwith "Usage: collect extension features..."
