(* concatenate files with a header  *)
let read_all file =
  let chan = open_in_bin file in
  let data = really_input_string chan (in_channel_length chan) in
  close_in chan;
  data

let subst file str =
  let buf = Buffer.create 16 in
  Buffer.add_substitute buf (function "name" -> file | s -> s) str;
  Buffer.contents buf

let cat header inputs output =
  let data = List.map (fun file ->
      subst file header,read_all file) inputs in
  let ch = open_out output in
  data |> List.iter (fun (header,data) ->
      output_string ch header;
      output_string ch data);
  close_out ch


let () =
  match Array.to_list Sys.argv with
  | _ :: sep :: "--" :: (_ :: _ as ios)  ->
    let soi = List.rev ios in
    let inputs = List.(soi |> tl |> rev) in
    let output = List.hd soi in
    let sep = try Scanf.sscanf sep "%S" (fun x -> x) with exn ->
      Printf.eprintf "Failed to unescape: %s\n" sep;
      sep in
    cat sep inputs output
  | _ ->
    prerr_endline "Usage: cat header -- input1 [.. inputN] output"
