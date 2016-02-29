(* this is a slowmo implementation of cat utility,
   that we need since `cat` is not guaranteed to exist
   on all platforms*)

let read_all file =
  let chan = open_in_bin file in
  let data = really_input_string chan (in_channel_length chan) in
  close_in chan;
  data

let cat inputs output =
  let data = List.map read_all inputs in
  let ch = open_out output in
  data |> List.iter (output_string ch);
  close_out ch

let () =
  match Array.to_list Sys.argv with
  | [] | [_] | [_; _] ->
    prerr_endline "Usage: cat input1 [.. inputN] output"
  | _ :: ios ->
    let soi = List.rev ios in
    let inputs = List.(soi |> tl |> rev) in
    let output = List.hd soi in
    cat inputs output
