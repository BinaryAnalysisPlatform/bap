let setup = "setup.data"
let config = "lib/bap_types/bap_config.ml"


let program () =
  let src = open_in setup in
  let dst = open_out config in
  let rec loop () =
    let data = input_line src in
    output_string dst "let ";
    output_string dst data;
    output_string dst "\n";
    loop () in
  try loop () with End_of_file -> ()

let () = program ()
