open Core_kernel.Std
open Bap.Std
open Cmdliner
include Self()

let man = [
  `S "DESCRIPTION";
  `P "Iterates through memory tagged with text objects that have 
      a `python' tag, and dumps them into a python script, that 
      can be later loaded into the IDA. "
]

let info = Term.info name ~version ~doc ~man

let dst : string option Term.t =
  let doc = "Dump annotations to the specified file $(docv). If 
            not specified, then the script will dumped into the
            standard output" in
  Arg.(value & opt (some string) None & info ["file"]
         ~doc ~docv:"NAME")

let extract_script map =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "from idautils import *\nWait()\n";
  Memmap.iter map ~f:(fun x ->
      match Value.get python x with
      | Some line -> Buffer.add_string buf line
      | None -> ());
  Buffer.contents buf

let main dst project = 
  let data = extract_script (Project.memory project) in
  match dst with
  | None -> print_string data
  | Some dst -> Out_channel.write_all dst ~data

let () = 
  let main = Term.(const main $dst) in
  match Term.eval ~argv (main,info) with
  | `Ok main -> Project.register_pass' main
  | `Help | `Version -> exit 0
  | `Error _ -> exit 1
