open Printf

let (/) = Filename.concat

(* TODO get it from bap_config *)
let bindir = Bap_config.bindir
let mandir = Bap_config.mandir

let tools = [
  "bap-mc";
  "bap";
  "bap-byteweight";
]

(** handwritten man pages *)
let manpages = [
  "bapbundle.1"
] |> List.map (fun page -> "man"/page)

let man_of_help prog =
  let man = "man" / sprintf "%s.1" prog in
  let cmd = sprintf "%s --help=groff > %s 2>/dev/null" prog man in
  ignore (Sys.command cmd);
  man

let bin_exists prog =
  try ignore (FileUtil.which prog); true with Not_found -> false

let create_man prog =
  if bin_exists prog then [man_of_help prog] else []


let main () =
  let utils = ["baptop"] in
  let helps = List.map create_man tools |> List.concat in
  FileUtil.mkdir ~parent:true mandir;
  FileUtil.cp (manpages @ helps) mandir;
  FileUtil.cp (List.map (fun t -> "tools" / t) utils) bindir;
  List.iter (fun name -> Unix.chmod (bindir / name) 0o700) utils

let () = main ()
