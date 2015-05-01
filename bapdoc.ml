(** this tool is to extract documentation from bap.mli  *)


let target = "lib/bap/bap.mli"
let outdir = "doc"
let deps = [
  "core_kernel";
  "ocamlgraph";
]

exception No_out_dir

let find_word ?(start=0) w s =
  let m,n = String.(length w, length s) in
  let rec find i j =
    if i >= m then Some (j - m) else
    if j >= n then None else
    if w.[i] = s.[j]
    then find (i+1) (j+1)
    else find 0 (j+1) in
  find 0 start

let find_comments s = match find_word "(*" s with
  | None -> None, find_word "*)" s
  | Some i -> Some i, find_word ~start:i "*)" s

(* according to OCaml grammar [with] in mli file can occur only in
   module constraint in the form [with module ...] or [with type] *)
let find_p4_with s =
  match find_word "with" s with
  | None -> None
  | Some i ->
    let has_type = find_word ~start:i "type" s <> None in
    let has_module = find_word ~start:i "module" s <> None in
    let is_word =
      let starts = i = 0 || s.[i - 1] = ' ' in
      let ends = i + 4 >= String.length s || s.[i+4] = ' ' in
      starts && ends in
    if has_type || has_module || not is_word
    then None else Some i


(* right now we will just with, but later we can transform
   them to "@with ..." comment, that can be later processed
   to output corresponding documentation. *)
let preprocess_with oc s = match find_p4_with s with
  | None -> output_string oc s
  | Some i -> match find_comments s with
    | None, None -> output_substring oc s 0 i
    | Some j,_ when j > i ->
      output_substring oc s 0 i;
      output_substring oc s j (String.length s - j)
    | _, Some j -> if j < i then output_substring oc s 0 i
    | _ -> output_string oc s


let preprocess ic oc  =
  let next () = try Some (input_line ic) with End_of_file -> None in
  let rec loop in_comment = match next () with
    | None -> ()
    | Some s ->
      let go s in_comment =
        output_string oc s;
        output_char oc '\n';
        loop in_comment in
      if in_comment
      then match find_comments s with
        | _,None -> go s in_comment
        | _,Some _ -> go s false
      else begin
        preprocess_with oc s;
        match find_comments s with
        | None,None -> go "" false
        | Some x, Some y -> go "" false
        | None, Some _ -> go "" false
        | Some _, None -> go "" true
      end in
  loop false;
  close_in ic;
  close_out oc

let run cmd =
  let res = Sys.command cmd in
  if res <> 0 then
    failwith ("Command: '" ^ cmd ^ "' failed")

let compile ~options name =
  let b = Buffer.create 64 in
  Buffer.add_string b ("ocamlfind ocamldoc " ^ options ^ " " ^ name);
  List.iter (fun dep ->
      Buffer.add_string b " -package ";
      Buffer.add_string b dep) deps;
  run (Buffer.contents b)

let generate () =
  let tmp = Filename.get_temp_dir_name () in
  let ic = open_in target in
  let on = Filename.concat tmp "bap.mli" in
  let oc = open_out on in
  preprocess ic oc;
  let cwd = Sys.getcwd () in
  let outdir = Filename.concat cwd outdir in
  if not (Sys.file_exists outdir) ||
     not (Sys.is_directory outdir) then
    raise No_out_dir;
  Sys.chdir tmp;
  compile ~options:("-d " ^ outdir ^ " -html") on


let () =
  try generate () with
  | No_out_dir ->
    prerr_string ("Please, create the `" ^ outdir ^ "' folder");
