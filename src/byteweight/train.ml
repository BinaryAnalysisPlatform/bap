open Core_kernel.Std
open Bap.Std
(* TODO:pass mode module *)
module Sigs = Byte.Table
let usage = "Train: ./train -bin-dir [test binary directory] -sig [output signature file]"
let d_bin = ref None
let sig_out = ref None
let k = 10

let arg_specs =
  ("-bin-dir", Arg.String(fun s -> d_bin := Some s), "train binary directory")
  :: ("-sig", Arg.String(fun s -> sig_out := Some s), "output signature file")
  :: []

let anon_fun _ = raise (Arg.Bad usage)

let parse_command =
  Arg.parse arg_specs anon_fun usage;
  match !d_bin, !sig_out with
  | Some d, Some out -> d, out
  | _ -> raise (Arg.Bad usage)

let fsi_of_section img sec : addr Seq.t =
  let syms = Image.symbols_of_section img sec in
  Sequence.filter_map syms ~f:(fun s ->
    if not (Image.Sym.is_function s) then None
    else
      let mem, _ = Image.memory_of_symbol img s in
      Some (Memory.min_addr mem))

let build_sigs imgs : 'a Sigs.t =
  let sigs = Sigs.create () in
  let add_pos key =
    match Sigs.find sigs key with
    | Some (p, n) ->
        Sigs.replace sigs ~key:key ~data:(p + 1, n)
    | None ->
        Sigs.add_exn sigs ~key:key ~data:(1, 0)
  in
  Array.iter imgs ~f:(fun img ->
    let process mem sec =
      let fsi = fsi_of_section img sec in
      Sequence.iter fsi ~f:(fun addr ->
        let keys = Byte.generate_keys addr mem in
        List.iter keys ~f:add_pos
      ) in
    let code_sections =
      Table.filter (Image.sections img) ~f:Image.Sec.is_executable in
    Table.iteri code_sections ~f:process
  );
  sigs

let update_sigs (sigs:'a Sigs.t) imgs =
  Array.iter imgs ~f:(fun img ->
    let code_sections =
      Table.filter (Image.sections img) ~f:Image.Sec.is_executable in
    Table.iteri code_sections ~f:(fun mem sec ->
      let fsi = fsi_of_section img sec in
      let sec_st = Memory.min_addr mem in
      let sec_nd = Memory.max_addr mem in
      let rec iterate addr =
        if addr >= sec_nd then ()
        else (
          if Sequence.mem fsi addr then iterate Addr.(addr ++ 1)
          else (
            let keys = Byte.generate_keys addr mem in
            let rec loop = function
              | [] -> ()
              | key :: tl ->
                match Sigs.find sigs key with
                | Some (p, n) ->
                    Sigs.replace sigs ~key:key ~data:(p, n + 1);
                    loop tl
                | None -> () in
            loop keys;
            iterate Addr.(addr ++ 1)))
      in
      iterate sec_st))

let train d =
  let bins = Array.map ~f:(Filename.concat d) (Sys.readdir d) in
  let imgs = Array.filter_map bins ~f:(fun bin ->
    match Image.create bin with
    | Ok (img, _) -> Some img
    | Error err ->
      eprintf "Program failed with: %s\n" @@ Error.to_string_hum err;
      None) in
  let sigs = build_sigs imgs in
  update_sigs sigs imgs;
  sigs

let output sigs file =
  let oc = Out_channel.create ~binary:true file in
  Sigs.iter sigs ~f:(fun ~key:k ~data:(p, n) ->
    Printf.fprintf oc "%s->%d,%d\n" (Byte.string_of_key k) p n);
  Out_channel.close oc

let () =
  Plugins.load ();
  let d, out = parse_command in
  let sigs = train d in
  output sigs out
