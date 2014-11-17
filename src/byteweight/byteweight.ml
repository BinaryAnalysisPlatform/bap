open Core_kernel.Std
open Bap.Std
open Or_error
(* TODO: enable dism when bap has disassembler *)
let usage = "./byteweight [binary file]"

let g_wpt = Filename.concat (Filename.dirname Sys.executable_name) "signatures/sig_arm"
let dft_threshold = 0.5
let threshold = ref dft_threshold
let d_bin = ref None
let d_out = ref None
let bin = ref None
let out = ref stdout
let f_wpt = ref g_wpt
let work_on_bytes = ref false

let arg_specs =
  ("-wpt", Arg.String(fun s -> f_wpt := s), "weighted prefix tree file")
  :: ("-bin-dir", Arg.String(fun s -> d_bin := Some s), "test binary directory")
  :: ("-o-dir", Arg.String(fun s -> d_out := Some s; try Unix.mkdir s 0o755 with _ -> ()), "output directory")
  :: ("-o", Arg.String(fun s -> out := open_out s), "output file")
  :: ("-t", Arg.Float(fun f -> threshold := f), "threshold")
  :: ("-byte", Arg.Set work_on_bytes, "work on byte")
  (* Question: can BAP infer architecture from binaries? *)
  :: []

let anon_fun s = bin := Some s


let output oc fsi =
  Sequence.iter fsi ~f:(fun addr ->
    Printf.fprintf oc "%s\n" @@ Addr.to_string addr
  );
  Out_channel.close oc


let fsi_bin_fn bin =
  let r =
    Image.create bin >>| fun (img, _) -> begin
      let code_sections = Table.filter (Image.sections img) ~f:Image.Sec.is_executable in
      (* TODO: currently the architecture is hard-coded. Check if we add
       * architecture as an option or figure out the architecture of binary by
       * calling some detection method *)
      let fn_table = Fn_table.create ~signatures:!f_wpt ~work_on_bytes:!work_on_bytes Arch.ARM
        ~sections:code_sections in
      Fn_table.addrs fn_table
    end in
  match r with
    | Ok fsi -> fsi
    | Error err ->
        eprintf "Program failed with: %s\n" @@
        Error.to_string_hum err; Sequence.empty

(* main *)
let () =
  Plugins.load ();
  let () = Arg.parse arg_specs anon_fun usage in
  match !bin, !d_bin with
    (* TODO: currently for every binary signature file has to be read again
     * because Fn_table.create interfaces (which targets at segments and one connot decide the
     * architecture before calling create) *)
    | None, Some d_i -> (
      match !d_out with
      | None ->
        let err =
          Printf.sprintf "Output directory is required.\n" ^ usage in
        raise (Arg.Bad err)
      | Some d_o ->
        let bins = List.map
          ~f:(Filename.concat d_i)
          (Array.to_list (Sys.readdir d_i)) in
        List.iter ~f:(fun bin ->
          Printf.printf "%s\n%!" bin;
          let fs_seq = fsi_bin_fn bin in
          let oc =
            let bin_out = Filename.concat d_o (Filename.basename bin) in
            open_out bin_out in
          output oc fs_seq
        ) bins
    )
    | Some i, None ->
      let fs_seq = fsi_bin_fn i in
      output !out fs_seq
    | _ -> raise (Arg.Bad usage)
