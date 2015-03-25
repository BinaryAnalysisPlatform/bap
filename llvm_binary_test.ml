open Core_kernel.Std
open Bap_types.Std


let read_file path =
  Printf.printf "file: %s " path;
  let img = Llvm_loader.from_file path in
  match img with
  | Some i -> print_endline "OK"
  | None -> print_endline "FAIL"

  (* let b = Bap_fileutils.readfile path |> *)
  (*         Llvm_binary.create in *)
  (* Llvm_binary.arch b |> *)
  (* Arch.to_string |> *)
  (* Printf.printf "arch: %s\n"; *)
  (* Llvm_binary.entry b |> *)
  (* Printf.printf "entry: %Lx\n"; *)
  (* print_endline "-------------------------------" *)

let () =
  let usage () =
    Printf.eprintf
      "usage: %s  path...
       path - path to binary file\n"
      (Filename.basename Sys.argv.(0)) in
  match Array.to_list Sys.argv with
  | [] -> usage ()
  | path::[] -> usage ()
  | path::key::_ when key = "-h" || key = "--help" -> usage ()
  | path::files -> List.iter ~f:read_file files
