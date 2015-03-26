open Core_kernel.Std
open Bap_types.Std
open Image_backend
open Image_common

let print_location l =
  let addr, len = l.Location.addr, l.Location.len in
  Printf.printf "\tlocation: %s (%d)\n" (Addr.to_string addr) len

let print_section s =
  Section.name s |> Printf.printf "\tname: %s\n";
  let string_of_perm p =
    let rec loop s = function
      | R -> s^"R"
      | W -> s^"W"
      | X -> s^"X"
      | Or (p1, p2) -> loop (loop s p1) p2 in
    loop "" p in
  Section.perm s |> string_of_perm |> Printf.printf "\tperm: %s\n";
  Section.off s |> Printf.printf "\toff: 0x%08x\n";
  Section.location s |> print_location

let print_sym s =
  Sym.name s |> Printf.printf "\tname: %s\n";
  Sym.is_function s |> Printf.printf "\tis_fuction: %B\n";
  Sym.is_debug s |> Printf.printf "\tis_debug: %B\n";
  let l, ll = Sym.locations s in
  print_location l;
  List.iter ~f:print_location ll

let print_tag t =
  Tag.name t |> Printf.printf "\tname: %s\n";
  Tag.data t |> Printf.printf "\tdata: %s\n";
  Tag.location t |> print_location

let print_image img =
  Img.arch img |> Arch.to_string |> Printf.printf "arch: %s\n";
  Img.entry img |> Addr.to_string |> Printf.printf "entry: %s\n";
  let s, sl = Img.sections img in
  print_endline "-----sections-------";
  print_section s;
  print_newline ();
  List.iter ~f:(fun e -> print_section e; print_newline()) sl;
  print_endline "--------------------\n";
  print_endline "------symbols-------";
  Img.symbols img |> List.iter ~f:(fun e -> print_sym e; print_newline());
  print_endline "--------------------\n";
  print_endline "--------tags--------";
  Img.tags img |> List.iter ~f:(fun e -> print_tag e; print_newline ());
  print_endline "--------------------\n"


let read_file path =
  Printf.printf "file: %s " path;
  let img = Llvm_loader.from_file path in
  match img with
  | Some i -> print_endline "OK"; print_image i
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
