(* This test compare llvm and native loader image

   By default it compare images created from /bin/ls binary in unix os_type.
   It can be used with others files, for example:

   ./run_tests.native -only-test name -loader-binary path

   where
   name = BAP:9:Image:0:llvm_loader
   path - path to binary e.g. gcc_coreutils_64_O0_cat

   To make test over files in binary repository it can be used like this:

   find ../x86_64-binaries/elf/ -type f -name "gcc_*O0*" -exec \
   ./run_tests.native -only-test BAP:9:Image:0:llvm_loader -loader-binary '{}' \;
*)
open Core_kernel
open OUnit2
open Or_error
open Bap_types.Std
open Image_backend

(* Accoding to llvm (ObjectFile.h line 186)
   symbols type can be debug or function.
   Thats why we don't compare is_debug field *)
module SS = Set.Make(
  struct
    include Symbol
    let compare t1 t2 =
      let open Symbol in
      let t2' = {t2 with is_debug = t1.is_debug} in
      compare t1 t2'
  end)

let get_filename =
  let default = match Sys.os_type with
    | "Unix" -> "/bin/ls"
    | "Win32" | "Cygwin" -> ""
    | _ -> "" in
  Conf.make_string "loader_binary" default
    "native and llvm loaders compare test file path"


let run_test ctxt =
  let file = get_filename ctxt in
  if not (Sys.file_exists file) then skip_if true "file not found";
  let img1, img2 =
    let img1_opt, img2_opt as images =
      Bap_fileutils.readfile file |> Bap_native_loader.of_data,
      Bap_fileutils.readfile file |> Bap_llvm_loader.of_data in
    let () = match images with
      | _ , None -> assert_string "llvm loader failed"
      | None, _ -> skip_if true "elf loader failed"
      | _ -> () in
    Option.value_exn img1_opt, Option.value_exn img2_opt in

  assert_equal ~ctxt ~printer:Arch.to_string
    ~msg:"loaders gives different arch"
    (Img.arch img1)
    (Img.arch img2);

  assert_equal ~ctxt ~printer:Addr.to_string
    ~msg:"loaders gives different entry point"
    (Img.entry img1)
    (Img.entry img2);

  assert_equal ~ctxt ~printer:(fun e ->
      List.sexp_of_t Segment.sexp_of_t e |> Sexp.to_string)
    ~msg:"loaders gives different segments"
    (Img.segments img1 |> (fun (e, ee) -> e::ee))
    (Img.segments img2 |> (fun (e, ee) -> e::ee));

  assert_equal ~ctxt ~printer:(fun e ->
      List.sexp_of_t Section.sexp_of_t e |> Sexp.to_string)
    ~msg:"loaders gives different sections"
    (Img.sections img1)
    (Img.sections img2);

  let sym1, sym2 =
    let symbols img =
      Img.symbols img |>
      SS.of_list in
    symbols img1, symbols img2 in

  if not (SS.subset sym1 sym2) then
    let string_of_set s =
      SS.fold ~init:[]
        ~f:(fun acc s -> (Symbol.sexp_of_t s |> Sexp.to_string)::acc) s |>
      String.concat ~sep:"\n" in
    let diff12 =
      SS.diff sym1 sym2 |>
      string_of_set in
    let diff21 =
      SS.diff sym2 sym1 |>
      string_of_set in

    String.concat ~sep:"\n"
      [file;
       "llvm loader losted symbols:"; diff12;
       "llvm loader surplused symbols:"; diff21] |>
    assert_string

let suite = "Image" >::: [
    "llvm_loader"   >::run_test;
  ]
