open Core_kernel.Std
open OUnit2
open Bap_image
open Or_error


let file = "/bin/ls"

let limg,_ = ok_exn (create ~backend:"llvm_loader" file)

let bimg,_ = ok_exn (create ~backend:"bap-elf" file)

let print_sec (x,y) =
  print_endline "\nSections : ";
  List.iter2_exn x y ~f:(fun ex ey ->
    Printf.printf "\nname = %s : %s\n" (Sec.name ex) (Sec.name ey))
    
let print_sym (x,y) = 
  print_endline "\nSymbols : ";
  List.iter2_exn x y ~f:(fun ex ey ->
    Printf.printf "\nname = %s : %s\n" (Sym.name ex) (Sym.name ey))
  
let print_tag (x,y) =
  print_endline "\nTags : ";
  List.iter2_exn x y ~f:(fun ex ey ->
    let (m1,(ex1,ex2)) = ex in
    let (m2,(ey1,ey2)) = ey in
    Printf.printf "\nmem %s : %s : %s = mem %s : %s : %s\n"
      (Bap_memory.hexdump m1) ex1 ex2 (Bap_memory.hexdump m2) ey1 ey2)

let comp_ss x y = x = y

let comp_t x y = 
  let (m1,(x1,x2)) = x in
  let (m2,(y1,y2)) = y in 
  (Bap_memory.hexdump m1 = Bap_memory.hexdump m2) && (x1 = y1) && (x2 = y2)
    
let comp f s l b c p = 
  let list_el e = Sequence.to_list (f (s e)) in
  match list_el l , list_el b with
  | [],[] -> true
  | x, y  -> try 
               if (List.exists2_exn ~f:c x y) then
                 true
               else let () = p (x,y) in false
    with Invalid_argument "size mismatch" -> false

let suite = "Compare_image" >::: [
  "arch"   >::(fun test_ctxt -> assert_equal (arch limg) (arch bimg));
  "entry"  >::(fun test_ctxt -> assert_equal (entry_point limg) (entry_point bimg));
  "data"   >::(fun test_ctxt -> assert_equal (data limg) (data bimg));
  "endian" >::(fun test_ctxt -> assert_equal (endian limg) (endian bimg));
  "sections">::(fun test_ctxt -> assert_bool "Not equal sections"
    (comp Bap_table.elements sections limg bimg comp_ss print_sec));
  "symbols">::(fun test_ctxt -> assert_bool "Not equal symbols"
    (comp Bap_table.elements symbols limg bimg comp_ss print_sym));
  "tags">::(fun test_ctxt -> assert_bool "Not equal tags"
  (comp Bap_memmap.to_sequence tags limg bimg comp_t print_tag));
]

