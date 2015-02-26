open Core_kernel.Std
open OUnit2
open Or_error
open Word_size
open Format

open Bap.Std
open Image_common
open Image_backend
open Bap_table

let create_addr = function
  | W32 -> Addr.of_int ~width:32
  | W64 -> Addr.of_int ~width:64

let empty_table = Bap_table.empty

let create_mem ?(addr_size=W32) ?(start_addr=0x00) ?(size=0x08) () =
  let addr = create_addr W32 start_addr in
  let contents = Bigstring.init size ~f:(fun i -> char_of_int (i+0x61)) in
  let m = Bap_memory.create LittleEndian addr contents in
  match m with
  | Ok m -> m
  | Error err -> assert_failure (Error.to_string_hum err)

let table =
  let mem = create_mem () in
  let t = Bap_table.add empty_table mem "Tagged" in
  match t with
  | Ok t -> t
  | Error err -> assert_failure (Error.to_string_hum err)

let test_table_len ctxt =
  assert_equal 0 (Bap_table.length empty_table)

let test_table_len2 ctxt =
  assert_equal 1 (Bap_table.length table)

let test_find ctxt =
  let mem_to_find = create_mem () in
  Bap_table.find table mem_to_find |> function
  | Some x -> assert_equal "Tagged" x
  | None -> assert_failure "Not found"

let test_mem ctxt =
  let mem_to_find = create_mem () in
  assert_bool "Memory not found" (Bap_table.mem table mem_to_find)

let get_mem_tag = function
  | Some (_,x) -> x
  | None -> assert_failure "No memory contents"

let mem_low = create_mem ~start_addr:0x10 ~size:0x10 ()
let mem_high = create_mem ~start_addr:0x30 ~size:0x10 ()

(* Construct a new table tab *)
let tab =
  let t = Bap_table.add empty_table mem_low "lo" >>= fun t ->
    Bap_table.add t mem_high "hi" >>= fun t -> return t in
  match t with
  | Ok t -> t
  | Error err -> assert_failure (Error.to_string_hum err)

(* Tests if the mem range intersects with the mem expected regions *)
let test_intersections ctxt ~start_addr ~size ~expect =
  let mem_intersect = create_mem ~start_addr ~size () in
  let result = Bap_table.intersections tab mem_intersect in
  let regions = Seq.fold result ~init:[] ~f:(fun acc x -> (snd x) :: acc) in
  let printer l = Sexp.to_string_hum (sexp_of_list sexp_of_string l) in
  assert_equal ~ctxt ~printer expect regions

let str_printer expect_err = Sexp.to_string_hum (sexp_of_string expect_err)

let test_next ctxt =
  let result = Bap_table.next tab mem_low |> get_mem_tag in
  let msg = "Wrong next element" in
  assert_equal ~ctxt ~printer:str_printer ~msg "hi" result

let test_prev ctxt =
  let result = Bap_table.prev tab mem_high |> get_mem_tag in
  let msg = "Wrong prev element" in
  assert_equal ~ctxt ~printer:str_printer ~msg "lo" result

let test_min ctxt =
  let result = Bap_table.min tab |> get_mem_tag in
  let msg = "Wrong lowest memory binding" in
  assert_equal ~ctxt ~printer:str_printer ~msg "lo" result

let test_max ctxt =
  let result = Bap_table.max tab |> get_mem_tag in
  let msg = "Wrong highest memory binding" in
  assert_equal ~ctxt ~printer:str_printer ~msg "hi" result

let test_one_to_one ctxt =
  let tab2 =
    let extra = create_mem ~start_addr:0x50 ~size:0x30 () in
    let t = Bap_table.add empty_table mem_low "reg1" >>= fun t ->
      Bap_table.add t mem_high "reg2" >>= fun t ->
      Bap_table.add t extra "reg3" >>= fun t -> return t in
    match t with
    | Ok t -> t
    | Error err -> assert_failure (Error.to_string_hum err) in
  let link_table = link ~one_to:one String.hashable tab tab2 in
  assert_equal "reg2" (link_table "hi")

let suite = "Table" >::: [
    "table_length_zero" >:: test_table_len;
    "table_length_nonzero" >:: test_table_len2;
    "find" >:: test_find;
    "mem" >:: test_mem;
    "intersections1" >::
    test_intersections ~start_addr:0x10 ~size:0x30 ~expect:["hi";"lo"];
    "intersections2" >::
    test_intersections ~start_addr:0x1F ~size:0x30 ~expect:["hi";"lo"];
    "intersections3" >::
    test_intersections ~start_addr:0x20 ~size:0x10 ~expect:[];
    "intersections4" >::
    test_intersections ~start_addr:0x20 ~size:0x30 ~expect:["hi"];
    "intersections5" >::
    test_intersections ~start_addr:0x31 ~size:0x01 ~expect:["hi"];
    "intersections6" >::
    test_intersections ~start_addr:0x31 ~size:0x20 ~expect:["hi"];
    "intersections7" >::
    test_intersections ~start_addr:0x0 ~size:0x10 ~expect:[];
    "intersections8" >::
    test_intersections ~start_addr:0x0 ~size:0x11 ~expect:["lo"];
    "intersections9" >::
    test_intersections ~start_addr:0x0 ~size:0x21 ~expect:["lo"];
    "next" >:: test_next;
    "prev" >:: test_prev;
    "min" >:: test_min;
    "max" >:: test_max;
  ]
