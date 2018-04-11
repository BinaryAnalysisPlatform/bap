open Core
open Core_bench.Std
open Or_error
open Bap.Std

let n = 100_000

let data = Bigstring.init n ~f:(fun _ -> '\x00')
let null = Addr.of_int ~width:32 0
let zero = Word.of_int ~width:8 0
let mem = ok_exn (Memory.create BigEndian null data)
let tab =
  Memory.With_error.foldi mem ~init:Table.empty ~f:(fun addr word tab ->
      Memory.view ~from:addr ~words:1 mem >>= fun mem ->
      Table.add tab mem word)
let tab = match tab with
  | Ok tab -> tab
  | Error err ->
    eprintf "Failing: %s" (Error.to_string_hum err);
    invalid_arg "invalid memory"

let sum_while () =
  let sum = ref zero in
  for i = 0 to n - 1 do
    sum := Word.Int_exn.(!sum + Word.of_int ~width:8 (Char.to_int data.{i}));
  done

let sum_foldi () =
  let (_ : word) =
    Memory.foldi mem ~f:(fun _ w1 w2 -> Word.Int_exn.(w1 + w2)) ~init:zero
  in ()

let sum_fold () =
  let (_ : word) =
    Memory.fold mem ~f:(fun w1 w2 -> Word.Int_exn.(w1 + w2)) ~init:zero
  in ()

let sum_foldm () =
  let (_ : word Or_error.t) =
    Memory.With_error.fold mem ~f:(fun w1 w2 -> Word.Int_err.(!$w1 + !$w2)) ~init:zero
  in ()

let sum_fold_tab () =
  let (_ : word) =
    Table.fold tab ~f:(fun w1 w2 -> Word.Int_exn.(w1 + w2)) ~init:zero
  in ()

let test = Bench.Test.create_group ~name:"image" [
    Bench.Test.create ~name:"Bap_image.map_sum_direct" sum_while;
    Bench.Test.create ~name:"Bap_image.map_sum_fold"   sum_fold;
    Bench.Test.create ~name:"Bap_image.map_sum_foldi"  sum_foldi;
    Bench.Test.create ~name:"Bap_image.map_sum_foldm"  sum_foldm;
    Bench.Test.create ~name:"Bap_image.map_sum_table"  sum_fold_tab;
  ]

let tests = [test]
