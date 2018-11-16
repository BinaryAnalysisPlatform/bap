open Core_kernel
open OUnit2
open Format
open Bap.Std

let start = Addr.of_int32 0x100l
let addr offset = Addr.(start ++ offset)

let mem_equal x y =
  Memory.(Addr.(min_addr x = min_addr y && max_addr x = max_addr y))

let max_size = 1024

let base : mem =
  let data = Bigstring.create max_size in
  Memory.create LittleEndian start data |> ok_exn

let chunk offset size =
  Memory.view ~from:(addr offset) ~words:size base |> ok_exn

let byte offset = chunk offset 1
let ten_bytes offset = chunk offset 10
let hundred_bytes offset = chunk offset 100

let empty : 'a Memmap.t = Memmap.empty

let random_chunk () =
  let size = Random.int (max_size - 1) + 1 in
  let offset = if max_size = size then 0 else
      Random.int (max_size - size) in
  chunk offset size

let random_list size =
  List.init size ~f:(fun v -> random_chunk (), v)

let map_of_list =
  List.fold ~init:empty ~f:(fun map (k,v) -> Memmap.add map k v)

let assert_memory =
  assert_equal ~printer:Memory.to_string ~cmp:mem_equal
let assert_int = assert_equal ~printer:Int.to_string


let add_lookup chunk ctxt =
  let map = Memmap.add empty chunk 42 in
  let check x = match Memmap.lookup map x |> Seq.to_list with
    | [mem,v] ->
      assert_memory ~ctxt chunk mem;
      assert_equal ~ctxt  ~printer:Int.to_string 42 v;
    | [] -> assert_string "can't find just added chunk"
    | _ :: _ -> assert_string "found more that one chunk" in
  let rec loop x =
    if x <= Memory.max_addr chunk
    then (check x; loop Addr.(succ x)) in
  loop (Memory.min_addr chunk)


let min_max cons x y z ctxt =
  let c1,c2,c3 = cons x, cons y, cons z in
  let map = List.foldi [c1;c2;c3] ~init:empty ~f:(fun n map c ->
      Memmap.add map c n) in
  match Memmap.min_binding map, Memmap.max_binding map with
  | Some (x,p), Some (y,q) ->
    assert_memory ~ctxt c1 x;
    assert_memory ~ctxt c3 y;
    assert_int p 0;
    assert_int q 2;
  | Some _, None -> assert_string "Failed to find max binding"
  | None, Some _ -> assert_string "Failed to find min binding"
  | None,None -> assert_string "Failed to find any bindings"


let dominate cons size ctxt =
  let seq = cons size in
  let map = map_of_list seq in
  List.iter seq ~f:(fun (k,v) ->
      let msg = sprintf "(%d,[%a,%a]) is not dominated by [%a,%a]" v
          Addr.pps (Memory.min_addr k)
          Addr.pps (Memory.max_addr k)
          Addr.pps (uw (Memmap.min_addr map))
          Addr.pps (uw (Memmap.max_addr map)) in
      assert_bool msg @@ Memmap.dominates map k)

let contains k v =
  Seq.exists ~f:(fun (mem,w) -> mem_equal k mem && v = w)

let find_all cons size ctxt =
  let lst = cons size in
  let map = map_of_list lst in
  List.iter lst ~f:(fun (k,v) ->
      assert_bool "find_dominators" @@
      contains k v @@ Memmap.dominators map k;
      assert_bool "find_intersections" @@
      contains k v @@ Memmap.intersections map k)

let sort_by_value = List.sort ~compare:(fun (_,x) (_,y) -> Int.compare x y)

(* mix three equal values into the random data *)
let three_equal cons size ctxt =
  let c3 = ten_bytes 0x100 in
  let eqs = [c3,0; c3,1; c3,2] in
  let lst = List.permute (eqs @ cons size) in
  let map = map_of_list lst in
  let map = List.foldi ~init:map [c3;c3;c3] ~f:(fun i map key ->
      Memmap.add map key i) in
  let ins = Memmap.intersections map c3 in
  List.iter eqs ~f:(fun (k,v) ->
      assert_bool "find_dominators" @@
      contains k v @@ ins);
  let map = Memmap.remove_intersections map c3 in
  assert_bool "C3 must be removed" @@
  not(Memmap.intersects map c3)

let run_through cons size ctxt =
  let lst = cons size in
  let map = map_of_list lst in
  let sort_ints = List.sort ~compare:Int.compare in
  let collect addr =
    List.filter_map lst ~f:(fun (mem,x) ->
        Option.some_if (Memory.contains mem addr) x) |>
    sort_ints in
  let rec loop = function
    | 0 -> ()
    | n ->
      let a = addr n in
      let expect = collect a in
      let got = Memmap.lookup map a |> Seq.map ~f:snd
                |> Seq.to_list
                |> sort_ints in
      assert_equal ~ctxt expect got;
      loop (n-1) in
  loop max_size

let intersections cons size ctxt =
  let lst = cons size in
  let jam = cons size in
  let map = map_of_list jam in
  let sort_ints = List.sort ~compare:Int.compare in
  let map = List.fold (cons size) ~init:map ~f:(fun map (k,_) ->
      Memmap.remove_intersections map k) in
  let map = List.fold jam ~init:map ~f:(fun map (k,_) ->
      Memmap.remove_dominators map k) in
  let map = List.fold lst ~init:map ~f:(fun map (k,v) ->
      Memmap.add map k v) in
  let is_in mem mem' =
    Memory.(contains mem (min_addr mem') ||
            contains mem (max_addr mem')) in
  let collect mem' =
    List.filter_map lst ~f:(fun (mem,x) ->
        let contains = is_in mem mem' || is_in mem' mem in
        Option.some_if contains x) |>
    sort_ints in
  let printer v = Sexp.to_string @@ sexp_of_list sexp_of_int v in
  List.iter (cons size) ~f:(fun (mem,_) ->
      let expect = collect mem in
      let got = Memmap.intersections map mem |> Seq.map ~f:snd
                |> Seq.to_list
                |> sort_ints |> List.dedup_and_sort ~compare:Int.compare in
      assert_equal ~printer ~ctxt ~cmp:(List.equal ~equal:Int.equal) expect got)

let suite () = "Memmap" >::: [
    "add/lookup/1@0"    >:: add_lookup (byte 0);
    "add/lookup/1@1"    >:: add_lookup (byte 1);
    "add/lookup/1@1024" >:: add_lookup (byte 1023);
    "add/lookup/10@0"    >:: add_lookup (ten_bytes 0);
    "add/lookup/10@1"    >:: add_lookup (ten_bytes 1);
    "add/lookup/10@512"  >:: add_lookup (ten_bytes 512);
    "add/lookup/100@0"    >:: add_lookup (hundred_bytes 0);
    "add/lookup/100@1"    >:: add_lookup (hundred_bytes 1);
    "add/lookup/100@512"  >:: add_lookup (hundred_bytes 512);
    "min_max/1/1" >:: min_max byte 0 1 2;
    "min_max/1/2" >:: min_max byte 0 10 1023;
    "min_max/10/1" >:: min_max ten_bytes 0 1 2;
    "min_max/10/2" >:: min_max ten_bytes 0 512 768;
    "dominate/1"   >:: dominate random_list 1;
    "dominate/100"  >:: dominate random_list 100;
    "dominate/1024" >:: dominate random_list 1024;
    "dominate/2048" >:: dominate random_list 2048;
    "find_all/1"   >:: find_all random_list 1;
    "find_all/100"  >:: find_all random_list 100;
    "find_all/1024" >:: find_all random_list 1024;
    "find_all/2048" >:: find_all random_list 2048;
    "three_equal/1"   >:: three_equal random_list 1;
    "three_equal/100"  >:: three_equal random_list 100;
    "three_equal/1024" >:: three_equal random_list 1024;
    "three_equal/2048" >:: three_equal random_list 2048;
    "run_through/1"   >:: run_through random_list 1;
    "run_through/100"  >:: run_through random_list 100;
    "run_through/1024" >:: run_through random_list 1024;
    "run_through/2048" >:: run_through random_list 2048;
    "intersections/1"   >:: intersections random_list 1;
    "intersections/16"  >:: intersections random_list 16;
    "intersections/100"  >:: intersections random_list 100;
    "intersections/1024" >:: intersections random_list 1024;
    "intersections/2048" >:: intersections random_list 2048;
  ]
