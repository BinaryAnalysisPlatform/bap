
open OUnit2
open Core_kernel

open Bap_future.Std

let magic_value = 42
let values = [0;1;2;3;4;5;6;7]

let assert_false fail_id v = assert_bool fail_id (not v)

let make_iota () = 
  let start = ref 0 in
  fun () -> 
    let v = !start in
    start := Int.succ !start;
    v

let is_even x = x mod 2 = 0 
let even_opt x = if is_even x then Some x else None 

let check_stream ctxt expect ss ~repeat signal =
  let f,p = Future.create () in
  let f' = Stream.before f ss in
  Signal.repeat signal ~times:repeat ();
  Promise.fulfill p ();
  assert_equal ~ctxt expect (Future.peek_exn f')

let send_signal ctxt =
  let buf = Queue.create () in
  let ss, signal = Stream.create () in
  Stream.observe ss (Queue.enqueue buf);
  Signal.send signal 0;
  Signal.send signal 1;
  Signal.send signal 2;
  let expect = [0;1;2;] in
  assert_equal ~ctxt expect (Queue.to_list buf)

let repeat_signal ~times ctxt = 
  let buf = Queue.create () in
  let ss, signal = Stream.create () in
  Stream.observe ss (Queue.enqueue buf);
  Signal.repeat signal ~times magic_value;
  let expect = Array.(to_list (create ~len:times magic_value)) in
  assert_equal ~ctxt expect (Queue.to_list buf)

let create ctxt = 
  let ss, signal = Stream.create () in
  let value = ref None in
  Stream.observe ss (fun x -> value := Some x);
  Signal.send signal magic_value;
  assert_equal ~ctxt magic_value (Option.value_exn !value)

let from ctxt =
  let ss, signal = Stream.from (make_iota ()) in
  let expect = [0;1;2;3;] in
  check_stream ctxt expect ss ~repeat:4 signal

let unfold ctxt =
  let f prev = prev, Int.succ prev in
  let ss, signal = Stream.unfold ~init:Int.zero ~f in
  let expect = [0;1;2;3;] in
  check_stream ctxt expect ss ~repeat:4 signal

let unfold_until ctxt = 
  let f x = 
    if x > 7 then None 
    else Some (x,x + 1)  in
  let expect = [0;1;2;3;4;5;6;7;] in
  let init = Int.zero in
  let ss, signal, future = Stream.unfold_until ~init ~f in
  check_stream ctxt expect ss ~repeat:(List.length expect + 1) signal;
  assert_bool "unfold_until" (Future.is_decided future) 

let unfold' ctxt =
  let f prev = Queue.of_list [prev; prev], Int.succ prev in
  let ss, signal = Stream.unfold' ~init:Int.zero ~f in
  let expect = [0;0;1;1;2;2;] in
  check_stream ctxt expect ss ~repeat:3 signal 

let repeat value ~times ctxt = 
  let ss, signal = Stream.repeat value in
  let expect = Array.(to_list (create ~len:times value)) in
  check_stream ctxt expect ss ~repeat:times signal 

let of_container ctxt fail_id container ~expect make_stream =
  let ss, signal, f = make_stream container in
  assert_false fail_id (Future.is_decided f);
  check_stream ctxt expect ss ~repeat:(List.length expect + 1) signal;
  assert_bool fail_id (Future.is_decided f)

let of_list ctxt = 
  of_container ctxt "of list" values ~expect:values Stream.of_list

let of_array ctxt = 
  let c = Array.of_list values in
  of_container ctxt "of array" c ~expect:values Stream.of_array

let of_sequence ctxt =
  let c = Sequence.of_list values in
  of_container ctxt "of sequence" c ~expect:values Stream.of_sequence

let watch ctxt =
  let ss, signal, _ = Stream.of_list values in
  let buf = Queue.create () in
  let once id x =   
    if x > 3 then Stream.unsubscribe ss id
    else Queue.enqueue buf x in
  Stream.watch ss once;
  Signal.repeat signal ~times:(List.length values) ();
  assert_equal ~ctxt (List.take values 4) (Queue.to_list buf)

let observe ctxt = 
  let ss, signal, _ = Stream.of_list values in
  let called = ref Int.zero in
  let f x = called := Int.succ !called in
  Signal.repeat signal ~times:2 ();
  assert_equal ~ctxt Int.zero !called;
  Stream.observe ss f;
  Signal.repeat signal ~times:2 ();
  assert_equal ~ctxt 2 !called
  
let subscribe ctxt = 
  let ss, signal, _ = Stream.of_list values in
  let called = ref Int.zero in
  let f x = called := Int.succ !called in
  Signal.repeat signal ~times:2 ();
  assert_equal ~ctxt Int.zero !called;
  let id = Stream.subscribe ss f in
  Signal.repeat signal ~times:2 ();
  assert_equal ~ctxt 2 !called;
  Stream.unsubscribe ss id;
  Signal.repeat signal ~times:2 ();
  assert_equal ~ctxt 2 !called

let unsubscribe ctxt = 
  let ss, _, _ = Stream.of_list values in
  let f x = () in
  assert_bool "no subsribers" (not (Stream.has_subscribers ss));
  let id = Stream.subscribe ss f in
  assert_bool "exists subsribers" (Stream.has_subscribers ss);
  Stream.unsubscribe ss id;
  assert_bool "no subsribers" (not (Stream.has_subscribers ss))

let on_wait ctxt = 
  let ss, _, _ = Stream.of_list values in
  let called = ref false in
  let f () = called := true in
  Stream.on_wait ss f;
  Stream.wait ss;
  assert_bool "on_wait" !called

let on_subscribe ctxt = 
  let ss, _, _ = Stream.of_list values in
  let id = ref None in
  let f id' = id := Some id' in
  let f' _ = () in
  Stream.on_subscribe ss f;
  let id' = Stream.subscribe ss f' in
  assert_equal ~ctxt id' (Option.value_exn !id)

let on_unsubscribe ctxt = 
  let ss, _, _ = Stream.of_list values in
  let id = ref None in
  let f id' = id := Some id' in
  let f' _ = () in
  Stream.on_unsubscribe ss f;
  let id' = Stream.subscribe ss f' in
  Stream.unsubscribe ss id';
  assert_equal ~ctxt id' (Option.value_exn !id)

let has_subscribers ctxt =
  let assert_no_subscribers ss = 
    assert_false "no subscribers" (Stream.has_subscribers ss) in
  let assert_exists_subscribers ss =
    assert_bool "exists subscribers" (Stream.has_subscribers ss) in
  let ss, _, _ = Stream.of_list values in  
  let f x = () in
  assert_no_subscribers ss; 
  let id = Stream.subscribe ss f in
  assert_exists_subscribers ss; 
  let id' = Stream.subscribe ss f in
  Stream.unsubscribe ss id;
  assert_exists_subscribers ss; 
  Stream.unsubscribe ss id';
  assert_no_subscribers ss

let different_subscribe_id ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let stub _ = () in
  let stub' _ = () in
  let id = Stream.subscribe ss stub in
  let id' = Stream.subscribe ss stub' in
  assert_false "check id" (id = id')

let different_watch_id ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let w_id = ref None in
  let w_id' = ref None in
  let f id _ = w_id := Some id in
  let f' id _ = w_id' := Some id in
  Stream.watch ss f;
  Stream.watch ss f';
  Signal.send signal ();
  assert_false "check watch id" 
    ((Option.value_exn !w_id) = (Option.value_exn !w_id'))

let same_watch_id ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let w_id = ref None in
  let f id _ = w_id := Some id in
  Stream.watch ss f;  
  Signal.send signal ();
  let id = Option.value_exn !w_id in
  Signal.send signal ();
  let id' = Option.value_exn !w_id in
  assert_equal ~ctxt id id'

let map' ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let f x = Queue.of_list [x, x;] in
  let ss' = Stream.map' ss ~f in
  let expected = List.map values (fun x -> x,x) in
  check_stream ctxt expected ss' ~repeat:(List.length expected) signal

let map ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let ss' = Stream.map ss ~f:Int.succ in
  let expected = List.map values ~f:Int.succ in
  check_stream ctxt expected ss' ~repeat:(List.length expected) signal

let filter_map ~values ~expect ~f ctxt =
  let ss,signal,_ = Stream.of_list values in
  let ss' = Stream.filter_map ss ~f:even_opt in
  check_stream ctxt expect ss' ~repeat:(List.length values) signal

let merge ~values ~values' ~expect ~f ctxt = 
  let ss,signal,_ = Stream.of_list values in
  let ss',signal',_ = Stream.of_list values' in
  let rs = Stream.merge ss ss' ~f in 
  let repeat = max (List.length values) (List.length values') in
  let buf = Queue.create () in
  Stream.observe rs (Queue.enqueue buf);
  Signal.repeat signal ~times:repeat ();
  Signal.repeat signal' ~times:repeat ();
  assert_equal ~ctxt expect (Queue.to_list buf)

let apply ctxt =
  let expected = List.mapi ~f:(fun i x -> x + i) values in  
  let ss,signal,_ = 
    Array.init (List.length expected) ~f:(fun i -> fun x -> x + i) |>
    Stream.of_array in
  let ss',signal',_ = Stream.of_list values in
  let rs = Stream.apply ss ss' in
  let buf = Queue.create () in
  Stream.observe rs (Queue.enqueue buf);
  Signal.repeat signal  ~times:(List.length values) ();
  Signal.repeat signal' ~times:(List.length values) ();
  assert_equal ~ctxt expected (Queue.to_list buf)

let split ctxt = 
  let f x = x, Int.succ x in
  let ss,signal,_= Stream.of_list values in
  let rs, rs' = Stream.split ss ~f in
  let expected = values in
  let expected' = List.map ~f:Int.succ values in
  let buf = Queue.create () in
  let buf' = Queue.create () in
  Stream.observe rs (Queue.enqueue buf);
  Stream.observe rs' (Queue.enqueue buf');
  Signal.repeat signal ~times:(List.length values) ();
  assert_equal ~ctxt expected (Queue.to_list buf);
  assert_equal ~ctxt expected' (Queue.to_list buf')

let zip ~values ~values' ~expect ctxt =
  let ss,signal,_ = Stream.of_list values in
  let ss',signal',_ = Stream.of_list values' in
  let times = max (List.length values) (List.length values') in
  let rs = Stream.zip ss ss' in
  let buf = Queue.create () in
  Stream.observe rs (Queue.enqueue buf);
  Signal.repeat signal ~times ();
  Signal.repeat signal' ~times ();
  assert_equal ~ctxt expect (Queue.to_list buf)

let unzip ctxt =
  let expected = values in
  let expected' = List.map ~f:string_of_int values in
  let ss, signal, _ = Stream.of_list values in 
  let ss' = Stream.map ~f:(fun x -> x, string_of_int x) ss in
  let rs, rs' = Stream.unzip ss' in
  let buf = Queue.create () in
  let buf' = Queue.create () in
  Stream.observe rs (Queue.enqueue buf);
  Stream.observe rs' (Queue.enqueue buf');
  Signal.repeat signal ~times:(List.length values) ();
  assert_equal ~ctxt expected (Queue.to_list buf);
  assert_equal ~ctxt expected' (Queue.to_list buf')

let once ctxt = 
  let ss, signal, _ = Stream.of_list values in
  let ss' = Stream.once ss in
  let expected = [0;] in
  check_stream ctxt expected ss' ~repeat:8 signal

let parse ctxt = 
  let ss, signal, _ = Stream.of_list values in
  let expected = List.filter_map ~f:even_opt values in
  let f state x =
    if is_even x then Some x, x + 1
    else None, x + 1 in
  let ss' = Stream.parse ss ~init:0 ~f in
  check_stream ctxt expected ss' ~repeat:(List.length values) signal

let foldw ~stride ~f ~width ~init ~values ~expect ~times ctxt =
  let ss,signal,_ = Stream.of_list values in
  let ss' = Stream.foldw ~stride ss width ~init ~f in
  check_stream ctxt expect ss' ~repeat:times signal 

let foldw_range ~max_stride ~max_width ~f ~init ctxt = 
  let run (s,w) = 
    let ss,signal,_ = Stream.of_list values in
    let ss' = Stream.foldw ~stride:s ss w ~init ~f in
    let f,p = Future.create () in
    let f' = Stream.before f ss' in
    Signal.repeat signal ~times:(List.length values + 1) ();
    Promise.fulfill p ();
    ignore(Future.peek_exn f') in
  let create_set n = Array.init (n + 1) ident |> Array.to_list in
  let stride_set = create_set max_stride in
  let width_set = create_set max_width in
  let all = List.cartesian_product stride_set width_set in
  let r = 
    try
      List.iter ~f:run all;
      true
    with _ -> false in
  assert_bool "foldw range test" r

let run_time_line signal clk_signal time_line =
  let rec run ind = function
    | [] -> ()
    | (time_ind :: times) as line ->     
      let ind', line' = 
        if time_ind = ind then
          let () = Signal.send clk_signal () in
          ind, times
        else
          let () = Signal.send signal () in
          Int.succ ind, line in
      run ind' line' in
  run 0 time_line

let frame values ~expect ~time_line ctxt =
  let ss,signal, _ = Stream.of_list values in
  let clk,signal' = Stream.from (fun () -> ()) in
  let ss' = Stream.frame ~clk ss ~init:[] ~f:(fun a x -> a @ [x]) in
  let buf = Queue.create () in
  Stream.observe ss' (Queue.enqueue buf);
  run_time_line signal signal' time_line;
  assert_equal ~ctxt expect (Queue.to_list buf)

let sample values ~expect ~time_line ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let clk,signal' = Stream.from (fun () -> ()) in
  let ss' = Stream.sample ~clk ss in
  let buf = Queue.create () in
  Stream.observe ss' (Queue.enqueue buf);
  run_time_line signal signal' time_line;
  assert_equal ~ctxt expect (Queue.to_list buf)

let hd values ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let f = Stream.hd ss in
  Signal.repeat signal ~times:2 ();
  match values with 
  | [] -> assert_false "hd is not decided" (Future.is_decided f)
  | _ ->
    assert_equal ~ctxt (List.hd_exn values) (Future.peek_exn f)
      
let tl ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let ss' = Stream.tl ss in
  let expect = List.drop values 1 in
  check_stream ctxt expect ss' ~repeat:(List.length expect + 1) signal 

let base_find values ~find f expect ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let future = find ss ~f in
  Signal.repeat signal ~times:(List.length values + 1) ();
  match expect with
  | None ->
    assert_false "find: future mustn't be decided"
      (Future.is_decided future)
  | Some v ->
    assert_equal ~ctxt v (Future.peek_exn future)

let find values ~f ~expect ctxt = 
  base_find values ~find:Stream.find f expect ctxt 

let find_map values ~f ~expect ctxt =
  base_find values ~find:Stream.find_map f expect ctxt 

let take ~values ~expect ~how_match ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let vals = Stream.take ss how_match in
  Signal.repeat signal ~times:(List.length values + 1) ();
  assert_equal ~ctxt expect (Future.peek_exn vals)

let nth values ~expect ~n ctxt =
  let ss,signal, _ = Stream.of_list values in
  let f = Stream.nth ss n in
  Signal.repeat signal ~times:(n + 1) ();
  match expect with 
  | None -> 
    assert_false "nth: future mustn't be decided"
      (Future.is_decided f)
  | Some v -> 
    assert_equal ~ctxt v (Future.peek_exn f)

let fulfill_after_signals promise ~after ~max signal = 
  let rec run ind = 
    if ind > max then ()
    else
      let () = Signal.send signal () in
      if ind = after then Promise.fulfill promise ();
      run (ind + 1) in
  run 0

let upon values ~expect ~after ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let event, promise = Future.create () in
  let f = Stream.upon event ss in
  let max = List.length values in
  fulfill_after_signals promise ~after ~max signal;
  match expect with
  | None -> 
    assert_false "upon: future mustn't be decided"
      (Future.is_decided f)
  | Some v -> 
    assert_equal ~ctxt v (Future.peek_exn f)

let last_before values ~expect ~event_after ~last ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let event, promise = Future.create () in
  let f = Stream.last_before event ss last in
  let max = List.length values in
  fulfill_after_signals promise ~after:event_after ~max signal;
  assert_equal ~ctxt expect (Future.peek_exn f)

let before ctxt = 
  let ss,signal, _ = Stream.of_list values in
  let event, promise = Future.create () in
  let f = Stream.before event ss in
  Signal.repeat signal ~times:4 ();
  Promise.fulfill promise ();
  let expect = [0;1;2;3;] in
  assert_equal ~ctxt expect (Future.peek_exn f)

let suite =  
  "Future_Stream" >:::
  [
    "send signal"       >:: send_signal;
    "repeat signal: 0"  >:: repeat_signal ~times:0;
    "repeat signal: 4"  >:: repeat_signal ~times:4;
    "create"            >:: create;
    "from"              >:: from;
    "unfold"            >:: unfold;
    "unfold_until"      >:: unfold_until;
    "unfold'"           >:: unfold';
    "repeat"            >:: repeat 42 ~times:3;
    "repeat"            >:: repeat 42 ~times:0;
    "of_list"           >:: of_list;
    "of_array"          >:: of_array;
    "of_sequence"       >:: of_sequence;
    "watch"             >:: watch;
    "observe"           >:: observe;
    "subscribe"         >:: subscribe;
    "unsubscribe"       >:: unsubscribe;
    "on_wait"           >:: on_wait;
    "on_subscribe"      >:: on_subscribe;
    "on_unsubscribe"    >:: on_unsubscribe;
    "has_subscribers"   >:: has_subscribers;
    "diff subscribe id" >:: different_subscribe_id;
    "diff watch id"     >:: different_watch_id;
    "same watch id"     >:: same_watch_id;
    "map'"              >:: map';
    "map"               >:: map;
    "apply"             >:: apply;
    "split"             >:: split;
    "unzip"             >:: unzip;
    "once"              >:: once;
    "parse"             >:: parse;
    "before"            >:: before;
    "hd"                >:: hd values;
    "hd empty"          >:: hd [];
    "tl"                >:: tl;  
    "find existed"      >:: find values ~f:(fun x -> x = 3) ~expect:(Some 3);
    "find not existed"  >:: find values ~f:(fun x -> x = 10) ~expect:None;
    "take 3"            >:: take ~values ~expect:[0;1;2] ~how_match:3;
    "take 0"            >:: take ~values ~expect:[] ~how_match:0;
    "nth: n=3"          >:: nth values ~expect:(Some 3) ~n:3;
    "nth: n=10"         >:: nth values ~expect:None ~n:10;
    "upon: after 3"     >:: upon values ~expect:(Some 3) ~after:3;
    "upon: after 10"    >:: upon values ~expect:None ~after:10;

    "zip diff length"   >:: 
    zip ~values:[0;1;2;3;] ~values':[0; 1; 2; 3; 4; 5;] 
      ~expect:[0,0; 1,1; 2,2; 3,3;];

    "filter_map"        >:: 
    filter_map ~values:[0;1;2;3;4;5;6;7;8] ~expect:[0;2;4;6;8] ~f:even_opt;

    "merge diff length" >:: 
    merge ~values:[0;1;2;3;] ~values':[0.; 1.; 2.; 3.; 4.; 5.; ]
      ~expect:[0.5; 2.5; 4.5; 6.5] ~f:(fun x y -> float x +. y +. 0.5);

    "foldw stride=1 width=3" >::
    foldw ~f:(+) ~init:0 ~stride:1 ~width:3 ~values 
      ~times:(List.length values + 1) ~expect:[3; 6; 9; 12; 15; 18] ;

    "foldw stride=2 width=3" >::
    foldw ~f:(+) ~init:0 ~stride:2 ~width:3 ~values
    ~times:(List.length values + 1) ~expect:[3; 9; 15];

    "foldw stride=2 width=1" >::
    foldw ~f:(+) ~init:0 ~stride:2 ~width:1 ~values
    ~times:(List.length values + 1) ~expect:[0;2;4;6;];

    "foldw stride=3 width=1" >::
    foldw ~f:(+) ~init:0 ~stride:3 ~width:1 ~values
      ~times:(List.length values + 1) ~expect:[0; 3; 6;] ;

    "foldw stride=4 width=2" >::
    foldw ~f:(+) ~init:0 ~stride:4 ~width:2 ~values
      ~times:(List.length values + 1) ~expect:[1; 9;] ;

    "foldw stride=1 width=1" >::
    foldw ~f:(+) ~init:0 ~stride:1 ~width:1 ~values
      ~times:(List.length values + 1) ~expect:values;

    "foldw: stride = 0; width = 0; size = 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:0 ~width:0
      ~values:[] ~times:10 ~expect:[];

    "foldw: stride = 0; width = 0; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:0 ~width:0
      ~values:[0;1;2] ~times:4
      ~expect:[[1]; [1]; [1];];

    "foldw: stride = 0; width > 0; size = 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:0 ~width:3
      ~values:[] ~times:10 ~expect:[];

    "foldw: stride = 0; 0 < width < size; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[] ~stride:0 ~width:2
      ~values ~times:10
      ~expect:[[0; 1]; [0;1]; [0;1]; [0;1]; [0;1]; [0;1]; [0;1] ];

    "foldw: stride = 0; width = size ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[] ~stride:0
      ~width:(List.length values)
      ~values ~times:(List.length values + 1) ~expect:[values;];

    "foldw: stride = 0; width > size ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:0
      ~width:(List.length values + 1)
      ~values ~times:(List.length values) ~expect:[];

    "foldw: stride > 0; width = 0; size = 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:3
      ~width:0
      ~values:[] ~times:10 ~expect:[];

    "foldw: stride > 0; width > 0; size = 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:3
      ~width:3
      ~values:[] ~times:10 ~expect:[];

    "foldw: 0 < stride < size; width = size ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[] ~stride:2
      ~width:(List.length values)
      ~values ~times:(List.length values + 1) ~expect:[values;];

    "foldw: 0 < stride < size; 0 < width < size; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[] ~stride:3
      ~width:3
      ~values ~times:(List.length values + 1)
      ~expect:[[0;1;2;]; [3;4;5]];

    "foldw: 0 < stride < size; width = 0; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1] ~stride:3
      ~width:0
      ~values ~times:(List.length values + 1)
      ~expect:[[1]; [1]; [1]];

    "foldw: stride > size; width > size; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1]
      ~stride:(List.length values + 1)
      ~width:(List.length values + 1)
      ~values ~times:(List.length values + 1)
      ~expect:[];

    "foldw: stride > size; width = 0; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[1]
      ~stride:(List.length values + 1)
      ~width:0
      ~values ~times:(List.length values + 1)
      ~expect:[[1];];
    
    "foldw: width < stride < size; 0 < width < size; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[]
      ~stride:3 ~width:2
      ~values ~times:(List.length values + 1)
      ~expect:[[0;1]; [3; 4]; [6; 7]];

    "foldw: stride > size; width < size ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[]
      ~stride:(List.length values + 1)
      ~width:2
      ~values ~times:(List.length values + 1)
      ~expect:[[0; 1]; ];

    "foldw: stride < size; width > stride ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[]
      ~stride:2
      ~width:4
      ~values ~times:(List.length values + 1)
      ~expect:[[0;1;2;3;]; [2;3;4;5;]; [4;5;6;7;]];

    "foldw: stride = size; width > size ; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[]
      ~stride:(List.length values)
      ~width:(List.length values + 1)
      ~values ~times:(List.length values + 1)
      ~expect:[];

    "foldw: stride = size; width = size; size > 0" >::
    foldw ~f:(fun xs x -> xs @ [x]) ~init:[]
      ~stride:(List.length values)
      ~width:(List.length values)
      ~values ~times:(List.length values + 1)
      ~expect:[values;];

    "foldw_range: stride = [0..10], width = [0..10]" >::
    foldw_range ~f:(fun xs x -> x :: xs) ~init:[]
      ~max_stride:10 ~max_width:10;

    "frame 
      clk:  T T    T  
     data: 0    12  34567 " >::
    frame values ~expect:[[0]; []; [1;2] ] ~time_line:[1;1;3;];

    "frame 
      clk: T T  T      
     data:  0 12 34567 " >::
    frame values ~expect:[ []; [0]; [1;2] ] ~time_line:[0;1;3;];

    "frame 
      clk: T T   T T     
     data:    012 3 4567" >::
    frame values ~expect:[[]; []; [0; 1; 2;]; [3] ] ~time_line:[0;0;3;4];

    "frame 
      clk:         T     
     data: 01234567" >::
    frame values ~expect:[ values;] ~time_line:[List.length values;];

    "sample" >:: 
    sample values ~expect:[Some 0; None; Some 1 ] ~time_line:[1;1;3;];

    "sample" >:: 
    sample values ~expect:[None; None; Some 0; Some 1 ] 
      ~time_line:[0; 0; 1; 2;];

    "find_map"  >:: find_map values 
      ~f:(fun x -> 
          if x = 3 then Some (Int.succ x) else None) ~expect:(Some 4);

    "find_map"  >:: find_map values 
      ~f:(fun x -> 
          if x = 10 then Some (Int.succ x) else None) ~expect:None;

    "last_before: after=2; length=3:
                  |event
     data: 0 1 2 3 4 5 6 7         "  >:: 
    last_before values ~expect:[0;1;2] ~event_after:2 ~last:3;
    
    "last_before: after=2; length=4
                |event
     data: 0 1 2 3 4 5 6 7         "  >:: 
    last_before values ~expect:[0;1;2] ~event_after:2 ~last:4;

    "last_before: after=4; length=2:
                    |event
     data: 0 1 2 3 4 5 6 7         "  >:: 
    last_before values ~expect:[3;4] ~event_after:4 ~last:2;

  ]
