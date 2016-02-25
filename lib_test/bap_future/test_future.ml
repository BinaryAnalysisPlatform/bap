
open OUnit2
open Core_kernel.Std

open Bap_future.Std

let magic_value = 42

let fulfill ctxt = 
  let future, promise = Future.create () in
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt magic_value (Future.peek_exn future)

let is_fulfilled ctxt = 
  let future, promise = Future.create () in
  assert_bool "is_fulfilled" (not (Promise.is_fulfilled promise));
  Promise.fulfill promise magic_value;
  assert_bool "is_fulfilled" (Promise.is_fulfilled promise)

let define_twice ctxt = 
  let future, promise = Future.create () in
  Promise.fulfill promise magic_value;
  let result = 
    try
      Promise.fulfill promise magic_value;
      false
    with Invalid_argument _ -> true in  
  assert_bool "define twice failed" result

let upon ctxt = 
  let future, promise = Future.create () in
  let result = ref None in
  let action x = result := Some x in
  Future.upon future action;
  assert_equal ~ctxt None !result;
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt magic_value (Option.value_exn !result)

let is_decided ctxt = 
  let future, promise = Future.create () in
  assert_bool "is_decided: false" (not (Future.is_decided future));
  Promise.fulfill promise magic_value;
  assert_bool "is_decided: true" (Future.is_decided future)

let peek ctxt = 
  let future, promise = Future.create () in
  assert_equal ~ctxt None (Future.peek future);
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt (Some magic_value) (Future.peek future)

let peek_exn ctxt = 
  let future, promise = Future.create () in
  let result = 
    try
      let _ = Future.peek_exn future in
      false
    with Invalid_argument  _ -> true in
  assert_bool "peek exn failed" result;
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt magic_value (Future.peek_exn future)

let bind ctxt = 
  let future, promise = Future.create () in
  let change x = 
    let future', promise' = Future.create () in
    Promise.fulfill promise' (Int.succ x);
    future' in
  let future' = Future.bind future change in
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt (Int.succ magic_value) (Future.peek_exn future')

let return ctxt =
  let f = Future.return magic_value in
  assert_equal ~ctxt magic_value (Future.peek_exn f)

let map ctxt = 
  let future, promise = Future.create () in
  let future' = Future.map future ~f:Int.succ in
  Promise.fulfill promise magic_value;
  assert_equal ~ctxt (Int.succ magic_value) (Future.peek_exn future')

let suite =  
  "Future" >:::
  [
    "fulfill"            >:: fulfill;
    "is_fulfilled"       >:: is_fulfilled;
    "define_twice"       >:: define_twice;
    "upon"               >:: upon;
    "is_decided"         >:: is_decided;
    "peek"               >:: peek; 
    "peek_exn"           >:: peek_exn;
    "bind"               >:: bind;
    "map"                >:: map;
    "return"             >:: return;
  ]
