open Core_kernel
open Bap.Std
open OUnit2

module T = Trie.String.Prefix;;

let longest_match _ =
  let (=) = Option.equal (fun (a,b) (c,d) -> a = c && b = d) in
  let t = T.create () in
  T.add t "" 0;
  T.add t "a" 1;
  T.add t "abcde" 3;

  assert_bool "1." (T.longest_match t "" = Some (0,0));
  assert_bool "1.a" (T.longest_match t "a" = Some (1,1));
  assert_bool "1.ab" (T.longest_match t "ab" = Some (1,1));
  assert_bool "1.abc" (T.longest_match t "abc" = Some (1,1));
  assert_bool "1.abcd" (T.longest_match t "abcd" = Some (1,1));
  assert_bool "1.abcde" (T.longest_match t "abcde" = Some (5,3));
  assert_bool "1.abcdef" (T.longest_match t "abcdef" = Some (5,3));
  assert_bool "1.nothing" (T.longest_match t "nothing" = Some (0,0));

  T.add t "abc" 2;

  assert_bool "2." (T.longest_match t "" = Some (0,0));
  assert_bool "2.a" (T.longest_match t "a" = Some (1,1));
  assert_bool "2.ab" (T.longest_match t "ab" = Some (1,1));
  assert_bool "2.abc" (T.longest_match t "abc" = Some (3,2));
  assert_bool "2.abcd" (T.longest_match t "abcd" = Some (3,2));
  assert_bool "2.abcde" (T.longest_match t "abcde" = Some (5,3));
  assert_bool "2.abcdef" (T.longest_match t "abcdef" = Some (5,3));
  assert_bool "2.nothing" (T.longest_match t "nothing" = Some (0,0));

  T.remove t "abc";

  assert_bool "3." (T.longest_match t "" = Some (0,0));
  assert_bool "3.a" (T.longest_match t "a" = Some (1,1));
  assert_bool "3.ab" (T.longest_match t "ab" = Some (1,1));
  assert_bool "3.abc" (T.longest_match t "abc" = Some (1,1));
  assert_bool "3.abcd" (T.longest_match t "abcd" = Some (1,1));
  assert_bool "3.abcde" (T.longest_match t "abcde" = Some (5,3));
  assert_bool "3.abcdef" (T.longest_match t "abcdef" = Some (5,3));
  assert_bool "3.nothing" (T.longest_match t "nothing" = Some (0,0));

  T.remove t "";

  assert_bool "4." (T.longest_match t "" = None);
  assert_bool "4.a" (T.longest_match t "a" = Some (1,1));
  assert_bool "4.ab" (T.longest_match t "ab" = Some (1,1));
  assert_bool "4.abc" (T.longest_match t "abc" = Some (1,1));
  assert_bool "4.abcd" (T.longest_match t "abcd" = Some (1,1));
  assert_bool "4.abcde" (T.longest_match t "abcde" = Some (5,3));
  assert_bool "4.abcdef" (T.longest_match t "abcdef" = Some (5,3));
  assert_bool "4.nothing" (T.longest_match t "nothing" = None)

let find _ =
  let (=) = Option.equal (fun a b -> a = b) in
  let t = T.create () in
  T.add t "a" 1;
  T.add t "abcde" 3;
  T.add t "abc" 2;
  T.remove t "abc";
  T.remove t "";

  assert_bool "1." (T.find t "" = None);
  assert_bool "1.a" (T.find t "a" = Some 1);
  assert_bool "1.ab" (T.find t "ab" = None);
  assert_bool "1.abc" (T.find t "abc" = None);
  assert_bool "1.abcd" (T.find t "abcd" = None);
  assert_bool "1.abcde" (T.find t "abcde" = Some 3);
  assert_bool "1.abcdef" (T.find t "abcdef" = None);
  assert_bool "1.nothing" (T.find t "nothing" = None)

let suite () = "Trie" >::: [
      "longest-match" >:: longest_match;
      "find" >:: find;
  ]
