#use "topfind";;
#require "OUnit";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "randomAccessList.cmo";;
#load "altBinaryRandomAccessList.cmo";;

open OUnit;;
open RandomAccessList;;
open AltBinaryRandomAccessList;;

module L = IntAltBinaryRandomAccessList

let cons_rlist n =
  let rec cons_rlist' = function
    | (0, a) -> a
    | (i, a) -> cons_rlist' (i-1, L.cons (n-i, a))
  in cons_rlist' (n, L.empty)
;;

let rec tail_rlist n rlist = match n with
  | 0 -> rlist
  | i -> tail_rlist (i-1) (L.tail rlist)
;;

(* 0 *)
let rlist_0 = cons_rlist 0;;
print_string "n = 0:\n"; L.print rlist_0;;

(* 1 *)
let rlist_1 = cons_rlist 1;;
print_string "n = 1:\n"; L.print rlist_1;;

(* 11 *)
let rlist_3 = cons_rlist 3;;
print_string "n = 3:\n";  L.print rlist_3;;
print_string "n = 4:\n"; L.print (L.cons (3, rlist_3));;

(* 1001 *)
let rlist_9 = cons_rlist 9;;

(* 11111 *)
let rlist_30 = cons_rlist 30;;
print_string "n = 30:\n"; L.print rlist_30;;
print_string "n = 31:\n"; L.print (L.cons (30, rlist_30));;
print_string "n = 32:\n"; L.print (L.cons (31, (L.cons (30, rlist_30))));;
print_string "n = 33:\n"; L.print (L.cons (32, (L.cons (31, L.cons (30, rlist_30)))));;

(* 1111 *)
let rlist_16 = cons_rlist 16;;
print_string "n = 16 - 1 = 15\n"; L.print (L.tail rlist_16);;

(* test *)
let test_0 _ = assert_bool "ok" (L.isEmpty rlist_0)
;;

let test_9 _ = match L.isEmpty rlist_9 with
  | false -> assert_bool "ok" true
  | _  -> assert_failure "not correct" 
;;

let test_9_head = assert_equal (8, rlist_9)

let test_9_lookup _ =
  assert_equal 8 (L.lookup (0, rlist_9));
  assert_equal 7 (L.lookup (1, rlist_9));
  assert_equal 6 (L.lookup (2, rlist_9));
  assert_equal 5 (L.lookup (3, rlist_9));
  assert_equal 4 (L.lookup (4, rlist_9));
  assert_equal 3 (L.lookup (5, rlist_9));
  assert_equal 2 (L.lookup (6, rlist_9));
  assert_equal 1 (L.lookup (7, rlist_9));
  assert_equal 0 (L.lookup (8, rlist_9))
;;

let test_9_update _ = 
  let rlist_9a = L.update (3, 10, rlist_9) in
  let rlist_9b = L.update (8, 11, rlist_9a) in
  let rlist_9c = L.update (5, 12, rlist_9b) in
  let rlist_9d = L.update (0, 13, rlist_9c) in
  let rlist_9e = L.cons (14, rlist_9d) in
  assert_equal 13 (L.head rlist_9d);
  assert_equal 14 (L.head rlist_9e);
  assert_equal 14 (L.lookup (0, rlist_9e));
  assert_equal 13 (L.lookup (1, rlist_9e));
  assert_equal 7 (L.lookup (2, rlist_9e));
  assert_equal 6 (L.lookup (3, rlist_9e));
  assert_equal 10 (L.lookup (4, rlist_9e));
  assert_equal 4 (L.lookup (5, rlist_9e));
  assert_equal 12 (L.lookup (6, rlist_9e));
  assert_equal 2 (L.lookup (7, rlist_9e));
  assert_equal 1 (L.lookup (8, rlist_9e));
  assert_equal 11 (L.lookup (9, rlist_9e))
;;

let suite = "Test LazyBinaryRandomAccessList Exercise 9.10" >:::
  ["test_0"        >:: test_0;
   "test_9"        >:: test_9;
   "test_9_lookup" >:: test_9_lookup;
   "test_9_update" >:: test_9_update;
  ]
;;

let _ = run_test_tt_main suite;;

