#use "topfind";;
#require "OUnit";;
#load "smallStream.cmo";;
open OUnit;;

(* Tests for SmallStream *)
open SmallStream.SmallStream;;

let stream_321 = lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))));;
let stream_933 = lazy (SScons(9, (lazy (SScons(3, lazy (SScons(3, lazy SSnil)))))));;
let stream_ttt = lazy (SScons(true, (lazy (SScons(true, lazy (SScons(true, lazy SSnil)))))));;
let stream_ftf = lazy (SScons(false, (lazy (SScons(true, lazy (SScons(false, lazy SSnil)))))));;
let stream_inf = repeat 1;;
let stream_inf2 = repeat 2;;

let test_drop _ = match drop 2 stream_321 with
  | (lazy SSnil) -> assert_failure "stream has only one elm."
  | (lazy (SScons(x, _))) -> assert_equal 1 x
;;

let test_take _ = match drop 3 (take 4 stream_inf) with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 1 x
;;

let test_append _ = match drop 9 (take 10 (stream_inf2 ++ stream_inf)) with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 2 x
;;

let test_reverse _ = match reverse (reverse stream_321) with
  | (lazy SSnil) -> assert_failure "stream has elms."
  | (lazy (SScons(x, _))) -> assert_equal 3 x
;;

let suite = "Test SmallStream" >:::
  ["test_drop"    >:: test_drop;
   "test_take"    >:: test_take;
   "test_append"  >:: test_append;
   "test_reverse" >:: test_reverse;
  ]
;;

let _ = run_test_tt_main suite;;

(* Tests for RealTimeQueue *)

