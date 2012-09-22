#use "topfind";;
#require "OUnit";;
open OUnit;;

(* Tests for SmallStream *)
#load "smallStream.cmo";;
open SmallStream;;
module S = SmallStream

let stream_321 = lazy (S.Cons(3, (lazy (S.Cons(2, lazy (S.Cons(1, lazy S.Nil)))))));;
let stream_933 = lazy (S.Cons(9, (lazy (S.Cons(3, lazy (S.Cons(3, lazy S.Nil)))))));;
let stream_ttt = lazy (S.Cons(true, (lazy (S.Cons(true, lazy (S.Cons(true, lazy S.Nil)))))));;
let stream_ftf = lazy (S.Cons(false, (lazy (S.Cons(true, lazy (S.Cons(false, lazy S.Nil)))))));;
let stream_inf = S.repeat 1;;
let stream_inf2 = S.repeat 2;;

let test_concat _ = match S.(++) stream_321 stream_933 with
  | (lazy (S.Cons(3, (lazy (S.Cons(2, lazy (S.Cons(1, lazy (S.Cons(9, (lazy
  (S.Cons(3, lazy (S.Cons(3, lazy S.Nil))))))))))))))) -> assert_bool "ok" true;
  | s ->  S.print s; print_newline(); assert_failure "not correct stream"
;;

let test_drop _ = match S.drop (2, stream_321) with
  | (lazy S.Nil) -> assert_failure "stream has only one elm."
  | (lazy (S.Cons(x, _))) -> assert_equal 1 x
;;

let test_take _ = match S.drop (3, (S.take (4, stream_inf))) with
  | (lazy S.Nil) -> assert_failure "stream_inf has no S.Nil."
  | (lazy (S.Cons(x, _))) -> assert_equal 1 x
;;

let test_append _ = match S.drop (9, (S.take (10, (S.(++) stream_inf2 stream_inf)))) with
  | (lazy S.Nil) -> assert_failure "stream_inf has no S.Nil."
  | (lazy (S.Cons(x, _))) -> assert_equal 2 x
;;

let test_reverse _ = match S.reverse (S.reverse stream_321) with
  | (lazy S.Nil) -> assert_failure "stream has elms."
  | (lazy (S.Cons(x, _))) -> assert_equal 3 x
;;

let suite = "Test SmallStream" >:::
  ["test_concat"  >:: test_concat;
   "test_drop"    >:: test_drop;
   "test_take"    >:: test_take;
   "test_append"  >:: test_append;
   "test_reverse" >:: test_reverse;
  ]
;;

let _ = run_test_tt_main suite;;

