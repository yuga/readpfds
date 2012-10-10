#use "topfind";;
#require "OUnit";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "rqueue.cmo";;
#load "bootStrappedQueue.cmo";;

open OUnit;;
open BootStrappedQueue;;
module Q = IntBootStrappedQueue

let make_queue n =
  let rec make_queue n i q =
    if i < n
    then make_queue n (i+1) (Q.snoc (q, i))
    else Q.snoc (q, i)
  in make_queue n 1 Q.empty
;;

print_string "n=0\n"; Q.print Q.empty;;
print_string "n=1\n"; Q.print (make_queue 1);;
print_string "n=2\n"; Q.print (make_queue 2);;
print_string "n=3\n"; Q.print (make_queue 3);;
print_string "n=4\n"; Q.print (make_queue 4);;
print_string "n=5\n"; Q.print (make_queue 5);;
print_string "n=6\n"; Q.print (make_queue 6);;
print_string "n=7\n"; Q.print (make_queue 7);;
print_string "n=8\n"; Q.print (make_queue 8);;
print_string "n=9\n"; Q.print (make_queue 9);;
print_string "n=10\n"; Q.print (make_queue 10);;

let test _ =
  let rec toQueue = function
    | (q, []) -> q 
    | (q, x :: xs') ->
    toQueue (Q.snoc (q, x), xs') in
  let rec toList q =
    if Q.isEmpty q then []
    else Q.head q :: toList (Q.tail q) in
  let elements = [3;98;23;84;7;22;1;98;2321;8848;33;29;48;41;9;8;111;233;58;343;2;32] in
  let result = toList (toQueue (Q.empty, elements)) in
  assert_equal elements result
;;

let suite = "Test BootStrappedQueue" >:::
  ["test"        >:: test
  ]
;;

let _ = run_test_tt_main suite;;

