#use "topfind";;
#require "OUnit";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "smallStream.cmo";;
#load "rqueue.cmo";;
#load "realTimeQueue.cmo";;
#load "hoodMelvilleQueue.cmo";;
#load "bootStrappedQueue.cmo";;
#load "catenableList.cmo";;

open OUnit;;
open CatenableList;;

module B = IntBootStrappedCatenableList;;
module H = IntHoodMelvilleCatenableList;;
module R = IntRealTimeCatenableList;;

module P (C : CATENABLELIST with type elt = int) = struct
  let make_list_cons n =
    let rec make_list n i c =
      if i < n
      then make_list n (i+1) (C.cons (i, c))
      else C.cons (i, c) in
    if n = 0 then C.empty
    else make_list n 1 C.empty
  ;;

  let make_list_snoc n =
    let rec make_list n i c =
      if i < n
      then make_list n (i+1) (C.snoc (c, i))
      else C.snoc (c, i) in
    if n = 0 then C.empty
    else make_list n 1 C.empty
  ;;

  let print c = C.dprint false c
  ;;
  let print_n n =
    let rec print_n' = function
      | 0 -> ()
      | i ->
          print_string "n=";
          print_int (n-i);
          print_string "\n";
          print (make_list_snoc (n-i));
          print_newline();
          print_n' (i-1) in
    print_n' n
  ;;
end
;;


module PB = P (B);;
module PH = P (H);;
module PR = P (R);;

print_string "CatenableList with BootStrappedQueue:\n"; PB.print_n 4;;
print_string "CatenableList with HoodMelvilleQueue:\n"; PH.print_n 4;;
print_string "CatenableList with RealTimeQueue:\n";     PR.print_n 4;;

let list_21 = PB.make_list_snoc 21;;
print_string "CatenableList with RealTimeQueue:\nn = 21:\n"; B.dprint false list_21;;
print_string "CatenableList with RealTimeQueue:\nn = 21 - 1:\n"; B.dprint false (B.tail list_21);;

let list_3 = PR.make_list_snoc 3;;
let list_4 = PR.make_list_snoc 4;;
let list_5 = PR.make_list_snoc 5;;
let list_list = [list_3; list_4; list_5;];;
print_string "Catnate a list of CatenableList with RealTimeQueue:\n";
print_string "[n = 3; n = 4; n = 5;]"; R.dprint true (R.concat list_list);;

(*
let test _ =
  let rec toCList = function
    | (c, []) -> c 
    | (c, x :: xs') ->
    toCList (Q.cons (x, c), xs') in
  let rec toList c =
    if Q.isEmpty c then []
    else Q.head c :: toList (Q.tail c) in
  let elements = [3;98;23;84;7;22;1;98;2321;8848;33;29;48;41;9;8;111;233;58;343;2;32] in
  let result = toList (toQueue (Q.empty, elements)) in
  assert_equal elements result
;;

let suite = "Test CatenableList" >:::
  ["testBootStrapped" >:: testBootStrapped
   "testHoodMelviile" >:: testHoodMelville
   "testRealTime"     >:: testRealTime
  ]
;;

let _ = run_test_tt_main suite;;
*)
