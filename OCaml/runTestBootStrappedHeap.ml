#use "topfind";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "smallStream.cmo";;
#load "heap.cmo";;
#load "scheduledBinomialHeap.cmo";;
#load "bootStrappedHeap.cmo";;

open BootStrappedHeap;;
module H = IntBootStrappedHeap

let make_heap n =
  let rec make_heap n i h =
    if i < n
    then make_heap n (i+1) (H.insert (i, h))
    else H.insert (i, h)
  in make_heap n 1 H.empty
;;

let rec print_heap h =
  if H.isEmpty h then
    (print_string "none";
    print_newline ())
  else
    (print_int (H.findMin h);
    print_newline ();
    print_heap (H.deleteMin h))
;;

print_string "n=0\n"; H.print H.empty;;
print_string "n=1\n"; H.print (make_heap 1);;
print_string "n=2\n"; H.print (make_heap 2);;
print_string "n=3\n"; H.print (make_heap 3);;
print_string "n=4\n"; H.print (make_heap 4);;
print_string "n=5\n"; H.print (make_heap 5);;
print_string "n=6\n"; H.print (make_heap 6);;
print_string "n=7\n"; H.print (make_heap 7);;
print_string "n=8\n"; H.print (make_heap 8);;
print_string "n=9\n"; H.print (make_heap 9);;
(*
print_string "n=1023\n"; H.print (make_heap 1023);;
*)

print_string "TEST\n";;
H.print (make_heap 8);;
H.print (H.deleteMin (make_heap 8));;
H.print (H.deleteMin (H.deleteMin (make_heap 8)));;

print_string "TEST2\n";;
let test_1 = H.insert (9, (H.insert (7, (H.insert (6, (H.insert (2, H.empty)))))));;
let test_2 = H.insert (5, (H.insert (4, (H.insert (3, H.empty)))));;
H.print test_1;;
H.print test_2;;
H.print (H.merge (test_1, test_2));;

print_string "n=32:\n";;
print_heap (make_heap 32);;
