#use "topfind";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "heap.cmo";;
#load "scheduledBinomialHeap.cmo";;

open ScheduledBinomialHeap;;
module H = IntHeap

let make_heap n =
  let rec make_heap n i h =
    if i < n
    then make_heap n (i+1) (H.insert (i, h))
    else H.insert (i, h)
  in make_heap n 1 H.empty
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
print_string "n=1023\n"; H.print (make_heap 1023);;
