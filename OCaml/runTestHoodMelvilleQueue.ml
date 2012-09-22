#use "topfind";;
#require "OUnit";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "rqueue.cmo";;
#load "hoodMelvilleQueue.cmo";;

open OUnit;;
open HoodMelvilleQueue;;
module Q = IntHoodMelvilleQueue

let make_queue n =
  let rec make_queue n i q =
    if i < n
    then make_queue n (i+1) (Q.snoc q i)
    else Q.snoc q i
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
