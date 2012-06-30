#use "topfind";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "heap.cmo";;
#load "scheduledBinomialHeap.cmo";;

open ScheduledBinomialHeap;;
module H = IntHeap

let heap_0 = H.empty;;

let heap_1_0 = H.empty;;
let heap_1_1 = H.insert (1, heap_1_0);;

let heap_2_0 = H.empty;;
let heap_2_1 = H.insert (1, heap_2_0);;
let heap_2_2 = H.insert (2, heap_2_1);;

let heap_3_0 = H.empty;;
let heap_3_1 = H.insert (1, heap_3_0);;
let heap_3_2 = H.insert (2, heap_3_1);;
let heap_3_3 = H.insert (3, heap_3_2);;

let heap_4_0 = H.empty;;
let heap_4_1 = H.insert (1, heap_4_0);;
let heap_4_2 = H.insert (2, heap_4_1);;
let heap_4_3 = H.insert (3, heap_4_2);;
let heap_4_4 = H.insert (4, heap_4_3);;

let heap_5_0 = H.empty;;
let heap_5_1 = H.insert (1, heap_5_0);;
let heap_5_2 = H.insert (2, heap_5_1);;
let heap_5_3 = H.insert (3, heap_5_2);;
let heap_5_4 = H.insert (4, heap_5_3);;
let heap_5_5 = H.insert (5, heap_5_4);;

let heap_6_0 = H.empty;;
let heap_6_1 = H.insert (1, heap_6_0);;
let heap_6_2 = H.insert (2, heap_6_1);;
let heap_6_3 = H.insert (3, heap_6_2);;
let heap_6_4 = H.insert (4, heap_6_3);;
let heap_6_5 = H.insert (5, heap_6_4);;
let heap_6_6 = H.insert (6, heap_6_5);;

let heap_7_0 = H.empty;;
let heap_7_1 = H.insert (1, heap_7_0);;
let heap_7_2 = H.insert (2, heap_7_1);;
let heap_7_3 = H.insert (3, heap_7_2);;
let heap_7_4 = H.insert (4, heap_7_3);;
let heap_7_5 = H.insert (5, heap_7_4);;
let heap_7_6 = H.insert (6, heap_7_5);;
let heap_7_7 = H.insert (7, heap_7_6);;

let heap_8_0 = H.empty;;
let heap_8_1 = H.insert (1, heap_8_0);;
let heap_8_2 = H.insert (2, heap_8_1);;
let heap_8_3 = H.insert (3, heap_8_2);;
let heap_8_4 = H.insert (4, heap_8_3);;
let heap_8_5 = H.insert (5, heap_8_4);;
let heap_8_6 = H.insert (6, heap_8_5);;
let heap_8_7 = H.insert (7, heap_8_6);;
let heap_8_8 = H.insert (8, heap_8_7);;

let heap_9_0 = H.empty;;
let heap_9_1 = H.insert (1, heap_9_0);;
let heap_9_2 = H.insert (2, heap_9_1);;
let heap_9_3 = H.insert (3, heap_9_2);;
let heap_9_4 = H.insert (4, heap_9_3);;
let heap_9_5 = H.insert (5, heap_9_4);;
let heap_9_6 = H.insert (6, heap_9_5);;
let heap_9_7 = H.insert (7, heap_9_6);;
let heap_9_8 = H.insert (8, heap_9_7);;
let heap_9_9 = H.insert (9, heap_9_8);;

print_string "n=0\n"; H.print heap_0;;
print_string "n=1\n"; H.print heap_1_1;;
print_string "n=2\n"; H.print heap_2_2;;
print_string "n=3\n"; H.print heap_3_3;;
print_string "n=4\n"; H.print heap_4_4;;
print_string "n=5\n"; H.print heap_5_5;;
print_string "n=6\n"; H.print heap_6_6;;
print_string "n=7\n"; H.print heap_7_7;;
print_string "n=8\n"; H.print heap_8_8;;
print_string "n=9\n"; H.print heap_9_9;;

let heap_1023 =
  let rec heap_insert i q =
    if i < 1023
    then heap_insert (i+1) (H.insert (i,q))
    else H.insert (i,q)
  in heap_insert 1 H.empty
;;

print_string "n=1023\n"; H.print heap_1023;;
