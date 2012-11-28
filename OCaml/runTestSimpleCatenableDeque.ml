#use "topfind";;
#require "OUnit";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "rqueue.cmo";;
#load "realTimeDeque.cmo";;
#load "simpleCatenableDeque.cmo";;

open OUnit;;
open SimpleCatenableDeque;;
module Q = IntSimpleCatenableDeque

let make_queue n =
  let rec make_queue n i q =
    if i < n
    then make_queue n (i+1) (Q.snoc (q, i))
    else Q.snoc (q, i)
  in make_queue n 1 Q.empty
;;

let tail_queue n q =
  let rec tail_queue n i q =
    if i < n
    then tail_queue n (i+1) (Q.tail q)
    else Q.tail q
  in tail_queue n 1 q
;;

let qprint = Q.dprint false
;;

print_string "n=0\n"; qprint Q.empty;;
print_string "n=1\n"; qprint (make_queue 1);;
print_string "n=2\n"; qprint (make_queue 2);;
print_string "n=3\n"; qprint (make_queue 3);;
print_string "n=4\n"; qprint (make_queue 4);;
print_string "n=5\n"; qprint (make_queue 5);;
print_string "n=6\n"; qprint (make_queue 6);;

print_string "---------------------------------";;

let queue_00 = Q.empty;;
let queue_01 = make_queue 1;;
let queue_02 = make_queue 2;;
let queue_12 = make_queue 12;;
let queue_13 = make_queue 13;;
let queue_00_01 = Q.(++) queue_00 queue_01;;
let queue_01_00 = Q.(++) queue_01 queue_00;;
let queue_01_02 = Q.(++) queue_01 queue_02;;
let queue_02_01 = Q.(++) queue_02 queue_01;;
let queue_12_13 = Q.(++) queue_12 queue_13;;
let queue_12_13_12_13 = Q.(++) queue_12_13 queue_12_13;;
print_string "n=00\n"; qprint queue_00;;
print_string "n=01\n"; qprint queue_01;;
print_string "n=02\n"; qprint queue_02;;
print_string "n=00+01\n"; qprint queue_00_01;;
print_string "n=01+00\n"; qprint queue_01_00;;
print_string "n=01+02\n"; qprint queue_01_02;;
print_string "n=02+01\n"; qprint queue_02_01;;
print_string "n=12+13\n"; qprint queue_12_13;;
print_string "n=12+13-11\n"; qprint (tail_queue 11 queue_12_13);;
print_string "n=12+13-23\n"; qprint (tail_queue 23 queue_12_13);;
print_string "n=12+13-24\n"; qprint (tail_queue 24 queue_12_13);;
print_string "n=12+13-25\n"; qprint (tail_queue 25 queue_12_13);;
print_string "n=12+13+12+13\n"; qprint queue_12_13_12_13;;
print_string "n=12+13+12+13-45\n"; qprint (tail_queue 45 queue_12_13_12_13);;

