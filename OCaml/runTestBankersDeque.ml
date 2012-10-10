#use "topfind";;
#require "OUnit";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "rqueue.cmo";;
#load "bankersDeque.cmo";;

open OUnit;;
open BankersDeque;;
module Q = IntBankersDeque

let make_queue n =
  let rec make_queue n i q =
    if i < n
    then make_queue n (i+1) (Q.snoc (q, i))
    else Q.snoc (q, i)
  in make_queue n 1 Q.empty
;;

let snoc_queue n q =
  let rec snoc_queue n i q =
    if i < n
    then snoc_queue n (i+1) (Q.snoc (q, (i*(-1))))
    else q
  in snoc_queue n 0 q
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

let dqprint = Q.dprint true
;;

print_string "n=0\n"; qprint Q.empty;;
print_string "n=1\n"; qprint (make_queue 1);;
print_string "n=2\n"; qprint (make_queue 2);;
print_string "n=3\n"; qprint (make_queue 3);;
print_string "n=4\n"; qprint (make_queue 4);;
print_string "n=5\n"; qprint (make_queue 5);;
print_string "n=6\n"; qprint (make_queue 6);;
print_string "n=7\n"; qprint (make_queue 7);;
print_string "n=8\n"; qprint (make_queue 8);;
print_string "n=9\n"; qprint (make_queue 9);;
print_string "n=10\n"; qprint (make_queue 10);;
print_string "n=11\n"; qprint (make_queue 11);;
print_string "n=12\n"; qprint (make_queue 12);;
print_string "n=13\n"; qprint (make_queue 13);;
print_string "n=14\n"; qprint (make_queue 14);;
print_string "n=15\n"; qprint (make_queue 15);;
print_string "n=16\n"; qprint (make_queue 16);;
print_string "n=17\n"; qprint (make_queue 17);;
print_string "n=18\n"; qprint (make_queue 18);;
print_string "n=19\n"; qprint (make_queue 19);;
print_string "n=20\n"; qprint (make_queue 20);;
print_string "n=21\n"; qprint (make_queue 21);;
print_string "n=22\n"; qprint (make_queue 22);;
print_string "n=23\n"; qprint (make_queue 23);;
print_string "n=24\n"; qprint (make_queue 24);;
print_string "n=25\n"; qprint (make_queue 25);;
print_string "n=26\n"; qprint (make_queue 26);;
print_string "n=27\n"; qprint (make_queue 27);;
print_string "n=28\n"; qprint (make_queue 28);;
print_string "n=29\n"; qprint (make_queue 29);;
print_string "n=30\n"; qprint (make_queue 30);;
print_string "n=31\n"; qprint (make_queue 31);;
print_string "n=32\n"; qprint (make_queue 32);;
print_string "n=33\n"; qprint (make_queue 33);;

print_string "----------------------------------------\n";
print_string "create a deque having 30 elements";
print_string ", then apply tail function to it 15 times continuously.\n";;

let queue_30 = make_queue 30;;
print_string "n=30\n"; qprint queue_30;;
print_string "n=30-11\n"; qprint (tail_queue 11 queue_30);;

print_string "----------------------------------------\n";

print_string "n=24:\n";;
let queue_24 = snoc_queue 2 (tail_queue 1 (make_queue 23));;
qprint queue_24;;
let queue_23 = Q.tail queue_24;;
print_string "n=23:\n"; qprint queue_23;;
print_string "n=14:\n"; dqprint (tail_queue 9 queue_23);;

print_string "----------------------------------------\n";
print_string "create a deque having 100 elements";
print_string ", then apply tail function to it 10 times";
print_string " (print it at each step)\n";;

let queue_100 = make_queue 100;;
print_string "n=100\n"; qprint queue_100;;
print_string "n=99\n"; qprint (tail_queue 1 queue_100);;
print_string "n=98\n"; qprint (tail_queue 2 queue_100);;
print_string "n=97\n"; qprint (tail_queue 3 queue_100);;
print_string "n=96\n"; qprint (tail_queue 4 queue_100);;
print_string "n=95\n"; qprint (tail_queue 5 queue_100);;
print_string "n=94\n"; qprint (tail_queue 6 queue_100);;
print_string "n=93\n"; qprint (tail_queue 7 queue_100);;
print_string "n=92\n"; qprint (tail_queue 8 queue_100);;
print_string "n=91\n"; qprint (tail_queue 9 queue_100);;
print_string "n=90\n"; qprint (tail_queue 10 queue_100);;
