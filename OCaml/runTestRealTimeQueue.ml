#use "topfind";;
#require "OUnit";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "rqueue.cmo";;
#load "realTimeQueue.cmo";;

open OUnit;;
open RealTimeQueue;;
module Q = IntQueue

let queue_0 = Q.empty;;

let queue_1_0 = Q.empty;;
let queue_1_1 = Q.snoc queue_1_0 1;;

let queue_2_0 = Q.empty;;
let queue_2_1 = Q.snoc queue_2_0 1;;
let queue_2_2 = Q.snoc queue_2_1 2;;

let queue_3_0 = Q.empty;;
let queue_3_1 = Q.snoc queue_3_0 1;;
let queue_3_2 = Q.snoc queue_3_1 2;;
let queue_3_3 = Q.snoc queue_3_2 3;;

let queue_4_0 = Q.empty;;
let queue_4_1 = Q.snoc queue_4_0 1;;
let queue_4_2 = Q.snoc queue_4_1 2;;
let queue_4_3 = Q.snoc queue_4_2 3;;
let queue_4_4 = Q.snoc queue_4_3 4;;

let queue_5_0 = Q.empty;;
let queue_5_1 = Q.snoc queue_5_0 1;;
let queue_5_2 = Q.snoc queue_5_1 2;;
let queue_5_3 = Q.snoc queue_5_2 3;;
let queue_5_4 = Q.snoc queue_5_3 4;;
let queue_5_5 = Q.snoc queue_5_4 5;;

let queue_6_0 = Q.empty;;
let queue_6_1 = Q.snoc queue_6_0 1;;
let queue_6_2 = Q.snoc queue_6_1 2;;
let queue_6_3 = Q.snoc queue_6_2 3;;
let queue_6_4 = Q.snoc queue_6_3 4;;
let queue_6_5 = Q.snoc queue_6_4 5;;
let queue_6_6 = Q.snoc queue_6_5 6;;

let queue_7_0 = Q.empty;;
let queue_7_1 = Q.snoc queue_7_0 1;;
let queue_7_2 = Q.snoc queue_7_1 2;;
let queue_7_3 = Q.snoc queue_7_2 3;;
let queue_7_4 = Q.snoc queue_7_3 4;;
let queue_7_5 = Q.snoc queue_7_4 5;;
let queue_7_6 = Q.snoc queue_7_5 6;;
let queue_7_7 = Q.snoc queue_7_6 7;;

let queue_8_0 = Q.empty;;
let queue_8_1 = Q.snoc queue_8_0 1;;
let queue_8_2 = Q.snoc queue_8_1 2;;
let queue_8_3 = Q.snoc queue_8_2 3;;
let queue_8_4 = Q.snoc queue_8_3 4;;
let queue_8_5 = Q.snoc queue_8_4 5;;
let queue_8_6 = Q.snoc queue_8_5 6;;
let queue_8_7 = Q.snoc queue_8_6 7;;
let queue_8_8 = Q.snoc queue_8_7 8;;

let queue_9_0 = Q.empty;;
let queue_9_1 = Q.snoc queue_9_0 1;;
let queue_9_2 = Q.snoc queue_9_1 2;;
let queue_9_3 = Q.snoc queue_9_2 3;;
let queue_9_4 = Q.snoc queue_9_3 4;;
let queue_9_5 = Q.snoc queue_9_4 5;;
let queue_9_6 = Q.snoc queue_9_5 6;;
let queue_9_7 = Q.snoc queue_9_6 7;;
let queue_9_8 = Q.snoc queue_9_7 8;;
let queue_9_9 = Q.snoc queue_9_8 9;;

Q.print queue_0;;
Q.print queue_1_1;;
Q.print queue_2_2;;
Q.print queue_3_3;;
Q.print queue_4_4;;
Q.print queue_5_5;;
Q.print queue_6_6;;
Q.print queue_7_7;;
Q.print queue_8_8;;
Q.print queue_9_9;;

