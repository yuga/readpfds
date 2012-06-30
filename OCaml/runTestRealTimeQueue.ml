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
let queue_1 = Q.snoc queue_0 1;;
let queue_2 = Q.snoc queue_1 2;;
let queue_3 = Q.snoc queue_2 3;;
let queue_4 = Q.snoc queue_3 4;;
let queue_5 = Q.snoc queue_4 5;;
let queue_6 = Q.snoc queue_5 6;;
let queue_7 = Q.snoc queue_6 7;;
let queue_8 = Q.snoc queue_7 8;;
let queue_9 = Q.snoc queue_8 9;;

Q.print queue_0;;
Q.print queue_1;;
Q.print queue_2;;
Q.print queue_3;;
Q.print queue_4;;
Q.print queue_5;;
Q.print queue_6;;
Q.print queue_7;;
Q.print queue_8;;
Q.print queue_9;;

