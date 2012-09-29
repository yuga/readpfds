#use "topfind";;
#require "OUnit";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "smallStream.cmo";;
#load "randomAccessList.cmo";;
#load "lazyBinaryRandomAccessList.cmo";;

open OUnit;;
open RandomAccessList;;
open LazyBinaryRandomAccessList;;

module C (R : RANDOMACCESSLIST) = struct
  let make_rlist xs =
    List.fold_left (fun a x -> R.cons (x, a)) R.empty xs
  ;;
end


(* **************************************************************** *)

module E = IntEx0910;;
module CE = C (E);;

let rlist_0 = CE.make_rlist [];;
let rlist_1 = CE.make_rlist [0];;
let rlist_2 = CE.make_rlist [0;1];;
let rlist_3 = CE.make_rlist [0;1;2];;
let rlist_4 = CE.make_rlist [0;1;2;3];;
let rlist_5 = CE.make_rlist [0;1;2;3;4];;
let rlist_6 = CE.make_rlist [0;1;2;3;4;5];;
let rlist_7 = CE.make_rlist [0;1;2;3;4;5;6];;
let rlist_8 = CE.make_rlist [0;1;2;3;4;5;6;7];;
let rlist_9 = CE.make_rlist [0;1;2;3;4;5;6;7;8];;
let rlist_10 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9]);;
let rlist_11 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10]);;
let rlist_12 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11]);;
let rlist_13 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11;12]);;
let rlist_14 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11;12;13]);;
let rlist_15 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14]);;
let rlist_16 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]);;
let rlist_10_2 = E.tail (E.tail rlist_10)
let rlist_10_3 = E.tail (E.tail (E.tail rlist_10))
let rlist_10_4 = E.tail (E.tail (E.tail (E.tail rlist_10)))

let print = E.dprint false;;

print rlist_0;;
print rlist_1;;
print rlist_2;;
print rlist_3;;
print rlist_4;;
print rlist_5;;
print rlist_6;;
print rlist_7;;
print rlist_8;;
print rlist_9;;
print rlist_10;;
print rlist_11;;
print rlist_12;;
print rlist_13;;
print rlist_14;;
print rlist_15;;
print rlist_16;;
print rlist_10_2;;
print rlist_10_3;;
print rlist_10_4;;
