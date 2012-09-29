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

module E = IntEx0909;;
module CE = C (E);;

let rlist_0 = CE.make_rlist [];;
let rlist_1 = CE.make_rlist [0];;
let rlist_10 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9]);;
let rlist_11 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10]);;
let rlist_12 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11]);;
let rlist_13 = CE.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9;10;11;12]);;
let rlist_10_2 = E.tail (E.tail rlist_10)
let rlist_10_3 = E.tail (E.tail (E.tail rlist_10))
let rlist_10_4 = E.tail (E.tail (E.tail (E.tail rlist_10)))

let print = E.dprint true;;

print rlist_0;;
print rlist_1;;
print rlist_10;;
print rlist_11;;
print rlist_12;;
print rlist_13;;
print rlist_10_2;;
print rlist_10_3;;
print rlist_10_4;;
