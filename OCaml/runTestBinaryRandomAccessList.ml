#use "topfind";;
#require "OUnit";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "smallStream.cmo";;
#load "randomAccessList.cmo";;
#load "binaryRandomAccessList.cmo";;
#load "zerolessBinaryRandomAccessList.cmo";;
#load "lazyBinaryRandomAccessList.cmo";;

open OUnit;;
open RandomAccessList;;
open BinaryRandomAccessList;;
open ZerolessBinaryRandomAccessList;;
open LazyBinaryRandomAccessList;;

module C (R : RANDOMACCESSLIST) = struct
  let make_rlist xs =
    List.fold_left (fun a x -> R.cons (x, a)) R.empty xs
  ;;
end


(* **************************************************************** *)

module B = IntBinaryRandomAccessList;;
module CB = C (B);;
module B1 = IntEx0901;;

let bRlist_0 = CB.make_rlist [];;
let bRlist_1 = CB.make_rlist [0];;
let bRlist_10 = CB.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9]);;

B.print bRlist_0;;
B.print bRlist_1;;
B.print bRlist_10;;

B.print (B1.drop (1, bRlist_10));;
B.print (B1.drop (2, bRlist_10));;
B.print (B1.drop (3, bRlist_10));;
B.print (B1.drop (4, bRlist_10));;
B.print (B1.drop (5, bRlist_10));;


(* **************************************************************** *)

module Z = IntZerolessBinaryRandomAccessList;;
module CZ = C (Z);;

let zRlist_0 = CZ.make_rlist [];;
let zRlist_1 = CZ.make_rlist [0];;
let zRlist_10 = CZ.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9]);;

Z.print zRlist_0;;
Z.print zRlist_1;;
Z.print zRlist_10;;


(* **************************************************************** *)

module L = IntLazyBinaryRandomAccessList;;
module CL = C (L);;

let lRlist_0 = CL.make_rlist [];;
let lRlist_1 = CL.make_rlist [0];;
let lRlist_10 = CL.make_rlist (List.rev [0;1;2;3;4;5;6;7;8;9]);;
let lRlist_10_3 = L.tail (L.tail (L.tail lRlist_10))

let lprint = L.dprint true;;

lprint lRlist_0;;
lprint lRlist_1;;
lprint lRlist_10;;
lprint lRlist_10_3;;
