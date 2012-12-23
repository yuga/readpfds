open Heap;;
open Ordered;;

module BootStrappedHeap
(*
= functor (MakeH : functor (Element : ORDERED) -> HEAP with module Elem = Element)
-> functor (Element : ORDERED)
-> struct
*)
(MakeH : functor (Element : ORDERED) -> HEAP with module Elem = Element)
(Element : ORDERED)
: (HEAP with module Elem = Element)
= struct
  module Elem = Element
  exception Empty

  module rec BootStrappedElem : sig
    type t = E
           | H of Elem.t * PrimH.heap
    include ORDERED with type t := t
  end = struct
    type t = E
           | H of Elem.t * PrimH.heap
    
    let eq = function
      | E, E -> true
      | (H (x, _), (H (y, _))) -> Elem.eq (x, y)
      | _, _ -> false

    let lt = function
      | (H (x, _), (H (y, _))) -> Elem.lt (x, y)
      | _ -> raise Empty

    let leq = function
      | E, E -> true
      | (H (x, _), (H (y, _))) -> Elem.leq (x, y)
      | _ -> raise Empty

    let print : t -> unit = function
      | E -> print_string "E"
      | H (x, h) ->
          print_string "H (";
          Elem.print x;
          print_string ", ";
          PrimH.print h;
          print_string ")"
  end
  and PrimH : HEAP with module Elem := BootStrappedElem = MakeH (BootStrappedElem);;

  include BootStrappedElem

  type heap = t

  let empty = E

  let isEmpty = function
    | E -> true
    | _ -> false

  let merge = function
    | (E, h) -> h
    | (h, E) -> h
    | ((H (x, p1) as h1), (H (y, p2) as h2)) ->
        if Elem.leq (x, y) then
          H (x, PrimH.insert (h2, p1))
        else
          H (y, PrimH.insert (h1, p2))

  let insert (x, h) = merge (H (x, PrimH.empty), h)

  let findMin = function 
    | E -> raise Empty
    | H (x, _) -> x

  let deleteMin = function
    | E -> raise Empty
    | H (x, p) ->
        if PrimH.isEmpty p then
          E
        else
          let H (y, p1) = PrimH.findMin p in
          let p2 = PrimH.deleteMin p in
          H (y, PrimH.merge (p1, p2))

  let print = function
    | E ->
        print_string "E";
        print_newline()
    | H (x, p) ->
        print_string "H (";
        Elem.print x;
        print_string ",\n";
        PrimH.print p;
        print_string ")";
        print_newline()
end

open ScheduledBinomialHeap;;
module IntBootStrappedHeap : HEAP with module Elem = Int = BootStrappedHeap (ScheduledBinomialHeap) (Int)
