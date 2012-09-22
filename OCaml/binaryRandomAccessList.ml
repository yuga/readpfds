open Item;;
open Ordered;;
open RandomAccessList;;

module BinaryRandomAccessListData (Element : ITEM) = struct
  type 'a tree = LEAF of 'a
               | NODE of int * 'a tree * 'a tree
  type 'a digit = ZERO
                | ONE of 'a tree
  type rlist = Element.t digit list
  
  let size = function
    | (LEAF x) -> 1
    | (NODE (w, t1, t2)) -> w
  ;;
end

module BinaryRandomAccessList (Element : ITEM) : sig
  include module type of BinaryRandomAccessListData(Element)
  include RANDOMACCESSLIST with module Elem = Element
                            and type rlist := rlist
end = struct
  include BinaryRandomAccessListData(Element)
  module Elem = Element

  exception Empty
  exception Subscript

  let empty = []
  ;;

  let isEmpty ts = ts = []
  ;;

  let link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  ;;

  let rec consTree = function
    | (t, []) -> [ONE t]
    | (t, ZERO :: ts) -> ONE t :: ts
    | (t1, ONE t2 :: ts) -> ZERO :: consTree (link (t1, t2), ts)
  ;;

  let rec unconsTree = function
    | [] -> raise Empty
    | [ONE t] -> (t, [])
    | (ONE t :: ts) -> (t, ZERO :: ts)
    | (ZERO :: ts) ->
        let (NODE (_, t1, t2), ts') = unconsTree ts in
        (t1, ONE t2 :: ts')
  ;;

  let cons (x, ts) = consTree (LEAF x, ts)
  ;;

  let head ts =
    let (LEAF x, _) = unconsTree ts in
    x
  ;;

  let tail ts =
    let (_, ts') = unconsTree ts in
    ts'
  ;;

  let rec lookupTree = function
    | (0, LEAF x) -> x
    | (i, LEAF x) -> raise Subscript
    | (i, NODE (w, t1, t2)) ->
        if i < (w/2) then lookupTree (i, t1)
        else lookupTree (i - (w/2), t2)
  ;;

  let rec lookup = function
    | (i, []) -> raise Subscript
    | (i, ZERO :: ts) -> lookup (i, ts)
    | (i, ONE t :: ts) ->
        if i < size t then lookupTree (i, t)
        else lookup (i - size t, ts)
  ;;

  let rec updateTree = function
    | (0, y, LEAF x) -> LEAF y
    | (i, y, LEAF x) -> raise Subscript
    | (i, y, NODE (w, t1, t2)) ->
        if i < (w/2) then NODE (w, updateTree (i, y, t1), t2)
        else NODE (w, t1, updateTree (i - (w/2), y, t2))
  ;;

  let rec update = function
    | (i, y, []) -> raise Subscript
    | (i, y, ZERO :: ts) -> ZERO :: update (i, y, ts)
    | (i, y, ONE t :: ts) ->
        if i < size t then ONE (updateTree (i, y, t)) :: ts
        else ONE t :: update (i - size t, y, ts)
  ;;

  let print xs =
    let rec print_tree = function
      | (LEAF x) ->
          print_string "LEAF (";
          Elem.print x;
          print_string ")"
      | (NODE (w, t1, t2)) ->
          print_string "NODE (";
          print_int w;
          print_string ", ";
          print_tree t1;
          print_string ", ";
          print_tree t2;
          print_string ")" in
    let print_digit = function
      | (ZERO) -> print_string "ZERO"
      | (ONE t) ->
          print_string "ONE (";
          print_tree t;
          print_string ")" in
    let rec print_digit_list = function
      | [] -> ()
      | (x :: xs) ->
          print_digit x;
          print_string ";\n";
          print_digit_list xs in
    print_string "rlist ([\n";
    print_digit_list xs;
    print_string "])";
    print_newline ()
  ;;

  let dprint _ xs = print xs
  ;;
end

module Ex0901 (Element : ITEM) : sig
  include module type of BinaryRandomAccessList(Element)
  val drop : int * rlist -> rlist
end = struct
  include BinaryRandomAccessList(Element)

  let rec fillZero : Elem.t tree * rlist -> rlist = function
    | (LEAF x, a) -> a
    | (NODE (w, t1, t2), a) -> fillZero (t1, ZERO :: a)
  ;;
  
  let rec dropTreeLeft : int * Elem.t tree * rlist -> rlist = function
    | (0, t, a) -> fillZero (t, ONE t :: a)
    | (i, LEAF x, a) -> raise Subscript
    | (i, NODE (w, t1, t2), a) ->
        if i < (w/2)
        then dropTreeLeft (i, t1, ONE t2 :: a)
        else dropTreeRight (i - (w/2), t1, a)
  and dropTreeRight  : int * Elem.t tree * rlist -> rlist = function
    | (0, t, a) -> fillZero (t, ONE t :: a)
    | (i, LEAF x, a) -> raise Subscript
    | (i, NODE (w, t1, t2), a) ->
        match a with
          | [] ->
              if i < (w/2)
              then dropTreeLeft (i, t1, ONE t2 :: a)
              else dropTreeRight (i - (w/2), t2, a)
          | a ->
              if i < (w/2)
              then dropTreeLeft (i, t1, ONE t2 :: ZERO :: a)
              else dropTreeRight (i - (w/2), t2, ZERO :: a)
  ;;

  let rec drop : int * rlist -> rlist = function
    | (i, []) -> []
    | (i, ZERO :: ts) -> drop (i, ts)
    | (i, ONE t :: ts) ->
        if i < size t
        then dropTreeRight (i, t, ts)
        else drop (i - size t, ts)
  ;;
end

module IntBinaryRandomAccessList = BinaryRandomAccessList (Int)
module IntEx0901 = Ex0901 (Int)

