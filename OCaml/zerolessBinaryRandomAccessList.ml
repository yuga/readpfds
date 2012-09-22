open Item;;
open Ordered;;
open RandomAccessList;;

module ZerolessBinaryRandomAccessList (Element : ITEM) : RANDOMACCESSLIST
  with module Elem = Element =
struct
  module Elem = Element

  type 'a tree = LEAF of 'a
               | NODE of int * 'a tree * 'a tree
  type 'a digit = ONE of 'a tree
                | TWO of 'a tree * 'a tree
  type rlist = Elem.t digit list

  exception Empty
  exception Subscript

  let empty = []
  ;;

  let isEmpty ts = ts = []
  ;;

  let size = function
    | (LEAF x) -> 1
    | (NODE (w, t1, t2)) -> w
  ;;

  let link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  ;;

  let rec consTree = function
    | (t, []) -> [ONE t]
    | (t1, ONE t2 :: ts) -> TWO (t1, t2) :: ts
    | (t1, TWO (t2, t3) :: ts) -> ONE t1 :: consTree (link (t2, t2), ts)
  ;;

  let rec unconsTree = function
    | [] -> raise Empty
    | [ONE t] -> (t, [])
    | (ONE t1 :: ts) ->
        let (NODE (_, t2, t3), ts') = unconsTree ts in
        (t1, TWO (t2, t3) :: ts')
    | (TWO (t1, t2) :: ts) -> (t1, ONE t2 :: ts)
  ;;

  let cons (x, ts) = consTree (LEAF x, ts)
  ;;

  let head = function
    | (ONE (LEAF x) :: _) -> x
    | (TWO (LEAF x, LEAF y) :: _) -> x
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
    | (i, ONE t :: ts) ->
        if i < size t then lookupTree (i, t)
        else lookup (i - size t, ts)
    | (i, TWO (t1, t2) :: ts) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else lookup (i - size t1 - size t2, ts)
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
    | (i, y, ONE t :: ts) ->
        if i < size t then ONE (updateTree (i, y, t)) :: ts
        else ONE t :: update (i - size t, y, ts)
    | (i, y, TWO (t1, t2) :: ts) ->
        if i < size t1 then TWO (updateTree (i, y, t1), t2) :: ts
        else if i < size t1 + size t2 then TWO (t2, updateTree (i - size t1, y, t2)) :: ts
        else TWO (t1, t2) :: update (i - size t1 - size t2, y, ts)
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
      | (ONE t) ->
          print_string "ONE (";
          print_tree t;
          print_string ")"
      | (TWO (t1, t2)) ->
          print_string "TWO (";
          print_tree t1;
          print_string ", ";
          print_tree t2;
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

module IntZerolessBinaryRandomAccessList = ZerolessBinaryRandomAccessList (Int)
