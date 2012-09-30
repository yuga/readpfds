open Item;;
open Ordered;;
open SmallStream;;
open RandomAccessList;;

module LazyBinaryRandomAccessList (Element : ITEM) : RANDOMACCESSLIST
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  exception Empty
  exception Subscript

  type 'a tree = LEAF of 'a
               | NODE of int * 'a tree * 'a tree
  type 'a digit = ZERO
                | ONE of 'a tree
                | TWO of 'a tree * 'a tree
  type rlist = Elem.t digit S.stream

  let empty = lazy S.Nil
  ;;

  let isEmpty = function
    | (lazy S.Nil) -> true
    | _ -> false
  ;;

  let size = function
    | (LEAF x) -> 1
    | (NODE (w, t1, t2)) -> w
  ;;

  let link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  ;;

  let rec consTree = function
    | (t, lazy S.Nil) -> lazy (S.Cons (ONE t, lazy S.Nil))
    | (t1, lazy (S.Cons (TWO (t2, t3), ts))) -> lazy (S.Cons (ONE t1, consTree (link (t2, t3), ts)))
    | (t1, lazy (S.Cons (ONE t2, ts))) -> lazy (S.Cons (TWO (t1, t2), ts))
    | (t, lazy (S.Cons (ZERO, ts))) -> lazy (S.Cons (ONE t,  ts))
  ;;

  let rec unconsTree = function
    | (lazy S.Nil) -> raise Empty
    | (lazy (S.Cons (TWO (t1, t2), ts))) -> (t1, lazy (S.Cons (ONE t2, ts)))
    | (lazy (S.Cons (ONE t, lazy S.Nil))) -> (t, lazy S.Nil)
    | (lazy (S.Cons (ONE t, ts))) -> (t, (lazy (S.Cons (ZERO, ts))))
    | (lazy (S.Cons (ZERO, ts))) ->
        let (NODE (_, t1, t2), ts') = unconsTree ts in
        (t1, lazy (S.Cons (ONE t2, ts')))
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
    | (i, lazy S.Nil) -> raise Subscript
    | (i, lazy (S.Cons (ZERO, ts))) -> lookup (i, ts)
    | (i, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lookupTree (i, t)
        else lookup (i - size t, ts)
    | (i, lazy (S.Cons (TWO (t1, t2), ts))) ->
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
    | (i, y, lazy S.Nil) -> raise Subscript
    | (i, y, lazy (S.Cons (ZERO, ts))) -> lazy (S.Cons (ZERO, update (i, y, ts)))
    | (i, y, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lazy (S.Cons (ONE (updateTree (i, y, t)), ts))
        else lazy (S.Cons (ONE t, update (i - size t, y, ts)))
    | (i, y, lazy (S.Cons (TWO (t1, t2), ts))) ->
        if i < size t1 then lazy (S.Cons (TWO (updateTree (i, y, t1), t2), ts))
        else if i < size t1 + size t2 then lazy (S.Cons (TWO (t1, updateTree (i - size t1, y, t2)), ts))
        else lazy (S.Cons (TWO (t1, t2), update (i - size t1 - size t2, y, ts)))
  ;;

  let dprint show xs =
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
          print_string ")"
      | (TWO (t1, t2)) ->
          print_string "TWO (";
          print_tree t1;
          print_string ", ";
          print_tree t2;
          print_string ")" in
    let rec print_digit_stream s =
      let print_digit_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (d, ds))) ->
            print_string "Cons (";
            print_digit d;
            print_string ",\n";
            print_digit_stream ds;
            print_string ")" in
      if show || Lazy.lazy_is_val s
      then print_digit_stream_val s
      else print_string "SUSP" in
    print_string "rlist (\n";
    print_digit_stream xs;
    print_string ")";
    print_newline ()
  ;;

  let print xs = dprint false xs
  ;;
end

module Ex0909 (Element : ITEM) : RANDOMACCESSLIST
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  exception Empty
  exception Subscript

  type 'a tree = LEAF of 'a
               | NODE of int * 'a tree * 'a tree
  type 'a digit = ONE of 'a tree
                | TWO of 'a tree * 'a tree
                | THREE of 'a tree * 'a tree * 'a tree
  type rlist = Elem.t digit S.stream

  let empty = lazy S.Nil
  ;;

  let isEmpty = function
    | (lazy S.Nil) -> true
    | _ -> false
  ;;

  let size = function
    | (LEAF x) -> 1
    | (NODE (w, t1, t2)) -> w
  ;;

  let link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  ;;

  let rec consTree = function
    | (t, lazy S.Nil) -> lazy (S.Cons (ONE t, lazy S.Nil))
    | (t1, lazy (S.Cons (THREE (t2, t3, t4), ts))) -> lazy (S.Cons (TWO (t1, t2), consTree (link (t3, t4), ts)))
    | (t1, lazy (S.Cons (TWO (t2, t3), ts))) -> lazy (S.Cons (THREE (t1, t2, t3), ts))
    | (t1, lazy (S.Cons (ONE t2, ts))) -> lazy (S.Cons (TWO (t1, t2), ts))
  ;;

  let rec unconsTree = function
    | (lazy S.Nil) -> raise Empty
    | (lazy (S.Cons (THREE (t1, t2, t3), ts))) -> (t1, lazy (S.Cons (TWO (t2, t3), ts)))
    | (lazy (S.Cons (TWO (t1, t2), ts))) -> (t1, lazy (S.Cons (ONE t2, ts)))
    | (lazy (S.Cons (ONE t, lazy S.Nil))) -> (t, lazy S.Nil)
    | (lazy (S.Cons (ONE t1, ts))) ->
        let (NODE (_, t2, t3), ts') = unconsTree ts in
        (t1, lazy (S.Cons (TWO (t2, t3), ts')))
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
    | (i, lazy S.Nil) -> raise Subscript
    | (i, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lookupTree (i, t)
        else lookup (i - size t, ts)
    | (i, lazy (S.Cons (TWO (t1, t2), ts))) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else lookup (i - size t1 - size t2, ts)
    | (i, lazy (S.Cons (THREE (t1, t2, t3), ts))) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else if i < size t1 + size t2 + size t3 then lookupTree (i - size t1 - size t2, t3)
        else lookup (i - size t1 - size t2 - size t3, ts)
  ;;

  let rec updateTree = function
    | (0, y, LEAF x) -> LEAF y
    | (i, y, LEAF x) -> raise Subscript
    | (i, y, NODE (w, t1, t2)) ->
        if i < (w/2) then NODE (w, updateTree (i, y, t1), t2)
        else NODE (w, t1, updateTree (i - (w/2), y, t2))
  ;;

  let rec update = function
    | (i, y, lazy S.Nil) -> raise Subscript
    | (i, y, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lazy (S.Cons (ONE (updateTree (i, y, t)), ts))
        else lazy (S.Cons (ONE t, update (i - size t, y, ts)))
    | (i, y, lazy (S.Cons (TWO (t1, t2), ts))) ->
        if i < size t1 then lazy (S.Cons (TWO (updateTree (i, y, t1), t2), ts))
        else if i < size t1 then lazy (S.Cons (TWO (t1, updateTree (i - size t1, y, t2)), ts))
        else lazy (S.Cons (TWO (t1, t2), update (i - size t1 - size t2, y, ts)))
    | (i, y, lazy (S.Cons (THREE (t1, t2, t3), ts))) ->
        if i < size t1 then lazy (S.Cons (THREE (updateTree (i, y, t1), t2, t3), ts))
        else if i < size t1 + size t2 then lazy (S.Cons (THREE (t1, updateTree (i - size t1, y, t2), t3), ts))
        else if i < size t1 + size t2 + size t3 then lazy (S.Cons (THREE (t1, t2, updateTree (i - size t1 - size t2, y, t3)), ts))
        else lazy (S.Cons (THREE (t1, t2, t3), update (i - size t1 - size t2 - size t3, y, ts)))
  ;;

  let dprint show xs =
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
          print_string ")"
      | (THREE (t1, t2, t3)) ->
          print_string "THREE (";
          print_tree t1;
          print_string ", ";
          print_tree t2;
          print_string ", ";
          print_tree t3;
          print_string ")" in
    let rec print_digit_stream s =
      let print_digit_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (d, ds))) ->
            print_string "Cons (";
            print_digit d;
            print_string ",\n";
            print_digit_stream ds;
            print_string ")" in
      if show || Lazy.lazy_is_val s
      then print_digit_stream_val s
      else print_string "SUSP" in
    print_string "rlist (\n";
    print_digit_stream xs;
    print_string ")";
    print_newline ()
  ;;

  let print xs = dprint false xs
  ;;
end

module Ex0910 (Element : ITEM) : RANDOMACCESSLIST
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  exception Empty
  exception Subscript

  type 'a tree = LEAF of 'a
               | NODE of int * 'a tree * 'a tree
  type 'a digit = ONE of 'a tree
                | TWO of 'a tree * 'a tree
                | TWOR of 'a tree * 'a tree
                | THREE of 'a tree * 'a tree * 'a tree
  type schedule = Elem.t digit S.stream list
  type rlist = Elem.t digit S.stream * schedule

  let empty = (lazy S.Nil, [])
  ;;

  let isEmpty = function
    | (lazy S.Nil, _) -> true
    | _ -> false
  ;;

  let size = function
    | (LEAF x) -> 1
    | (NODE (w, t1, t2)) -> w
  ;;

  let link (t1, t2) = NODE (size t1 + size t2, t1, t2)
  ;;

  let exec = function
    | [] -> []
    | (lazy (S.Cons (TWOR (t1, t2), ts)) :: sched) -> ts :: sched
    | (_ :: sched) -> sched
  ;;

  let rec consTree = function
    | (t,  lazy S.Nil)                             -> lazy (S.Cons (ONE t, lazy S.Nil))
    | (t1, lazy (S.Cons (ONE t2, ts)))             -> lazy (S.Cons (TWO (t1, t2), ts))
    | (t1, lazy (S.Cons (TWO (t2, t3), ts)))       -> lazy (S.Cons (THREE (t1, t2, t3), ts))
    | (t1, lazy (S.Cons (TWOR (t2, t3), ts)))      -> lazy (S.Cons (THREE (t1, t2, t3), ts))
    | (t1, lazy (S.Cons (THREE (t2, t3, t4), ts))) -> lazy (S.Cons (TWOR (t1, t2), consTree (link (t3, t4), ts)))
  ;;

  let rec unconsTree = function
    | (lazy S.Nil) -> raise Empty
    | (lazy (S.Cons (THREE (t1, t2, t3), ts))) -> (t1, lazy (S.Cons (TWO (t2, t3), ts)))
    | (lazy (S.Cons (TWOR (t1, t2), ts)))      -> (t1, lazy (S.Cons (ONE t2, ts)))
    | (lazy (S.Cons (TWO (t1, t2), ts)))       -> (t1, lazy (S.Cons (ONE t2, ts)))
    | (lazy (S.Cons (ONE t, lazy S.Nil)))      -> (t, lazy S.Nil)
    | (lazy (S.Cons (ONE t1, ts)))             ->
        let (NODE (_, t2, t3), ts') = unconsTree ts in
        (t1, lazy (S.Cons (TWOR (t2, t3), ts')))
  ;;

  let cons (x, (ts, sched)) =
    let ts' = consTree (LEAF x, ts) in
    (ts', exec (exec (ts' :: sched)))
  ;;

  let head (ts, sched) = match ts with
    | lazy S.Nil -> raise Empty
    | lazy (S.Cons(ONE (LEAF x), _)) -> x
    | lazy (S.Cons(TWO (LEAF x, _), _)) -> x
    | lazy (S.Cons(TWOR (LEAF x, _), _)) -> x
    | lazy (S.Cons(THREE (LEAF x, _, _), _)) -> x
    | _ -> raise Subscript
  ;;

  let tail (ts, sched) =
    let (_, ts') = unconsTree ts in
    (ts', exec (exec (ts' :: sched)))
  ;;

  let rec lookupTree = function
    | (0, LEAF x) -> x
    | (i, LEAF x) -> raise Subscript
    | (i, NODE (w, t1, t2)) ->
        if i < (w/2) then lookupTree (i, t1)
        else lookupTree (i - (w/2), t2)
  ;;

  let rec lookup' = function
    | (i, (lazy S.Nil)) -> raise Subscript
    | (i, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lookupTree (i, t)
        else lookup' (i - size t, ts)
    | (i, lazy (S.Cons (TWO (t1, t2), ts))) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else lookup' (i - size t1 - size t2, ts)
    | (i, lazy (S.Cons (TWOR (t1, t2), ts))) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else lookup' (i - size t1 - size t2, ts)
    | (i, lazy (S.Cons (THREE (t1, t2, t3), ts))) ->
        if i < size t1 then lookupTree (i, t1)
        else if i < size t1 + size t2 then lookupTree (i - size t1, t2)
        else if i < size t1 + size t2 + size t3 then lookupTree (i - size t1 - size t2, t3)
        else lookup' (i - size t1 - size t2 - size t3, ts)
  ;;

  let lookup (i, (ts, sched))  = lookup' (i, ts)
  ;;

  let rec updateTree = function
    | (0, y, LEAF x) -> LEAF y
    | (i, y, LEAF x) -> raise Subscript
    | (i, y, NODE (w, t1, t2)) ->
        if i < (w/2) then NODE (w, updateTree (i, y, t1), t2)
        else NODE (w, t1, updateTree (i - (w/2), y, t2))
  ;;

  let rec update' = function
    | (i, y, lazy S.Nil) -> raise Subscript
    | (i, y, lazy (S.Cons (ONE t, ts))) ->
        if i < size t then lazy (S.Cons (ONE (updateTree (i, y, t)), ts))
        else lazy (S.Cons (ONE t, update' (i - size t, y, ts)))
    | (i, y, lazy (S.Cons (TWO (t1, t2), ts))) ->
        if i < size t1 then lazy (S.Cons (TWO (updateTree (i, y, t1), t2), ts))
        else if i < size t1 + size t2 then lazy (S.Cons (TWO (t1, updateTree (i - size t1, y, t2)), ts))
        else lazy (S.Cons (TWO (t1, t2), update' (i - size t1 - size t2, y, ts)))
    | (i, y, lazy (S.Cons (TWOR (t1, t2), ts))) ->
        if i < size t1 then lazy (S.Cons (TWOR (updateTree (i, y, t1), t2), ts))
        else if i < size t1 + size t2 then lazy (S.Cons (TWOR (t1, updateTree (i - size t1, y, t2)), ts))
        else lazy (S.Cons (TWOR (t1, t2), update' (i - size t1 - size t2, y, ts)))
    | (i, y, lazy (S.Cons (THREE (t1, t2, t3), ts))) ->
        if i < size t1 then lazy (S.Cons (THREE (updateTree (i, y, t1), t2, t3), ts))
        else if i < size t1 + size t2 then lazy (S.Cons (THREE (t1, updateTree (i - size t1, y, t2), t3), ts))
        else if i < size t1 + size t2 + size t3 then lazy (S.Cons (THREE (t1, t2, updateTree (i - size t1 - size t2, y, t3)), ts))
        else lazy (S.Cons (THREE (t1, t2, t3), update' (i - size t1 - size t2 - size t3, y, ts)))
  ;;

  let update (i, y, (ts, sched)) = (update' (i, y, ts), sched)
  ;;

  let dprint show (xs, ys) =
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
          print_string ")"
      | (TWOR (t1, t2)) ->
          print_string "TWOR (";
          print_tree t1;
          print_string ", ";
          print_tree t2;
          print_string ")"
      | (THREE (t1, t2, t3)) ->
          print_string "THREE (";
          print_tree t1;
          print_string ", ";
          print_tree t2;
          print_string ", ";
          print_tree t3;
          print_string ")" in
    let rec print_digit_stream s =
      let print_digit_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (d, ds))) ->
            print_string "Cons (";
            print_digit d;
            print_string ",\n";
            print_digit_stream ds;
            print_string ")" in
      if show || Lazy.lazy_is_val s
      then print_digit_stream_val s
      else print_string "SUSP" in
    let rec print_schedule = function
      | [] -> ();
      | (s :: ss) ->
          print_digit_stream s;
          print_string ";\n";
          print_schedule ss in
    print_string "rlist (\n";
    print_string "digits (\n";
    print_digit_stream xs;
    print_string "),\nsched [\n";
    print_schedule ys;
    print_string "])";
    print_newline ()
  ;;

  let print xs = dprint false xs
  ;;
end

module IntLazyBinaryRandomAccessList = LazyBinaryRandomAccessList (Int)
module IntEx0909 = Ex0909 (Int)
module IntEx0910 = Ex0910 (Int)
