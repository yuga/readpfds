open Item;;
open Ordered;;
open Rqueue;;
open SmallStream;;

module type CONSTNUM =
sig
  val c : int
end

module RealTimeDeque (Item : ITEM) (ConstNum : CONSTNUM) : RDEQUE
  with type elt = Item.t =
struct
  type elt = Item.t
  module S = SmallStream

  type t = int * elt S.stream * elt S.stream
         * int * elt S.stream * elt S.stream
  
  exception Empty

  let c = ConstNum.c
  ;;

  let empty = (0, lazy S.Nil, lazy S.Nil, 0, lazy S.Nil, lazy S.Nil)
  ;;

  let isEmpty (lenf, f, sf, lenr, r, sr) = (lenf + lenr == 0)
  ;;

  let exec1 = function
    | (lazy (S.Cons (x, s))) -> s
    | s -> s
  ;;

  let exec2 s = exec1 (exec1 s)
  ;;

  let rec rotateRev = function
    | (lazy S.Nil, r, a) -> 
        S.(++) (S.reverse r) a
    | (lazy (S.Cons (x, f)), r, a) ->
        lazy (S.Cons (x, rotateRev (f, S.drop (c, r), S.(++) (S.reverse (S.take (c, r))) a)))
  ;;

  let rec rotateDrop (f, j, r) =
    if j < c then
      rotateRev (f, S.drop (j, r), lazy S.Nil)
    else
      let (lazy (S.Cons (x, f'))) = f in
      lazy (S.Cons (x, rotateDrop (f', j - c, S.drop (c, r))))
  ;;

  let check ((lenf, f, sf, lenr, r, sr) as q) =
    if lenf > c * lenr + 1 then
      let i = (lenf + lenr) / 2 in
      let j = lenf + lenr - i in
      let f' = S.take (i, f) in
      let r' = rotateDrop (r, i, f) in
      (i, f', f', j, r', r')
    else if lenr > c * lenf + 1 then
      let j = (lenf + lenr) / 2 in
      let i = lenf + lenr - j in
      let r' = S.take (j, r) in
      let f' = rotateDrop (f, j, r) in
      (i, f', f', j, r', r')
    else q
  ;;

  let cons (x, (lenf, f, sf, lenr, r, sr)) =
    check (lenf + 1, lazy (S.Cons (x, f)), exec1 sf, lenr, r, exec1 sr)
  ;;

  let head = function
    | (lenf, lazy S.Nil, sf, lenr, lazy S.Nil, sr) -> raise Empty
    | (lenf, lazy S.Nil, sf, lenr, lazy (S.Cons (x, _)), sr) -> x
    | (lenf, lazy (S.Cons (x, f')), sf, lenr, r, sr) -> x
  ;;

  let tail = function
    | (lenf, lazy S.Nil, sf, lenr, lazy S.Nil, sr) -> raise Empty
    | (lenf, lazy S.Nil, sf, lenr, lazy (S.Cons (x, _)), sr) -> empty
    | (lenf, lazy (S.Cons (x, f')), sf, lenr, r, sr) ->
        check (lenf - 1, f', exec2 sf, lenr, r, exec2 sr)
  ;;

  let reverse (lenf, f, sf, lenr, r, sr) = (lenr, r, sr, lenf, f, sf)
  ;;

  let snoc (q, x) = reverse (cons (x, (reverse q)))
  ;;

  let last q = head (reverse q)
  ;;

  let init q = reverse (tail (reverse q))
  ;;

  let dprint show (lenf, f, sf, lenr, r, sr) =
    let rec print_elem_stream s =
      let print_elem_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (x, xs))) ->
            print_string "Cons (";
            Item.print x;
            print_string ", ";
            print_elem_stream xs;
            print_string ")" in
      if show || Lazy.lazy_is_val s
      then print_elem_stream_val s
      else print_string "SUSP" in
    print_string "queue\n\t(";
    print_int lenf;
    print_string ",\n\t";
    print_elem_stream f;
    print_string ",\n\t";
    print_elem_stream sf;
    print_string ",\n\t";
    print_int lenr;
    print_string ",\n\t";
    print_elem_stream r;
    print_string ",\n\t";
    print_elem_stream sr;
    print_string ")";
    print_newline ()
  ;;
  
  let print q = dprint false q
  ;; 
end

module IntRealTimeDeque = RealTimeDeque (Int) (struct let c = 3 end) 
