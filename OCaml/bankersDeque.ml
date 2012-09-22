open Item;;
open Ordered;;
open Rqueue;;
open SmallStream;;

module type CONSTNUM =
sig
  val c : int
end

module BankersDeque (Element : ITEM) (ConstNum : CONSTNUM) : RDEQUE
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  type queue = int * Elem.t S.stream * int * Elem.t S.stream

  exception Empty

  let c = ConstNum.c
  ;;

  let empty = (0, lazy S.Nil, 0, lazy S.Nil)
  ;;

  let isEmpty (lenf, f, lenr, r) = (lenf + lenr == 0)
  ;;

  let check ((lenf, f, lenr, r) as q) =
    if lenf > c * lenr + 1 then
      let i = (lenf + lenr) / 2 in
      let j = lenf + lenr - i in
      let f' = S.take (i, f) in
      let r' = S.(++) r (S.reverse (S.drop (i, f))) in
      (i, f', j, r')
    else if lenr > c * lenf + 1 then
      let j = (lenf + lenr) / 2 in
      let i = lenf + lenr - j in
      let r' = S.take (j, r) in
      let f' = S.(++) f (S.reverse (S.drop (j, r))) in
      (i, f', j, r')
    else q
  ;;

  let cons x (lenf, f, lenr, r) = check (lenf + 1, lazy (S.Cons (x, f)), lenr, r)
  ;;

  let head = function
    | (lenf, lazy S.Nil, lenr, lazy S.Nil) -> raise Empty
    | (lenf, lazy S.Nil, lenr, lazy (S.Cons (x, _))) -> x
    | (lenf, lazy (S.Cons (x, f')), lenr, r) -> x
  ;;

  let tail = function
    | (lenf, lazy S.Nil, lenr, lazy S.Nil) -> raise Empty
    | (lenf, lazy S.Nil, lenr, lazy (S.Cons (x, _))) -> empty
    | (lenf, lazy (S.Cons (x, f')), lenr, r) -> check (lenf - 1, f', lenr, r)
  ;;

  let reverse (lenf, f, lenr, r) = (lenr, r, lenf, f)
  ;;

  let snoc q x = reverse (cons x (reverse q))
  ;;

  let last q = head (reverse q)
  ;;

  let init q = reverse (tail (reverse q))
  ;;

  let dprint show (lenf, f, lenr, r) =
    let rec print_elem_stream s =
      let print_elem_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (x, xs))) ->
            print_string "Cons (";
            Elem.print x;
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
    print_int lenr;
    print_string ",\n\t";
    print_elem_stream r;
    print_string ")";
    print_newline ()
  ;;

  let print q = dprint false q
  ;;
end

module IntBankersDeque = BankersDeque (Int) (struct let c = 3 end) 
