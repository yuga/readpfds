open Item;;
open Ordered;;
open Rqueue;;
open BootStrappedQueue;;
open HoodMelvilleQueue;;
open RealTimeQueue;;

module type CATENABLELIST = sig
  type elt
  type t
  val empty   : t 
  val isEmpty : t -> bool
  val cons    : elt * t -> t
  val snoc    : t * elt -> t
  val (++)    : t -> t -> t
  val head    : t -> elt
  val tail    : t -> t
  val dprint  : bool -> t -> unit
  val print   : t -> unit
end

module CatenableList (Q : RQUEUEP) (Item : ITEM) : CATENABLELIST
  with type elt = Item.t =
struct
  type elt = Item.t
  type t = E
         | C of elt * t Lazy.t Q.q

  exception Empty

  let empty = E
  ;;

  let isEmpty = function
    | E -> true
    | _ -> false
  ;;

  let link (C (x, q), s) = C (x, Q.snoc (q, s))
  ;;
 
  let rec linkAll q =
    let lazy t = Q.head q in
    let q' = Q.tail q in
    if Q.isEmpty q' then t else link (t, lazy (linkAll q'))
  ;;

  let (++) xs ys = match (xs, ys) with
    | (E, ys) -> ys
    | (xs, E) -> xs
    | (xs, ys) -> link (xs, lazy ys)
  ;;

  let cons (x, xs) = C (x, Q.empty) ++ xs
  ;;

  let snoc (xs, x) = xs ++ C (x, Q.empty)
  ;;

  let head = function
    | E -> raise Empty
    | C (x, _) -> x
  ;;

  let tail = function
    | E -> raise Empty
    | C (x, q) -> if Q.isEmpty q then E else linkAll q
  ;;

  let dprint show cs =
    let rec print_lazy_t s =
      let print_lazy_t' = function
        | lazy e -> print_catenablelist e in
      if show || Lazy.lazy_is_val s
      then print_lazy_t' s
      else print_string "SUSP"
    and print_catenablelist = function
      | E -> print_string "E"
      | C (x, q) ->
          print_string "C (";
          Item.print x;
          print_string ", ";
          Q.print_queue print_lazy_t show q;
          print_string ")" in
    print_string "CatenableList (\n";
    print_catenablelist cs;
    print_string ")";
    print_newline()
  ;;

  let print cs = dprint false cs
  ;;
end

module IntBootStrappedCatenableList = CatenableList (BootStrappedQueueP) (Int)
module IntHoodMelvilleCatenableList = CatenableList (HoodMelvilleQueueP) (Int)
module IntRealTimeCatenableList = CatenableList (RealTimeQueueP) (Int)
