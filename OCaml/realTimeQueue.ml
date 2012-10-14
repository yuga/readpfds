open Item;;
open Ordered;;
open Rqueue;;
open SmallStream;;
open Printf;;

module RealTimeQueueP : sig
  include RQUEUEP
end = struct
  module S = SmallStream

  (* definition of RQUEUE.t 
   * (front, rear, schedule) *)
  type 'a q = 'a S.stream * 'a list * 'a S.stream

  (* exception *)
  exception Empty

  (* funcitons *)
  let empty = (lazy S.Nil, [], lazy S.Nil);;

  let isEmpty = function
    | (lazy S.Nil, _, _) -> true
    | _ -> false
  ;;

  let rec rotate : 'a q -> 'a S.stream = function
    | (lazy S.Nil, y :: _, a) -> lazy (S.Cons (y, a))
    | (lazy (S.Cons (x, xs)), y :: ys, a) ->
        lazy (S.Cons (x, rotate (xs, ys, lazy (S.Cons (y, a)))))
  ;;

  let exec : 'a q -> 'a q = function
    | (f, r, lazy (S.Cons (x, s))) -> (f, r, s)
    | (f, r, lazy S.Nil) -> let f' = rotate (f, r, lazy S.Nil)
                            in (f', [], f')
  ;;

  let snoc ((f, r, s), x) = exec (f, x :: r, s);;

  let head = function
    | (lazy S.Nil, r, s) -> raise Empty
    | (lazy (S.Cons (x, f)), r, s) -> x
  ;;

  let tail = function
    | (lazy S.Nil, r, s) -> raise Empty
    | (lazy (S.Cons (x, f)), r, s) -> exec (f, r, s)
  ;;

  let print_queue print show (f, r, s) =
    let rec print_elem_stream s =
      let print_elem_stream_val = function
        | (lazy S.Nil) -> print_string "Nil"
        | (lazy (S.Cons (x, xs))) ->
            print_string "Cons (";
            print x;
            print_string ", ";
            print_elem_stream xs;
            print_string ")" in
        if show || Lazy.lazy_is_val s
        then print_elem_stream_val s
        else print_string "SUSP" in
    let rec print_elem_list = function
      | [] -> ()
      | (x :: xs) -> print x; print_string ";"; print_elem_list xs in
    print_string "RealTimeQueue\n\t(";
    print_elem_stream f;
    print_string ",\n\t[";
    print_elem_list r;
    print_string "],\n\t";
    print_elem_stream s;
    print_string ")";
    print_newline ()
  ;;
end

module RealTimeQueue (Item : ITEM) : RQUEUE
  with type elt = Item.t = 
struct
  include RealTimeQueueP
  
  type elt = Item.t
  type t = elt q

  let dprint show q = print_queue Item.print show q
  ;;

  let print q = dprint false q
  ;;
end

module IntQueue = RealTimeQueue (Int)
