open Item;;
open Ordered;;
open Rqueue;;

module SimpleCatenableDequeP (D : RDEQUEP) : CATENABLEDEQUEP = struct
  (* DEEP (f, m, r) is created by only (++) function when
   * both of the catenated queues have 2 elements or more.
   * Both of tail and init function change a queue from DEEP to SHALLOW
   * when either of front or rear c-deques have at most one element
   * after removing an element. *)
  type 'a cat = SHALLOW of 'a D.q
              | DEEP of 'a D.q * 'a D.q cat Lazy.t * 'a D.q
  type 'a q = 'a cat

  exception Empty

  let tooSmall d = D.isEmpty d || D.isEmpty (D.tail d)
  
  let dappendL (d1, d2) =
    if D.isEmpty d1 then d2
    else D.cons (D.head d1, d2)

  let dappendR (d1, d2) =
    if D.isEmpty d2 then d1
    else D.snoc (d1, D.head d2)

  let empty = SHALLOW D.empty

  let isEmpty = function
    | SHALLOW d -> D.isEmpty d
    | _ -> false

  let cons = function
    | (x, SHALLOW d) -> SHALLOW (D.cons (x, d))
    | (x, DEEP (f, m, r)) -> DEEP (D.cons (x, f), m, r)

  let rec head : 'a. 'a cat -> 'a  = function
    | SHALLOW d -> D.head d
    | DEEP (f, m, r) -> D.head f
  and tail : 'a. 'a cat -> 'a cat = function
    | SHALLOW d -> SHALLOW (D.tail d)
    | DEEP (f, m, r) ->
        let f' = D.tail f
        in if not (tooSmall f') then DEEP (f', m, r)
           else if isEmpty (Lazy.force m) then SHALLOW (dappendL (f', r))
                else DEEP (dappendL (f', head (Lazy.force m)), lazy (tail (Lazy.force m)), r) 

  let snoc = function
    | (SHALLOW d, x) -> SHALLOW (D.snoc (d, x))
    | (DEEP (f, m, r), x) -> DEEP (f, m, D.snoc (r, x))

  let rec last : 'a. 'a cat -> 'a = function
    | SHALLOW d -> D.last d
    | DEEP (f, m, r) -> D.last r
  and init : 'a. 'a cat -> 'a cat = function
    | SHALLOW d -> SHALLOW (D.init d)
    | DEEP (f, m, r) ->
        let r' = D.init r
        in if not (tooSmall r') then DEEP (f, m, r')
           else if isEmpty (Lazy.force m) then SHALLOW (dappendR (f, r'))
           else DEEP (f, lazy (init (Lazy.force m)), dappendR (last (Lazy.force m), r'))

  let rec (++) : 'a. 'a cat -> 'a cat -> 'a cat = fun x1 x2 -> match (x1, x2) with
    | (SHALLOW d1, SHALLOW d2) ->
        if tooSmall d1 then SHALLOW (dappendL (d1, d2))
        else if tooSmall d2 then SHALLOW (dappendR (d1, d2))
        else DEEP (d1, lazy empty, d2)
    | (SHALLOW d1, DEEP (f, m, r)) ->
        if tooSmall d1 then DEEP (dappendL (d1, f), m, r)
        else DEEP (d1, lazy (cons (f, Lazy.force m)), r)
    | (DEEP (f, m, r), SHALLOW d2) ->
        if tooSmall d2 then DEEP (f, m, dappendR (r, d2))
        else DEEP (f, lazy (snoc (Lazy.force m, r)), d2)
    | (DEEP (f1, m1, r1), DEEP (f2, m2, r2)) ->
        DEEP (f1, lazy (snoc (Lazy.force m1, r1) ++ cons (f2, Lazy.force m2)), r2) 

  let rec print_cat : 'a. ('a -> unit) -> bool -> 'a cat -> unit =
    fun print_a show c ->
        let print_deque =
          D.print_queue print_a show
        in let print_deque_cat =
          print_cat print_deque show
        in let print_cat_susp s =
          if show || Lazy.lazy_is_val s
          then print_deque_cat (Lazy.force s)
          else print_string "SUSP"
        in match c with
          | SHALLOW d -> 
              print_string "SHALLOW (";
              print_deque d;
              print_string ")"
          | DEEP (f, m, r) ->
              print_string "DEEP (";
              print_deque f;
              print_string ", ";
              print_cat_susp m;
              print_string ", ";
              print_deque r;
              print_string ")"

  let print_queue print_a show q =
    print_cat print_a show q;
    print_newline ()
end

module SimpleCatenableDeque (D : RDEQUEP) (Item : ITEM) : sig
  include CATENABLEDEQUE with type elt = Item.t
end = struct
  include SimpleCatenableDequeP (D)

  type elt = Item.t
  type t = elt q

  let dprint show q =
    print_queue Item.print show q

  let print q = dprint false q
end

open RealTimeDeque;;

module IntSimpleCatenableDeque = SimpleCatenableDeque (RealTimeDequeP (struct let c = 3 end)) (Int) 
