open Item;;
open Ordered;;
open Rqueue;;

module ImplicitQueueP : RQUEUEP = struct
  type 'a digit = ZERO
                | ONE of 'a
                | TWO of 'a * 'a
  type 'a queue = SHALLOW of 'a digit
                | DEEP of 'a digit * ('a * 'a) queue Lazy.t * 'a digit
  type 'a q = 'a queue

  exception Empty
  
  let empty = SHALLOW ZERO

  let isEmpty = function
    | SHALLOW ZERO -> true
    | _ -> false

  let rec snoc : 'a. 'a queue * 'a -> 'a queue = function
    | (SHALLOW ZERO, y) -> SHALLOW (ONE y)
    | (SHALLOW (ONE x), y) -> DEEP (TWO (x, y), lazy empty, ZERO)
    | (DEEP (f, m, ZERO), y) -> DEEP (f, m, ONE y)
    | (DEEP (f, m, ONE x), y) -> DEEP (f, lazy (snoc (Lazy.force m, (x, y))), ZERO)

  let head : 'a. 'a queue -> 'a = function
    | SHALLOW ZERO -> raise Empty
    | SHALLOW (ONE x) -> x
    | DEEP (ONE x, m , r) -> x
    | DEEP (TWO (x, y), m, r) -> x

  let rec tail : 'a. 'a queue -> 'a queue = function
    | SHALLOW ZERO -> raise Empty
    | SHALLOW (ONE x) -> SHALLOW ZERO
    | DEEP (TWO (x, y), m, r) -> DEEP (ONE y, m, r)
    | DEEP (ONE x, lazy q, r) ->
        if isEmpty q then SHALLOW r
        else let (y, z) = head q
             in DEEP (TWO (y, z), lazy (tail q), r)

  let rec print_queuerec : 'a. ('a -> unit) -> bool -> 'a queue -> unit =
    fun print_a show q ->
        let print_digit = function
          | ZERO ->
              print_string "ZERO"
          | ONE x ->
              print_string "ONE (";
              print_a x;
              print_string ")"
          | TWO (x, y) ->
              print_string "TWO (";
              print_a x;
              print_string ", ";
              print_a y;
              print_string ")"
        in let print_a_tuple (x, y) =
          print_string "(";
          print_a x;
          print_string ", ";
          print_a y;
          print_string ")"
        in let print_queue_susp s =
          if show || Lazy.lazy_is_val s
          then print_queuerec print_a_tuple show (Lazy.force s)
          else print_string "SUSP"
        in match q with
          | SHALLOW d -> 
              print_string "SHALLOW (";
              print_digit d;
              print_string ")"
          | DEEP (f, m, r) ->
              print_string "DEEP (";
              print_digit f;
              print_string ", ";
              print_queue_susp m;
              print_string ", ";
              print_digit r;
              print_string ")"

  let print_queue print_a show q =
    print_queuerec print_a show q;
    print_newline ()
end

module ImplicitQueue (Item : ITEM) : sig
  include RQUEUE with type elt = Item.t 
end = struct
  include ImplicitQueueP

  type elt = Item.t
  type t = elt q

  let dprint show q =
    print_queue Item.print show q

  let print q = dprint false q
end

module IntImplicitQueue = ImplicitQueue (Int)
