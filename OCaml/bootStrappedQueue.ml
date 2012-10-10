open Item;;
open Ordered;;
open Rqueue;;

module QueueException = struct
  exception Empty
end

module rec PolymorphicQueue : sig
  type 'a pqueue = E
                 | Q of int * 'a list * 'a list Lazy.t pqueue * int * 'a list
  val isEmpty      : 'a pqueue -> bool
  val snoc         : 'a pqueue * 'a -> 'a pqueue
  val head         : 'a pqueue -> 'a
  val tail         : 'a pqueue -> 'a pqueue

  val checkF       : (int * 'a list * 'a list Lazy.t pqueue * int * 'a list) -> 'a pqueue
  val checkQ       : (int * 'a list * 'a list Lazy.t pqueue * int * 'a list) -> 'a pqueue 

  val print_pqueue : string * ('a -> unit) * 'a pqueue -> unit
end = struct
  open PolymorphicQueue
  open QueueException

  type 'a pqueue = E
                 | Q of int * 'a list * 'a list Lazy.t pqueue * int * 'a list

  let isEmpty = function
    | E -> true
    | _ -> false
  ;;

  let snoc = function
    | (E, x) -> Q (1, [x], E, 0, [])
    | (Q (lenfm, f, m, lenr, r), x) -> checkQ (lenfm, f, m, lenr+1, x :: r)
  ;;

  let head = function
    | E -> raise Empty
    | Q (lenfm, x :: f', m, lenr, r) -> x
  ;;

  let tail = function
    | E -> raise Empty
    | Q (lenfm, x :: f', m, lenr, r) -> checkQ (lenfm - 1, f', m, lenr, r)
  ;;

  let checkF = function
    | (lenfm, [], E, lenr, r) -> E
    | (lenfm, [], m, lenr, r) -> Q (lenfm, Lazy.force (head m), tail m, lenr, r)
    | (lenfm, f, m, lenr, r)  -> Q (lenfm, f, m, lenr, r)
  ;;

  let checkQ ((lenfm, f, m, lenr, r) as q) =
    if lenfm < lenr then checkF (lenfm + lenr, f, snoc (m, lazy (List.rev r)), 0, [])
    else checkF q
  ;;

  let print_pqueue (indent, print, q) = match q with
    | E -> print_string "E"
    | Q (lenfm, f, m, lenr, r) ->
        let rec print_list = function
          | [] -> ()
          | x :: xs' ->
              print x;
              print_string ";";
              print_list xs' in
        let print_list_susp = function
          | lazy xs ->
              print_string "[";
              print_list xs;
              print_string "]" in
        print_string indent;
        print_string "Q (\n"; print_string indent;
        print_int lenfm;
        print_string ", [";
        print_list f;
        print_string "],\n"; print_string indent;
        print_pqueue (indent ^ "  ", print_list_susp, m);
        print_string ",\n"; print_string indent;
        print_int lenr;
        print_string ", [";
        print_list r;
        print_string "])"
  ;;
end

module BootStrappedQueue (Element : ITEM) : RQUEUE
  with module Elem = Element =
struct
  include QueueException
  include PolymorphicQueue
  module Elem = Element

  type queue = Elem.t pqueue

  let empty = E
  ;;

  let dprint show q =
    print_string "queue (\n";
    print_pqueue ("", (fun x -> Elem.print x), q);
    print_string ")";
    print_newline ()
  ;;

  let print q = dprint false q
  ;;
end

module IntBootStrappedQueue = BootStrappedQueue (Int)
