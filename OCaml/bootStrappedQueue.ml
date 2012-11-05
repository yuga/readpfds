open Item;;
open Ordered;;
open Rqueue;;

module (* rec *) BootStrappedQueuePRec : sig
  type 'a queue = E
                | Q of int * 'a list * 'a list Lazy.t queue * int * 'a list

  val isEmpty      : 'a queue -> bool
  val snoc         : 'a queue * 'a -> 'a queue
  val head         : 'a queue -> 'a
  val tail         : 'a queue -> 'a queue
 
  val checkF       : (int * 'a list * 'a list Lazy.t queue * int * 'a list) -> 'a queue
  val checkQ       : (int * 'a list * 'a list Lazy.t queue * int * 'a list) -> 'a queue 

  val print_queuerec : ('a -> unit) -> bool -> string -> 'a queue -> unit
end = struct
  (* open BootStrappedQueuePRec *)

  type 'a queue = E
                | Q of int * 'a list * 'a list Lazy.t queue * int * 'a list

  exception Empty

  let isEmpty = function
    | E -> true
    | _ -> false
  ;;

  let head = function
    | E -> raise Empty
    | Q (lenfm, x :: f', m, lenr, r) -> x
  ;;

  let rec snoc : 'a.'a queue * 'a -> 'a queue = function
    | (E, x) -> Q (1, [x], E, 0, [])
    | (Q (lenfm, f, m, lenr, r), x) -> checkQ (lenfm, f, m, lenr+1, x :: r)

  and tail : 'a.'a queue -> 'a queue = function
    | E -> raise Empty
    | Q (lenfm, x :: f', m, lenr, r) -> checkQ (lenfm - 1, f', m, lenr, r)

  and checkF : 'a.(int * 'a list * 'a list Lazy.t queue * int * 'a list) -> 'a queue = function
    | (lenfm, [], E, lenr, r) -> E
    | (lenfm, [], m, lenr, r) -> Q (lenfm, Lazy.force (head m), tail m, lenr, r)
    | (lenfm, f, m, lenr, r)  -> Q (lenfm, f, m, lenr, r)

  and checkQ : 'a.(int * 'a list * 'a list Lazy.t queue * int * 'a list) -> 'a queue = function
    | ((lenfm, f, m, lenr, r) as q) ->
        if lenfm < lenr then checkF (lenfm + lenr, f, snoc (m, lazy (List.rev r)), 0, [])
        else checkF q
  ;;

  let rec print_queuerec : 'a.('a -> unit) -> bool -> string -> 'a queue -> unit =
    fun print_a show indent q -> match q with
      | E -> print_string "E"
      | Q (lenfm, f, m, lenr, r) ->
          let rec print_list = function
            | [] -> ()
            | x :: xs' ->
                print_a x;
                print_string ";";
                print_list xs' in
          let print_list_susp s =
            let print_list_susp' = function
              | lazy xs ->
                  print_string "[";
                  print_list xs;
                  print_string "]" in
              if show || Lazy.lazy_is_val s
              then print_list_susp' s
              else print_string "SUSP" in
          print_string indent;
          print_string "Q (\n"; print_string indent;
          print_int lenfm;
          print_string ", [";
          print_list f;
          print_string "],\n"; print_string indent;
          print_queuerec print_list_susp show (indent ^ "  ") m;
          print_string ",\n"; print_string indent;
          print_int lenr;
          print_string ", [";
          print_list r;
          print_string "])"
  ;;
end

module BootStrappedQueueP : sig
  include module type of BootStrappedQueuePRec
  include RQUEUEP with type 'a q = 'a queue
end = struct
  include BootStrappedQueuePRec
  type 'a q = 'a queue
  
  let empty = E
  ;;

  let print_queue print_a show q =
    print_string "BootStrappedQueue (\n";
    print_queuerec (fun x -> print_a x) show "" q;
    print_string ")";
    print_newline ()
  ;;
end

module BootStrappedQueue (Item : ITEM) : sig
  include RQUEUE with type elt = Item.t 
end = struct
  include BootStrappedQueueP

  type elt = Item.t
  type t = elt queue

  let empty = E
  ;;

  let dprint show q =
    print_queue Item.print show q
  ;;

  let print q = dprint false q
  ;;
end

module IntBootStrappedQueue = BootStrappedQueue (Int)
