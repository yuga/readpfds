(* These modules are based on the original smallStream.ml written by Okabe san (@master_q).
 * https://github.com/master-q/readPurelyFunctionalDataStructures/tree/master/LazyEvaluation
 *)

open Printf

module type SMALLSTREAM =
sig
  type 'a cell = Nil | Cons of 'a * 'a stream
  and  'a stream = 'a cell Lazy.t
  (* type 'a scell;;   *)
  (* type 'a stream;; *)

  exception Empty;;

  val empty : 'a stream
  val cons : 'a -> 'a stream -> 'a stream
  val (++) : 'a stream -> 'a stream -> 'a stream
  val take : int * 'a stream -> 'a stream
  val drop : int * 'a stream -> 'a stream
  val repeat : 'a -> 'a stream
  val reverse : 'a stream -> 'a stream

  val print : 'a stream -> unit
end

module SmallStream : SMALLSTREAM =
struct
  (* definition of stream *)
  type 'a cell = Nil | Cons of 'a * 'a stream
  and  'a stream = 'a cell Lazy.t

  (* exception *)
  exception Empty

  (* functions *)
  let empty = lazy Nil
  ;;

  let cons x xs = lazy (Cons (x, xs))
  ;;

  let rec (++) t1 t2 = lazy (match (t1, t2) with
    | (lazy Nil, lazy t2) -> t2
    | (lazy (Cons(x, s)), t2) -> Cons (x, s ++ t2))
  ;;

  let rec take (n, s) = lazy (match (n, s) with
    | (0, _) -> Nil
    | (_, lazy Nil) -> Nil
    | (n, lazy (Cons (x, s))) -> Cons (x, take ((n-1), s)))
  ;;

  let drop (n, s) = lazy (
    let rec drop' n s = match (n, s) with
      | (0, lazy s) -> s
      | (_, lazy Nil) -> Nil
      | (n, lazy (Cons (_, s))) -> drop' (n-1) s
    in drop' n s)
  ;;

  let repeat x =
    let rec xs = lazy (Cons (x, xs))
    in xs
  ;;

  let reverse xs = lazy (
    let rec reverse' xs r = match (xs, r) with
      | (lazy Nil, r) -> r
      | (lazy (Cons (x, xs')), r) -> reverse' xs' (Cons (x, lazy r))
    in reverse' xs Nil)
  ;;

  let print xs =
    let rec print_stream chan = function
      | (lazy Nil) -> output_string chan "Nil"
      | (lazy (Cons (x, xs'))) -> 
        output_string chan "Cons (";
        output_value  chan x;
        output_string chan ", ";
        print_stream chan xs';
        output_string chan ")"
    in printf "%a\n" print_stream xs
  ;;
end

let rec listToStream = function
  | [] -> lazy SmallStream.Nil
  | (x :: xs) -> lazy (SmallStream.Cons (x, listToStream xs))
;;

let rec streamToList = function
  | (lazy SmallStream.Nil) -> []
  | (lazy (SmallStream.Cons (x, xs))) -> x :: streamToList xs
;;
