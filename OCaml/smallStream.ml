(* These modules are based on the original smallStream.ml written by Okabe san (@master_q).
 * https://github.com/master-q/readPurelyFunctionalDataStructures/tree/master/LazyEvaluation
 *)

open Printf

module type SMALLSTREAM =
sig
  type 'a scell = SSnil | SScons of 'a * 'a sstream
  and  'a sstream = 'a scell Lazy.t;;
  (* type 'a scell;;   *)
  (* type 'a sstream;; *)

  exception Empty;;

  val empty : 'a sstream
  val cons : 'a -> 'a sstream -> 'a sstream
  val (++) : 'a sstream -> 'a sstream -> 'a sstream
  val take : int -> 'a sstream -> 'a sstream
  val drop : int -> 'a sstream -> 'a sstream
  val repeat : 'a -> 'a sstream
  val reverse : 'a sstream -> 'a sstream

  val print : 'a sstream -> unit
end

module SmallStream : SMALLSTREAM =
struct
  (* definition of sstream *)
  type 'a scell = SSnil | SScons of 'a * 'a sstream
  and  'a sstream = 'a scell Lazy.t;;

  (* exception *)
  exception Empty;;

  (* functions *)
  let empty = lazy SSnil;;

  let cons x xs = lazy (SScons (x, xs));;

  let rec (++) t1 t2 = lazy (match (t1, t2) with
    | (lazy SSnil, lazy t2) -> SSnil
    | (lazy (SScons(x, s)), t2) -> SScons (x, s ++ t2))
  ;;

  let rec take n s = lazy (match (n, s) with
    | (0, _) -> SSnil
    | (_, lazy SSnil) -> SSnil
    | (n, lazy (SScons (x, s))) -> SScons (x, take (n-1) s))
  ;;

  let drop n s = lazy (
    let rec drop' n s = match (n, s) with
      | (0, lazy s) -> s
      | (_, lazy SSnil) -> SSnil
      | (n, lazy (SScons (_, s))) -> drop' (n-1) s
    in drop' n s)
  ;;

  let repeat x =
    let rec xs = lazy (SScons (x, xs))
    in xs
  ;;

  let reverse xs = lazy (
    let rec reverse' xs r = match (xs, r) with
      | (lazy SSnil, r) -> r
      | (lazy (SScons (x, xs')), r) -> reverse' xs' (SScons (x, lazy r))
    in reverse' xs SSnil)
  ;;

  let print xs =
    let rec print_sstream chan = function
      | (lazy SSnil) -> output_string chan "SSnil"
      | (lazy (SScons (x, xs'))) -> 
        output_string chan "SScons (";
        output_value  chan x;
        output_string chan ", ";
        print_sstream chan xs';
        output_string chan ")"
    in printf "%a\n" print_sstream xs
  ;;
end

let rec listToStream = function
  | [] -> lazy SmallStream.SSnil
  | (x :: xs) -> lazy (SmallStream.SScons (x, listToStream xs))
;;

let rec streamToList = function
  | (lazy SmallStream.SSnil) -> []
  | (lazy (SmallStream.SScons (x, xs))) -> x :: streamToList xs
;;
