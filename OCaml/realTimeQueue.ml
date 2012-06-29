open Rqueue;;
open SmallStream;;
open Printf;;

module RealTimeQueue : RQUEUE =
struct
  module S = SmallStream

  (* definition of queue
   * (front, rear, schedule) *)
  type 'a queue = 'a S.stream * 'a list * 'a S.stream;;

  (* exception *)
  exception Empty;;

  (* funcitons *)
  let empty = (lazy S.Nil, [], lazy S.Nil);;

  let isEmpty = function
    | (lazy S.Nil, _, _) -> true
    | _ -> false
  ;;

  let rec rotate : 'a queue -> 'a S.stream = function
    | (lazy S.Nil, y :: _, a) -> lazy (S.Cons (y, a))
    | (lazy (S.Cons (x, xs)), y :: ys, a) ->
        lazy (S.Cons (x, rotate (xs, ys, lazy (S.Cons (y, a)))))
  ;;

  let exec : 'a queue -> 'a queue = function
    | (f, r, lazy (S.Cons (x, s))) -> (f, r, s)
    | (f, r, lazy S.Nil) -> let f' = rotate (f, r, lazy S.Nil)
                            in (f', [], f')
  ;;

  let snoc (f, r, s) x = exec (f, x :: r, s);;

  let head = function
    | (lazy S.Nil, r, s) -> raise Empty
    | (lazy (S.Cons (x, f)), r, s) -> x
  ;;

  let tail = function
    | (lazy S.Nil, r, s) -> raise Empty
    | (lazy (S.Cons (x, f)), r, s) -> exec (f, r, s)
  ;;

  let print q =
    let rec print_queue chan = function
      | (f, r, s) ->
          S.print f;
          output_value chan r;
          S.print s
    in printf "%a\n" print_queue q
  ;;
end
