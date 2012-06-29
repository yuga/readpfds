open Queue;;
open SmallStream;;
open Printf;;

module RealTimeQueue : QUEUE =
struct
  module S = SmallStream

  (* definition of queue
   * (front, rear, schedule) *)
  type 'a queue = 'a S.sstream * 'a list * 'a S.sstream;;

  (* exception *)
  exception Empty;;

  (* funcitons *)
  let empty = (lazy S.SSnil, [], lazy S.SSnil);;

  let isEmpty = function
    | (lazy S.SSnil, _, _) -> true
    | _ -> false
  ;;

  let rec rotate : 'a queue -> 'a S.sstream = function
    | (lazy S.SSnil, y :: _, a) -> lazy (S.SScons (y, a))
    | (lazy (S.SScons (x, xs)), y :: ys, a) ->
        lazy (S.SScons (x, rotate (xs, ys, lazy (S.SScons (y, a)))))
  ;;

  let exec : 'a queue -> 'a queue = function
    | (f, r, lazy (S.SScons (x, s))) -> (f, r, s)
    | (f, r, lazy S.SSnil) -> let f' = rotate (f, r, lazy S.SSnil)
                            in (f', [], f')
  ;;

  let snoc (f, r, s) x = exec (f, x :: r, s);;

  let head = function
    | (lazy S.SSnil, r, s) -> raise Empty
    | (lazy (S.SScons (x, f)), r, s) -> x
  ;;

  let tail = function
    | (lazy S.SSnil, r, s) -> raise Empty
    | (lazy (S.SScons (x, f)), r, s) -> exec (f, r, s)
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
