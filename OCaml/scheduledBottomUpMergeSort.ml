open Ordered;;
open Sortable;;
open SmallStream;;

module ScheduledBottomUpMergeSort (Element : ORDERED) : SORTABLE
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  type schedule = Elem.t S.stream list
  and sortable = int * (Elem.t S.stream * schedule) list

  let rec mrg : Elem.t S.stream * Elem.t S.stream -> Elem.t S.stream =
    function | (xs,ys) -> lazy (match (xs, ys) with
    | (lazy S.Nil, lazy ys) -> ys
    | (lazy xs, lazy S.Nil) -> xs
    | (lazy (S.Cons (x, xs')), lazy (S.Cons (y, ys'))) ->
        if Elem.leq (x, y)
        then S.Cons (x, mrg (xs', ys))
        else S.Cons (y, mrg (xs, ys')))
  ;;

  let rec exec1 = function
    | [] -> []
    | ((lazy S.Nil) :: sched) -> exec1 sched
    | ((lazy (S.Cons (x, xs))) :: sched) -> xs :: sched
  ;;

  let exec2 (xs, sched) = (xs, exec1 (exec1 sched))
  ;;

  let empty = (0, [])
  ;;

  let add (x, (size, segs)) =
    let rec addSeg (xs, segs, size, rsched) =
      if size mod 2 = 0
      then (xs, List.rev rsched) :: segs
      else let ((xs', []) :: segs') = segs in
           let xs'' = mrg (xs, xs') in
           addSeg (xs'', segs', size / 2, xs'' :: rsched) in
    let segs' = addSeg (lazy (S.Cons (x, lazy S.Nil)), segs, size, []) in
    (size+1, List.map exec2 segs')
  ;;

  let sort (size, segs) =
    let rec mrgAll = function
      | (xs, []) -> xs
      | (xs, (xs', _) :: segs) -> mrgAll (mrg (xs, xs'), segs) in
    streamToList (mrgAll (lazy S.Nil, segs))
  ;;

  let print (size, segs) =
    let rec print_stream = function
      | (lazy S.Nil) ->
          print_string "$Nil"
      | (lazy (S.Cons (x, xs))) ->
          print_string "$Cons(";
          Elem.print   x;
          print_stream xs;
          print_string ")" in
    let print_schedule es =
      print_string "schedule [";
      print_stream es;
      print_string "]" in
    let rec print_segment_list = function
      | [] -> ()
      | ((stream, sched) :: ss) ->
          print_string "(";
          print_stream stream;
          print_string ", ";
          print_schedule sched;
          print_string ");";
          print_segment_list ss in
    print_string "sortable (";
    print_int    size;
    print_string ", [";
    print_segment_list segs;
    print_string "])"
end

module IntSort = ScheduledBottomUpMergeSort (Int);;
