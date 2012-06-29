open Ordered;;
open Sortable;;
open SmallStream;;

module ScheduledBottomUpMergeSort (Element : ORDERED) : SORTABLE
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  type schedule = Elem.t S.sstream list
  and sortable = int * (Elem.t S.sstream * schedule) list

  let rec mrg : Elem.t S.sstream * Elem.t S.sstream -> Elem.t S.sstream =
    function | (xs,ys) -> lazy (match (xs, ys) with
    | (lazy S.SSnil, lazy ys) -> ys
    | (lazy xs, lazy S.SSnil) -> xs
    | (lazy (S.SScons (x, xs')), lazy (S.SScons (y, ys'))) ->
        if Elem.leq (x, y)
        then S.SScons (x, mrg (xs', ys))
        else S.SScons (y, mrg (xs, ys')))
  ;;

  let rec exec1 = function
    | [] -> []
    | ((lazy S.SSnil) :: sched) -> exec1 sched
    | ((lazy (S.SScons (x, xs))) :: sched) -> xs :: sched
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
    let segs' = addSeg (lazy (S.SScons (x, lazy S.SSnil)), segs, size, []) in
    (size+1, List.map exec2 segs')
  ;;

  let sort (size, segs) =
    let rec mrgAll = function
      | (xs, []) -> xs
      | (xs, (xs', _) :: segs) -> mrgAll (mrg (xs, xs'), segs) in
    streamToList (mrgAll (lazy S.SSnil, segs))
  ;;
end

module IntSort = ScheduledBottomUpMergeSort (Int);;
