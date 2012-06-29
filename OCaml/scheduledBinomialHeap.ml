open Heap;;
open Ordered;;
open SmallStream;;

module ScheduledBinomialHeap (Element : ORDERED) : HEAP
  with module Elem = Element =
struct
  module Elem = Element
  module S = SmallStream

  type tree = Node of Elem.t * tree list
  and digit = Zero | One of tree
  and schedule = digit S.sstream list
  and heap = digit S.sstream * schedule 

  exception Empty

  let empty = (lazy S.SSnil, [])
  ;;

  let isEmpty = function
    | (lazy S.SSnil, _) -> true
    | _ -> false
  ;;

  let link ((Node (x1, c1) as t1), (Node (x2, c2) as t2)) =
    if Elem.eq (x1, x2)
    then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)
  ;;

  let rec insTree = function
    | (t, lazy S.SSnil) -> lazy (S.SScons (One t, lazy S.SSnil))
    | (t, lazy (S.SScons (Zero, ds))) -> lazy (S.SScons (One t, ds))
    | (t, lazy (S.SScons (One t', ds))) -> lazy (S.SScons (Zero, insTree (link (t, t' ), ds)))
  ;;

  let rec mrg = function
    | (ds1, lazy S.SSnil) -> ds1
    | (lazy S.SSnil, ds2) -> ds2
    | (lazy (S.SScons (Zero, ds1)), lazy (S.SScons (d, ds2))) -> lazy (S.SScons (d, mrg (ds1, ds2)))
    | (lazy (S.SScons (d, ds1)), lazy (S.SScons (Zero, ds2))) -> lazy (S.SScons (d, mrg (ds1, ds2)))
    | (lazy (S.SScons (One t1, ds1)), lazy (S.SScons (One t2, ds2))) ->
        lazy (S.SScons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))
  ;;

  let rec normalize = function
    | (lazy S.SSnil as ds) -> ds
    | (lazy (S.SScons (_, ds')) as ds) -> (
        ignore (normalize ds'); 
        ds)
  ;;

  let exec = function
    | ((lazy (S.SScons (Zero, job))) :: sched) -> job :: sched
    | (_ :: sched) -> sched
  ;;

  let insert (x, (ds, sched)) =
    let ds' = insTree (Node (x, []), ds) in
    (ds', exec (exec (ds' :: sched)))
  ;;

  let merge ((ds1, _), (ds2, _)) =
    let ds = normalize (mrg (ds1, ds2)) in
    (ds, [])
  ;;

  let rec removeMinTree = function
    | (lazy S.SSnil) -> raise Empty
    | (lazy (S.SScons (One t, lazy S.SSnil))) -> (t, lazy S.SSnil)
    | (lazy (S.SScons (Zero, ds))) ->
        let (t', ds') = removeMinTree ds in
        (t', lazy (S.SScons (Zero, ds')))
    | (lazy (S.SScons (One (Node (x, _) as t), ds))) ->
        match removeMinTree ds with
          | ((Node (x', _) as t'), ds') ->
              if Elem.eq (x, x') then (t, lazy (S.SScons (Zero, ds)))
              else (t', lazy (S.SScons (One t, ds')))
  ;;

  let findMin (ds, _) =
    let (Node (x, _), _) = removeMinTree ds in
    x
  ;;

  let deleteMin (ds, _) =
    let (Node (x, c), ds') = removeMinTree ds in
    let ds'' = mrg (listToStream (List.map (fun x -> One x) (List.rev c)), ds') in
    (normalize ds'', [])
  ;;

  let dprint heap = print_newline ()
  (*
  let dprint heap =
    let rec dprint = function
      | E -> print_string "E"
      | T (r, y, a, b) ->
        print_string "T ("; print_int r;
        print_string ", " ; Element.dprint y;
        print_string ", " ; dprint a;
        print_string ", " ; dprint b;
        print_string ")"
    in dprint heap; print_newline ();
  *)
  ;;
end

module IntHeap = ScheduledBinomialHeap (Int);;
