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
  and schedule = digit S.stream list
  and heap = digit S.stream * schedule 

  exception Empty

  let empty = (lazy S.Nil, [])
  ;;

  let isEmpty = function
    | (lazy S.Nil, _) -> true
    | _ -> false
  ;;

  let link ((Node (x1, c1) as t1), (Node (x2, c2) as t2)) =
    if Elem.eq (x1, x2)
    then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)
  ;;

  let rec insTree = function
    | (t, lazy S.Nil) -> lazy (S.Cons (One t, lazy S.Nil))
    | (t, lazy (S.Cons (Zero, ds))) -> lazy (S.Cons (One t, ds))
    | (t, lazy (S.Cons (One t', ds))) -> lazy (S.Cons (Zero, insTree (link (t, t' ), ds)))
  ;;

  let rec mrg = function
    | (ds1, lazy S.Nil) -> ds1
    | (lazy S.Nil, ds2) -> ds2
    | (lazy (S.Cons (Zero, ds1)), lazy (S.Cons (d, ds2))) -> lazy (S.Cons (d, mrg (ds1, ds2)))
    | (lazy (S.Cons (d, ds1)), lazy (S.Cons (Zero, ds2))) -> lazy (S.Cons (d, mrg (ds1, ds2)))
    | (lazy (S.Cons (One t1, ds1)), lazy (S.Cons (One t2, ds2))) ->
        lazy (S.Cons (Zero, insTree (link (t1, t2), mrg (ds1, ds2))))
  ;;

  let rec normalize = function
    | (lazy S.Nil as ds) -> ds
    | (lazy (S.Cons (_, ds')) as ds) -> (
        ignore (normalize ds'); 
        ds)
  ;;

  let exec = function
    | ((lazy (S.Cons (Zero, job))) :: sched) -> job :: sched
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
    | (lazy S.Nil) -> raise Empty
    | (lazy (S.Cons (One t, lazy S.Nil))) -> (t, lazy S.Nil)
    | (lazy (S.Cons (Zero, ds))) ->
        let (t', ds') = removeMinTree ds in
        (t', lazy (S.Cons (Zero, ds')))
    | (lazy (S.Cons (One (Node (x, _) as t), ds))) ->
        match removeMinTree ds with
          | ((Node (x', _) as t'), ds') ->
              if Elem.eq (x, x') then (t, lazy (S.Cons (Zero, ds)))
              else (t', lazy (S.Cons (One t, ds')))
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

  let print (ds, sched) =
    let rec print_stream = function
      | (lazy S.Nil) -> 
          print_string "$Nil"
      | (lazy (S.Cons (x, xs))) -> (
          print_string "$Cons (";
          print_digit  x;
          print_string ", ";
          print_stream xs;
          print_string ")")
    and print_schedule ds = (
      print_string "schedule [";
      print_stream_list ds;
      print_string "]")
    and print_stream_list = function
      | [] -> ()
      | (x :: xs) -> (
          print_stream x;
          print_string ";";
          print_stream_list xs)
    and print_digit = function
      | (Zero) ->
          print_string "Zero"
      | (One tree) -> (
          print_string "One (";
          print_tree   tree;
          print_string ")")
    and print_tree = function
      | (Node (x, xs)) -> (
          print_string "Node (";
          Elem.print   x;
          print_string ", [";
          print_tree_list xs;
          print_string "])")
    and print_tree_list = function
      | [] -> ()
      | (x :: xs) -> (
          print_tree   x;
          print_string ";";
          print_tree_list xs) in
    print_string "heap (";
    print_stream ds;
    print_string ", ";
    print_schedule sched;
    print_string ")";
    print_newline ()
  ;;
end

module IntHeap = ScheduledBinomialHeap (Int);;
