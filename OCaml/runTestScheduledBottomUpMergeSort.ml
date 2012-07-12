#use "topfind";;
#load "smallStream.cmo";;
#load "item.cmo";;
#load "ordered.cmo";;
#load "sortable.cmo";;
#load "scheduledBottomUpMergeSort.cmo";;

open ScheduledBottomUpMergeSort;;
module M = IntSort

let make_sortable n =
  let rec make_sortable n i s =
    if i < n
    then make_sortable n (i+1) (M.add (i, s))
    else M.add (i, s)
  in make_sortable n 1 M.empty
;;

print_string "n=0\n"; M.print M.empty;;
print_string "n=1\n"; M.print (make_sortable 1);;
print_string "n=2\n"; M.print (make_sortable 2);;
print_string "n=3\n"; M.print (make_sortable 3);;
print_string "n=4\n"; M.print (make_sortable 4);;
print_string "n=5\n"; M.print (make_sortable 5);;
print_string "n=6\n"; M.print (make_sortable 6);;
print_string "n=7\n"; M.print (make_sortable 7);;
print_string "n=8\n"; M.print (make_sortable 8);;
print_string "n=9\n"; M.print (make_sortable 9);;

let rec print_elem_list = function
  | [] -> ()
  | (x :: xs) ->
      M.Elem.print x;
      print_string ";";
      print_elem_list xs
;;

let sortable_10 = make_sortable 10;;
print_string "n=10:sortable (before)\n"; M.print sortable_10;;
let sorted_10 = M.sort sortable_10;;
print_string "n=10:sorted\n";   print_elem_list sorted_10;;
print_string "n=10:sortable (after)\n"; M.print sortable_10;;
