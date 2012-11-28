open Item;;
open Ordered;;
open Rqueue;;

module ImplicitCatenableDequeP (D : RDEQUEPS) : CATENABLEDEQUEP = struct
  type 'a cat = SHALLOW of 'a D.q
              | DEEP of 'a D.q * 'a cmpd cat Lazy.t * 'a D.q 
                               * 'a cmpd cat Lazy.t * 'a D.q
  and 'a cmpd = SIMPLE of 'a D.q
              | CMPD of 'a D.q * 'a cmpd cat Lazy.t * 'a D.q
  type 'a q = 'a cat

  exception Empty

  let empty = SHALLOW D.empty

  let isEmpty = function
    | SHALLOW d -> D.isEmpty d
    | _ -> false

  let cons = function
    | (x, SHALLOW d) -> SHALLOW (D.cons (x, d))
    | (x, DEEP (f, a, m, b, r)) -> DEEP (D.cons (x, f), a, m, b, r)

  let head = function
    | SHALLOW d -> D.head d
    | DEEP (f, a, m, b, r) -> D.head f

  let snoc = function
    | (SHALLOW d, x) -> SHALLOW (D.snoc (d, x))
    | (DEEP (f, a, m, b, r), x) -> DEEP (f, a, m, b, D.snoc (r, x))

  let last = function
    | SHALLOW d -> D.last d
    | DEEP (f, a, m, b, r) -> D.last r

  let share (f, r) =
    let m = D.cons (D.last f, D.cons (D.head r, D.empty))
    in (D.init f, m, D.tail r)

  let rec dappendL (d1, d2) =
    if D.isEmpty d1 then d2
    else dappendL (D.init d1, D.cons (D.last d1, d2))

  let rec dappendR (d1, d2) =
    if D.isEmpty d2 then d1
    else dappendR (D.snoc (d1, D.head d2), D.tail d2)

  let (++) : 'a cat -> 'a cat -> 'a cat = fun x1 x2 -> match (x1, x2) with 
    | (SHALLOW d1, SHALLOW d2) ->
        if D.size d1 < 4 then SHALLOW (dappendL (d1, d2))
        else if D.size d2 < 4 then SHALLOW (dappendR (d1, d2))
        else let (f, m, r) = share (d1, d2)
             in DEEP (f, lazy empty, m, lazy empty, r)
    | (SHALLOW d1, DEEP (f, a, m, b, r)) ->
        if D.size d1 < 4 then DEEP (dappendL (d1, f), a, m, b, r)
        else DEEP (d1, lazy (cons (SIMPLE f, Lazy.force a)), m, b, r)
    | (DEEP (f, a, m, b, r), SHALLOW d2) ->
        if D.size d2 < 4 then DEEP (f, a, m, b, dappendR (r, d2))
        else DEEP (f, a, m, lazy (snoc (Lazy.force b, SIMPLE r)), d2)
    | (DEEP (f1, a1, m1, b1, r1), DEEP (f2, a2, m2, b2, r2)) ->
        let (r1', m, f2') = share (r1, f2) in
        let a1' = lazy (snoc (Lazy.force a1, CMPD (m1, b1, r1'))) in
        let b2' = lazy (cons (CMPD (f2', a2, m2), Lazy.force b2))
        in DEEP (f1, a1', m, b2', r2)

  let replaceHead = function
    | (x, SHALLOW d) -> SHALLOW (D.cons (x, D.tail d))
    | (x, DEEP (f, a, m, b, r)) -> DEEP (D.cons (x, D.tail f), a, m, b, r)

  let rec tail : 'a. 'a cat -> 'a cat = function
    | SHALLOW d -> SHALLOW (D.tail d)
    | DEEP (f, a, m, b, r) ->
        if D.size f > 3 then DEEP (D.tail f, a, m, b, r)
        else if not (isEmpty (Lazy.force a)) then
          match head (Lazy.force a) with
            | SIMPLE d ->
                let f' = dappendL (D.tail f, d)
                in DEEP (f', lazy (tail (Lazy.force a)), m, b, r)
            | CMPD (f', c', r') ->
                let f'' = dappendL (D.tail f, f') in
                let a'' = lazy ((Lazy.force c') ++ (replaceHead (SIMPLE r', Lazy.force a)))
                in DEEP (f'', a'', m, b, r)
        else if not (isEmpty (Lazy.force b)) then
          match head (Lazy.force b) with
            | SIMPLE d ->
                let f' = dappendL (D.tail f, m)
                in DEEP (f', lazy empty, d, lazy (tail (Lazy.force b)), r)
            | CMPD (f', c', r') -> 
                let f'' = dappendL (D.tail f, m) in
                let a'' = lazy (cons (SIMPLE f', Lazy.force c'))
                in DEEP (f'', a'', r', lazy (tail (Lazy.force b)), r)
        else SHALLOW (dappendL (D.tail f, m)) ++ SHALLOW r

  let replaceLast = function
    | (SHALLOW d, x) -> SHALLOW (D.snoc (D.tail d, x))
    | (DEEP (f, a, m, b, r), x) -> DEEP (f, a, m, b, D.snoc (D.tail r, x))

  let rec init : 'a. 'a cat -> 'a cat = function
    | SHALLOW d -> SHALLOW (D.init d)
    | DEEP (f, a, m, b, r) ->
        if D.size r > 3 then DEEP (f, a, m, b, D.init r)
        else if not (isEmpty (Lazy.force b)) then
          match last (Lazy.force b) with
            | SIMPLE d ->
                let r' = dappendR (d, D.init r)
                in DEEP (f, a, m, lazy (init (Lazy.force b)), r')
            | CMPD (f', c', r') ->
                let r'' = dappendR (r', D.init r) in
                let b'' = lazy (Lazy.force c' ++ replaceLast (Lazy.force b, SIMPLE r'))
                in DEEP (f, a, m, b'', r'')
        else if not (isEmpty (Lazy.force a)) then
          match last (Lazy.force a) with
            | SIMPLE d ->
                let r' = dappendR (m, D.init r)
                in DEEP (f, lazy (init (Lazy.force a)), d, lazy empty, r')
            | CMPD (f', c', r') ->
                let r'' = dappendR (m, D.init r) in
                let b'' = lazy (snoc (Lazy.force c', SIMPLE r'))
                in DEEP (f, lazy (init (Lazy.force a)), f', b'', r'')
        else SHALLOW f ++ SHALLOW (dappendR (m, D.init r))

  let rec print_cat : 'a. ('a -> unit) -> bool -> 'a cat -> unit =
    fun print_a show c ->
        let print_deque =
          D.print_queue print_a show
        in let rec print_cmpd = function
          | SIMPLE d ->
              print_string "SIMPLE (";
              print_deque d;
              print_string ")"
          | CMPD (f, c, r) ->
              print_string "CMPD (";
              print_deque f;
              print_string ", ";
              print_cmpd_cat_susp c;
              print_string ", ";
              print_deque r;
              print_string ")"
        and print_cmpd_cat_susp s =
          let print_cmpd_cat =
              print_cat print_cmpd show
          in if show || Lazy.lazy_is_val s
          then print_cmpd_cat (Lazy.force s)
          else print_string "SUSP"
        in match c with
          | SHALLOW d -> 
              print_string "SHALLOW (";
              print_deque d;
              print_string ")"
          | DEEP (f, a, m, b, r) ->
              print_string "DEEP (";
              print_deque f;
              print_string ", ";
              print_cmpd_cat_susp a;
              print_string ", ";
              print_deque m;
              print_string ", ";
              print_cmpd_cat_susp b;
              print_string ", ";
              print_deque r;
              print_string ")"

  let print_queue print_a show q =
    print_cat print_a show q;
    print_newline ()
end

module ImplicitCatenableDeque (D : RDEQUEPS) (Item : ITEM) : sig
  include CATENABLEDEQUE with type elt = Item.t
end = struct
  include ImplicitCatenableDequeP (D)

  type elt = Item.t
  type t = elt q

  let dprint show q =
    print_queue Item.print show q

  let print q = dprint false q
end

open RealTimeDeque;;

module IntImplicitCatenableDeque = ImplicitCatenableDeque (RealTimeDequeP (struct let c = 3 end)) (Int) 
