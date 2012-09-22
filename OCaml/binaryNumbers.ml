
module Dense =
struct
  type digit = ZERO | ONE
  type nat = digit list

  let rec inc = function
    | [] -> [ONE]
    | (ZERO :: ds) -> ONE :: ds
    | (ONE :: ds) -> ZERO :: inc ds
  ;;

  let rec dec = function
    | [ONE] -> []
    | (ONE :: ds) -> ZERO :: ds
    | (ZERO :: ds) -> ONE :: dec ds
  ;;

  let rec add = function
    | (ds, []) -> ds
    | ([], ds) -> ds
    | (d :: ds1, ZERO :: ds2) -> d :: add (ds1, ds2)
    | (ZERO :: ds1, d :: ds2) -> d :: add (ds1, ds2)
    | (ONE :: ds1, ONE :: ds2) -> ZERO :: inc (add (ds1, ds2))
  ;;
end

module SparseByWeight =
struct
  type nat = int list

  let rec carry = function
    | (w, []) -> [w]
    | (w, (w' :: ws' as ws)) ->
        if w < w' then w :: ws
        else carry (2*w, ws)
  ;;

  let rec borrow (w, (w' :: ws' as ws)) =
    if w = w' then ws'
    else w :: borrow (2*w, ws)
  ;;

  let inc ws = carry (1, ws)
  ;;

  let dec ws = borrow (1, ws)
  ;;

  let rec add = function
    | (ws, []) -> ws
    | ([], ws) -> ws
    | ((w1 :: ws1 as m), (w2 :: ws2 as n)) ->
        if w1 < w2 then w1 :: add (ws1, n)
        else if w2 < w1 then w2 :: add (m, ws2)
        else carry (2*w1, add (ws1, ws2))
  ;;
end

