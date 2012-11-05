open Item;;
open Ordered;;
open RandomAccessList;;

module (*rec*) PolymorphicBinaryRandomAccessList : sig
  type 'a plist = NIL
                | ZERO of ('a * 'a) plist
                | ONE of 'a * ('a * 'a) plist
  
  val isEmpty     : 'a plist -> bool
  val cons        : 'a * 'a plist -> 'a plist

  val uncons      : 'a plist -> ('a * 'a plist)
  val head        : 'a plist -> 'a
  val tail        : 'a plist -> 'a plist
  val lookup      : int * 'a plist -> 'a
  val fupdate     : ('a -> 'a) * int * 'a plist -> 'a plist
  val update      : int * 'a * 'a plist -> 'a plist

  val print_plist : ('a -> unit) * 'a plist -> unit
end = struct
  (* open PolymorphicBinaryRandomAccessList *)

  type 'a plist = NIL
                | ZERO of ('a * 'a) plist
                | ONE of 'a * ('a * 'a) plist

  exception Subscript

  let isEmpty = function
    | NIL -> true
    | _ -> false
  ;;

  let rec cons : 'a.'a * 'a plist -> 'a plist = function
    | (x, NIL) -> ONE (x, NIL)
    | (x, ZERO ps) -> ONE (x, ps)
    | (x, ONE (y, ps)) -> ZERO (cons ((x, y), ps))
  ;;

  let rec uncons : 'a.'a plist -> ('a * 'a plist) = function
    | ONE (x, NIL) -> (x, NIL)
    | ONE (x, ps) -> (x, ZERO ps)
    | ZERO ps ->
        let ((x, y), ps') = uncons ps in
        (x, ONE (y, ps'))
  ;;

  let head xs = let (x, _) = uncons xs in x
  ;;

  let tail xs = let (_, xs') = uncons xs in xs'
  ;;

  let rec lookup : 'a.int * 'a plist -> 'a = function
    | (i, NIL) -> raise Subscript
    | (0, ONE (x, ps)) -> x
    | (i, ONE (x, ps)) -> lookup (i-1, ZERO ps)
    | (i, ZERO ps) ->
        let (x, y) = lookup (i/2, ps) in
        if i mod 2 = 0 then x else y
  ;;

  let rec fupdate : 'a.('a -> 'a) * int * 'a plist -> 'a plist = function
    | (f, i, NIL) -> raise Subscript
    | (f, 0, ONE (x, ps)) -> ONE (f x, ps)
    | (f, i, ONE (x, ps)) -> cons (x, fupdate (f, i-1, ZERO ps))
    | (f, i, ZERO ps) ->
        let f' (x, y) = if i mod 2 = 0 then (f x, y) else (x, f y) in
        ZERO (fupdate (f', i/2, ps))
  ;;

  let update (i, y, xs) = fupdate ((fun x -> y), i, xs)
  ;;

  let rec print_plist : 'a.('a -> unit) * 'a plist -> unit = function
    | (f, NIL) ->
        print_string "NIL\n"
    | (f, ZERO ps) ->
        let f' (x, y) =
          print_string "(";
          f x;
          print_string ", ";
          f y;
          print_string ")" in
        print_string "ZERO (\n";
        print_plist (f', ps);
        print_string ")"
    | (f, ONE (x, ps)) ->
        let f' (x, y) =
          print_string "(";
          f x;
          print_string ", ";
          f y;
          print_string ")" in
        print_string "ONE (";
        f x;
        print_string ", \n";
        print_plist (f', ps);
        print_string ")"
  ;;
end

module AltBinaryRandomAccessList (Element : ITEM) : sig
  include RANDOMACCESSLIST with module Elem = Element
end = struct
  exception Empty
  exception Subscript

  include PolymorphicBinaryRandomAccessList
  module Elem = Element

  type rlist = Elem.t plist

  let empty = NIL
  ;;

  let dprint show xs =
    print_string "rlist (\n";
    print_plist ((fun x -> Elem.print x), xs);
    print_string ")";
    print_newline ()
  ;;

  let print = dprint false
  ;;
end

module IntAltBinaryRandomAccessList = AltBinaryRandomAccessList (Int)
