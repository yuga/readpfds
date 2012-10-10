open Item;;
open Ordered;;
open Rqueue;;

module HoodMelvilleQueue (Element : ITEM) : RQUEUE
  with module Elem = Element =
struct
  module Elem = Element

  type 'a rotation_state =
      IDLE
    | REVERSING of int * 'a list * 'a list * 'a list * 'a list
    | APPENDING of int * 'a list * 'a list
    | DONE of 'a list

  type queue = int * Elem.t list * Elem.t rotation_state * int * Elem.t list

  exception Empty

  let exec = function
    | (REVERSING (ok, x :: f, f', y :: r, r')) -> REVERSING (ok + 1, f, x :: f', r, y :: r')
    | (REVERSING (ok, [], f', [y], r')) -> APPENDING (ok, f', y :: r')
    | (APPENDING (0, f', r')) -> DONE r'
    | (APPENDING (ok, x :: f', r')) -> APPENDING (ok - 1, f', x :: r')
    | state -> state
  ;;

  let invalidate = function
    | (REVERSING (ok, f, f', r, r')) -> REVERSING (ok - 1, f, f', r, r')
    | (APPENDING (0, f', x :: r')) -> DONE r'
    | (APPENDING (ok, f', r')) -> APPENDING (ok - 1, f', r')
    | state -> state
  ;;

  let exec2 (lenf, f, state, lenr, r) =
    match exec (exec state) with
      | (DONE newf) -> (lenf, newf, IDLE, lenr, r)
      | newstate -> (lenf, f, newstate, lenr, r)
  ;;

  let check ((lenf, f, state, lenr, r) as q) =
    if lenr <= lenf
    then exec2 q
    else let newstate = REVERSING (0, f, [], r, []) in
         exec2 (lenf + lenr, f, newstate, 0, [])
  ;;

  let empty = (0, [], IDLE, 0, [])
  ;;

  let isEmpty (lenf, f, state, lenr, r) = (lenf == 0)
  ;;

  let snoc ((lenf, f, state, lenr, r), x) = check (lenf, f, state, lenr + 1, x :: r)
  ;;

  let head = function
    | (lenf, [], state, lenr, r) -> raise Empty
    | (lenf, x :: f, state, lenr, r) -> x
  ;;

  let tail = function
    | (lenf, [], state, lenr, r) -> raise Empty
    | (lenf, x :: f, state, lenr, r) -> check (lenf - 1, f, invalidate state, lenr, r)
  ;;

  let print (lenf, f, state, lenr, r) =
    let rec print_list = function
      | [] -> ()
      | (x :: xs) ->
          Elem.print x;
          print_string ";";
          print_list xs in
    let print_state = function
      | IDLE -> print_string "IDLE"
      | (REVERSING (ok, f, f', r, r')) ->
          print_string "REVERSING\n\t\t(";
          print_int ok;
          print_string ",\n\t\t[";
          print_list f;
          print_string "],\n\t\t[";
          print_list f';
          print_string "],\n\t\t[";
          print_list r;
          print_string "],\n\t\t[";
          print_list r';
          print_string "])"
      | (APPENDING (ok, f', r')) ->
          print_string "APPENDING\n\t\t(";
          print_int ok;
          print_string ",\n\t\t[";
          print_list f';
          print_string "],\n\t\t[";
          print_list r';
          print_string "])"
      | (DONE newf) ->
          print_string "DONE\n\t\t([";
          print_list newf;
          print_string "])" in
    print_string "queue\n\t(";
    print_int lenf;
    print_string ",\n\t[";
    print_list f;
    print_string "],\n\t";
    print_state state;
    print_string ",\n\t";
    print_int lenr;
    print_string ",\n\t[";
    print_list r;
    print_string"])";
    print_newline ();
  ;;

  let dprint _ q = print q
  ;;
end

module IntHoodMelvilleQueue = HoodMelvilleQueue (Int) 
