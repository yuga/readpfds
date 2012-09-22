open Item

module type RANDOMACCESSLIST =
sig
  module Elem : ITEM

  type rlist

  exception Empty
  exception Subscript

  val empty   : rlist
  val isEmpty : rlist -> bool

  val cons    : Elem.t * rlist -> rlist
  val head    : rlist -> Elem.t
  val tail    : rlist -> rlist

  val lookup  : int * rlist -> Elem.t
  val update  : int * Elem.t * rlist -> rlist

  val print  : rlist -> unit
  val dprint   : bool -> rlist -> unit
end
