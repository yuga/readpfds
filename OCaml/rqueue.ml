open Item

module type RQUEUE =
sig
  module Elem : ITEM

  type queue
  
  exception Empty

  val empty : queue
  val isEmpty : queue -> bool
  
  val snoc : (queue * Elem.t) -> queue
  val head : queue -> Elem.t (* raises Empty if queue is empty *)
  val tail : queue -> queue (* raises Empty if queue is empty *)

  val print : queue -> unit
  val dprint : bool -> queue -> unit
end

module type RDEQUE =
sig
  include RQUEUE

  val cons : (Elem.t * queue) -> queue
  val last : queue -> Elem.t
  val init : queue -> queue
end
