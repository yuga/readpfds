open Item

module type RQUEUEP =
sig
  type 'a q 
  val empty   : 'a q
  val isEmpty : 'a q -> bool
  
  val snoc    : 'a q * 'a -> 'a q
  val head    : 'a q -> 'a   (* raises Empty if queue is empty *)
  val tail    : 'a q -> 'a q (* raises Empty if queue is empty *)

  val print_queue : ('a -> unit) -> bool -> 'a q -> unit
end

module type RDEQUEP =
sig
  include RQUEUEP

  val cons : 'a * 'a q -> 'a q
  val last : 'a q -> 'a
  val init : 'a q -> 'a q
end

module type RDEQUEPS =
sig
  include RDEQUEP

  val size : 'a q -> int
end

module type CATENABLEDEQUEP =
sig
  include RDEQUEP

  val (++) : 'a q -> 'a q -> 'a q
end

module type RQUEUE =
sig
  type elt
  type t

  val empty   : t
  val isEmpty : t -> bool
  
  val snoc    : t * elt -> t
  val head    : t -> elt (* raises Empty if queue is empty *)
  val tail    : t -> t   (* raises Empty if queue is empty *)

  val print   : t -> unit
  val dprint  : bool -> t -> unit
end

module type RDEQUE =
sig
  include RQUEUE

  val cons : elt * t -> t
  val last : t -> elt
  val init : t -> t
end

module type CATENABLEDEQUE =
sig
  include RDEQUE

  val (++) : t -> t -> t
end
