module type RQUEUE =
sig
  type 'a queue
  
  exception Empty

  val empty : 'a queue
  val isEmpty : 'a queue -> bool
  
  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a (* raises Empty if queue is empty *)
  val tail : 'a queue -> 'a queue (* raises Empty if queue is empty *)

  val print : 'a queue -> unit
end

