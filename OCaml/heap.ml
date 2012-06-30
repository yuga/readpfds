open Ordered

module type HEAP =
sig
  module Elem : ORDERED

  type heap

  val empty   : heap
  val isEmpty : heap -> bool
  val insert  : Elem.t * heap -> heap
  val merge   : heap * heap -> heap

  val findMin   : heap -> Elem.t
  val deleteMin : heap -> heap

  val print : heap -> unit
end

