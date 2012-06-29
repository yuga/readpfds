open Ordered

module type SORTABLE =
sig
  module Elem : ORDERED
  
  type sortable

  val empty : sortable
  val add : Elem.t * sortable -> sortable
  val sort : sortable -> Elem.t list
end

