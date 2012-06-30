module type ITEM =
sig
  type t
  val print : t -> unit
end
