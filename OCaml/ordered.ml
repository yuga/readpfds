module type ORDERED =
sig
  type t

  val eq  : t * t -> bool
  val lt  : t * t -> bool
  val leq : t * t -> bool

  val dprint : t -> unit
end

module Int : ORDERED with type t = int =
struct
  type t = int

  let eq  (x,y) = x == y
  let lt  (x,y) = x <  y
  let leq (x,y) = x <= y

  let dprint = print_int
end
