module F = Format

module type DOMAIN = sig
  type t

  val leq : lhs:t -> rhs:t -> bool 

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:'a -> t 

  val pp : F.formatter -> t -> unit

  val bottom : t

  val initial : t
end
