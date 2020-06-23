module F = Format

module Idx : sig
  (* TODO
  (string * float * int * pointer * object * object pointer)
  *)
  type t = Itv.t 

  val compare : t -> t -> int
  
  val leq : lhs:t -> rhs:t -> bool

  val join : t -> t -> t

  val widen : prev:t -> next:t -> num_iters:int -> t

  val bottom : t

  val to_string : t -> string

  val pp : F.formatter -> t -> unit
end
