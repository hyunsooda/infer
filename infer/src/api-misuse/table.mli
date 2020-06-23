module F = Format

module type VARIABLE = sig
  type t = (Var.t * Typ.t)

  val compare : t -> t -> int 

  val of_variable : (Var.t * Typ.t) -> t

  val pp : F.formatter -> t -> unit

  val initial : t
end

module type MEMORY = sig
  module M : Stdlib.Map.S

  module V : VARIABLE

  module R : Domain.DOMAIN

  type t = R.t M.t

  include Domain.DOMAIN with type t := t 

  val add : V.t -> R.t -> t -> t

  val find : V.t -> t -> R.t
end

module type TABLE = sig
  type table_t

  type t = (table_t * Procdesc.Node.t list)

  include Domain.DOMAIN with type t := t

  module M : MEMORY

  module T : Stdlib.Map.S
 
  val add : int -> M.t -> table_t -> table_t
  
  val find : int -> table_t -> M.t
end
