(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module CFG = ProcCfg.Normal

module type NULLNESS = sig 
  type t = Bottom | Null | Nonnull | Top 

  include AbstractDomain.S with type t:= t

  val top : t

  val bottom : t

  val null : t

  val nonnull : t

  val widen : prev:t -> next:t -> num_iters:'a -> t 

  val join : t -> t -> t

  val leq : lhs:t -> rhs:t -> bool 

  val pp : Format.formatter -> t -> unit

  val eq : t -> t -> bool

  val is_null : t -> bool

  val is_nullable : t -> bool

  val initial : t

  type summary = t 
end

module type VARIABLE = sig
  (*type t = Location.t * (Var.t * Typ.t)*)
  type t = (Var.t * Typ.t)

  val compare : t -> t -> int

  (*val of_variable : Location.t -> (Var.t * Typ.t) -> t*)
  val of_variable : (Var.t * Typ.t) -> t

  val pp : Format.formatter -> t -> unit

  (* null ident * null type *)
  val initial : t 
end

module type MEMORY = sig
  module M : Stdlib.Map.S 

  module N : NULLNESS

  module V : VARIABLE
  
  type t = N.t M.t
  
  include AbstractDomain.S with type t:= t

  val bottom : t

  val add : V.t -> N.t -> t -> t

  val find : V.t -> t -> N.t
  
  val widen : prev:t -> next:t -> num_iters:'a -> t 

  val join : t -> t -> t

  val leq : lhs:t -> rhs:t -> bool 

  val pp : Format.formatter -> t -> unit

  val initial : t
end

module type TABLE = sig
  module M : MEMORY 

  module T : Stdlib.Map.S

  type table_t = M.t T.t

  type t = (table_t * Procdesc.Node.t list)

  include AbstractDomain.S with type t:= t

  val get_line : T.key -> int

  val find : int -> table_t -> M.t

  val add : int -> M.t -> table_t -> table_t 

  val widen : prev:t -> next:t -> num_iters:'a -> t 

  val join : t -> t -> t 

  val leq : lhs:t -> rhs:t -> bool 

  val initial : t 

  val pp : Format.formatter -> t -> unit
end

module NullTable : TABLE

(* module NullnessMemory : MEMORY  *)
