(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module F = Format

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

module Nullness : NULLNESS = struct
  type t = Bottom | Null | Nonnull | Top

  let bottom = Bottom

  let null = Null

  let nonnull = Nonnull

  let top = Top

  let leq ~lhs ~rhs =
    if Stdlib.( = ) lhs rhs then true
    else
      match lhs, rhs with
      | Bottom, _ | _, Top -> true
      | _, _ -> false

  let join a b =
    if Stdlib.( = ) a b then a
    else
      match a, b with
      | Bottom, _ -> b
      | _, Bottom -> a
      | _, _ -> Top

  let widen ~prev ~next ~num_iters:_ = join prev next

  let eq a b = if Stdlib.( = ) a b then true else false

  let is_null a = eq a Null

  let is_nullable a =
    if eq a Null || eq a Top then true else false

  let pp fmt astate =
    match astate with
    | Bottom -> F.fprintf fmt "(Bottom)\n"
    | Null -> F.fprintf fmt "(Null)\n"
    | Nonnull -> F.fprintf fmt "(Nonnull)\n"
    | Top -> F.fprintf fmt "(Potential\x1B[33m Nullable \x1B[0m)\n"

  let initial = Bottom

  type summary = t
end

module Variable : VARIABLE = struct
  (*type t = Location.t * (Var.t * Typ.t)*)
  type t = (Var.t * Typ.t)

  let compare = Stdlib.compare

  (*let of_variable loc varinfo = (loc, varinfo)*)
  let of_variable varinfo = varinfo

  let pp fmt variable =
    let (var, typ) = variable in
    let typ_pp = Typ.pp Pp.text in
    (*
    F.fprintf fmt "loc : %a,  var : %a, typ : %a " 
    Location.pp loc Var.pp var typ_pp typ
    *)
    F.fprintf fmt "var : %a, typ : %a " 
    Var.pp var typ_pp typ


  (* dummy(loc) * null ident(var) * null type(typ) *)
  (*let initial = (Location.dummy, ((Ident.create_none () |> Var.of_id ), Typ.void ))*)
  let initial = (((Ident.create_none () |> Var.of_id ), Typ.void ))
end

module Memory (Nullness : NULLNESS) : MEMORY = struct
  module M = Stdlib.Map.Make (Variable)

  module N = Nullness

  module V = Variable

  type t = N.t M.t

  let bottom = M.empty

  let add = M.add

  let find var astate =
    try M.find var astate with Stdlib.Not_found -> N.bottom

  let get_vars m1 m2 =
    let vars = M.fold (fun var _ acc -> var :: acc) m1 [] in
    M.fold (fun var _ acc ->
      if Stdlib.List.mem var acc then var :: acc else acc
    ) m2 vars
    
  let leq ~lhs ~rhs =
    let vars = get_vars lhs rhs in
    Stdlib.List.fold_left (fun acc var ->
      let v1 = M.find_opt var lhs in
      let v2 = M.find_opt var rhs in
      let check =
        match v1, v2 with
        | None, None -> true
        | None, Some _ -> true
        | Some _, None -> false
        | Some v1', Some v2' -> N.leq ~lhs:v1' ~rhs:v2'
      in
      check && acc
    ) true vars 

  let join a b =
    let bigger, smaller =
      if M.cardinal a > M.cardinal b then a,b else b,a
    in
    M.fold (fun var value new_mem ->
      match M.find_opt var new_mem with
      | None -> add var value new_mem
      | Some v ->
          let new_v = N.join v value in
          add var new_v new_mem
    ) bigger smaller

  let widen ~prev ~next ~num_iters:_ = join prev next

  let initial = bottom

  let pp fmt m =
    M.iter (fun var value ->
      Variable.pp fmt var;
      Nullness.pp fmt value) m

      (*
  let report_alarm fmt m =
    M.iter (fun _ value ->
      if Nullness.is_nullable value then Nullness.pp fmt value
      else ()
    ) m
    *)
end

module CFG = ProcCfg.Normal

module NullTable : TABLE = struct
  module M = Memory (Nullness)

  module T = Stdlib.Map.Make (Int)

  type table_t = M.t T.t

  type t = (table_t * Procdesc.Node.t list)

  let initial = (T.empty, [])

  let get_line line = line

  let find line_num table =
    try T.find line_num table with Not_found -> M.bottom

  let add line m table = T.add line m table

  let must_same next_lines1 next_lines2 =
    let is_same =
      Stdlib.List.fold_left2 (fun check next_line1 next_line2 ->
        (Procdesc.Node.equal next_line1 next_line2) && check
      ) true next_lines1 next_lines2
    in
    if is_same then () else Stdlib.failwith "next labels are differnet"

  let leq ~lhs ~rhs = 
    let lhs_table, lhs_next_lines = lhs in
    let rhs_table, rhs_next_lines = rhs in
    must_same lhs_next_lines rhs_next_lines;

    Stdlib.List.for_all (fun next_line ->
      let {Location.line} = CFG.Node.of_underlying_node next_line |> CFG.Node.loc in
      let lhs_mem = find line lhs_table in
      let rhs_mem = find line rhs_table in
      M.leq ~lhs:lhs_mem ~rhs:rhs_mem
    ) lhs_next_lines

  let get_all_lines table =
    T.fold (fun line _ lines -> line :: lines) table []

  let pp fmt table_cfg =
    let table, _ = table_cfg in
    T.iter (fun line m -> 
      F.fprintf fmt "%d: %a" line M.pp m) table
    
  let join a b =
    let lhs_table, lhs_next_lines = a in
    let rhs_table, rhs_next_lines = b in
    must_same lhs_next_lines rhs_next_lines;

    let new_table =
      Stdlib.List.fold_left (fun table next_line ->
        let {Location.line} = CFG.Node.of_underlying_node next_line |> CFG.Node.loc in
        Printf.printf "join line : %d\n" line;
        let lhs_mem = find line lhs_table in
        let rhs_mem = find line rhs_table in
        let new_mem = M.join lhs_mem rhs_mem in
        add line new_mem table 
      ) lhs_table lhs_next_lines
    in
    let rhs_table_lines = get_all_lines rhs_table in
    let new_table =
      Stdlib.List.fold_left (fun new_table rhs_line ->
        let rhs_mem = find rhs_line rhs_table in
        let is_bottom = find rhs_line new_table in
        if Stdlib.(=) is_bottom M.bottom then
          add rhs_line rhs_mem new_table
        else
          new_table
      ) new_table rhs_table_lines
    in
    (new_table, [])

  let widen ~prev ~next ~num_iters:_ = join prev next

end

include NullTable 

(*
module NullnessMemory = Memory (Nullness) 

include NullnessMemory
*)
