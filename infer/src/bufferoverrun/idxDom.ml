module F = Format

module Idx = struct
  type t = Itv.t

  let compare = Itv.compare

  let leq ~lhs ~rhs = Itv.leq ~lhs:lhs ~rhs:rhs

  let join a b = Itv.join a b

  let widen ~prev ~next ~num_iters =
    Itv.widen ~prev:prev ~next:next ~num_iters:num_iters

  let bottom = Itv.bottom

  let to_string idx = Itv.to_string idx

  let pp fmt idx = Itv.pp fmt idx


  (*
  type t =
    { intidx: Itv.t }

  let leq ~lhs ~rhs = Itv.leq ~lhs:lhs ~rhs:rhs

  let join a b = Itv.join a b

  let widen ~prev ~next ~num_iters =
    Itv.widen ~prev:prev ~next:next ~num_iters:num_iters

  let bottom =
    { intidx: Itv.bottom }

  let pp fmt idx = Itv.pp fmt idx
  *)
end


