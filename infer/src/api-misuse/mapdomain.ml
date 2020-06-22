module MapDomain : Domain.DOMAIN = struct 
  type t = int 
  
  let leq ~lhs ~rhs = true 

  let join a b = a

  let widen ~prev ~next ~num_iters:_ = next

  let pp fmt mapd = () 

  let initial = 0
end
