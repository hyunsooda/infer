module F = Format

module CFG = ProcCfg.Normal

module RangeDomain : Domain.DOMAIN = struct
  type t = int list

  let leq ~lhs ~rhs =
    Stdlib.List.fold_left (fun check x ->
      (Stdlib.List.mem x rhs) && check
    ) true lhs

  let join a b = Stdlib.List.sort_uniq Stdlib.compare (a @ b)

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt range =
    Stdlib.List.iter (fun x -> F.fprintf fmt "%d," x) range

  let bottom = []

  let initial = bottom
end

module Variable : Table.VARIABLE = struct
  type t = (Var.t * Typ.t)

  let compare = Stdlib.compare

  let of_variable varinfo = varinfo

  let pp fmt variable =
    let (var, typ) = variable in
    let typ_pp = Typ.pp Pp.text in
    F.fprintf fmt "var : %a, typ : %a " Var.pp var typ_pp typ

  (* dummy(loc) * null ident(var) * null type(typ) *)
  let initial = (((Ident.create_none () |> Var.of_id ), Typ.void ))
end

module Memory : Table.MEMORY = struct
  module M = Stdlib.Map.Make (Variable)

  module V = Variable
  
  module R = RangeDomain

  type t = R.t M.t

  let bottom = M.empty

  let add = M.add

  let find var astate =
    try M.find var astate with Stdlib.Not_found -> R.bottom

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
        | Some v1', Some v2' -> R.leq ~lhs:v1' ~rhs:v2'
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
          let new_v = R.join v value in
          add var new_v new_mem
    ) bigger smaller

  let widen ~prev ~next ~num_iters:_ = join prev next

  let initial = bottom

  let pp fmt m =
    M.iter (fun var value ->
      Variable.pp fmt var;
      R.pp fmt value) m
end

module RangeMemTable : Table.TABLE = struct
  module M = Memory

  module T = Stdlib.Map.Make (Int)

  type table_t = M.t T.t

  type t = (table_t * Procdesc.Node.t list)

  let bottom = (T.empty, [])

  let initial = bottom

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

  let get_all_lines table =
    T.fold (fun line _ lines -> line :: lines) table []

  let pp fmt table_cfg =
    let table, _ = table_cfg in
    T.iter (fun line m -> 
      F.fprintf fmt "%d: %a" line M.pp m) table

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
