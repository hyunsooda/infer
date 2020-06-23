open AbsLoc
module F = Format
module Sem = BufferOverrunSemantics


(*
module CFG = BufferOverrunAnalysis.CFG
module Dom = BufferOverrunDomain

let get_all_cfg cfg =
  let nodes =
    CFG.fold_nodes cfg
      ~f: (fun acc node -> node :: acc)
      ~init:[]
  in
  Stdlib.List.rev nodes

let get_uniq_cfg cfg =
  let nodes =
    CFG.fold_nodes cfg
      ~f: (fun acc node ->
        let check =
          Stdlib.List.exists (fun x ->
            if Stdlib.(=) (CFG.Node.loc x |> Location.to_string) (CFG.Node.loc node |> Location.to_string) then
              true
            else false
          ) acc
        in
        if not check then node :: acc else acc
      )
      ~init:[]
  in
  Stdlib.List.rev nodes

let get_all_states cfg inv_map =
  CFG.fold_nodes cfg
    ~f: (fun acc node ->
      (*Stdlib.Printf.printf "loc: %s\n" (CFG.Node.loc node |> Location.to_string);*)
      match BufferOverrunAnalysis.extract_state (CFG.Node.id node) inv_map with
      | Some state -> state :: acc
      | None -> acc
    )
    ~init:[]

let get_uniq_states inv_map nodes =
  let states =
    Stdlib.List.fold_left (fun states node ->
      (* Stdlib.Printf.printf "loc: %s\n" (CFG.Node.loc node |> Location.to_string); *)
      match BufferOverrunAnalysis.extract_state (CFG.Node.id node) inv_map with
      | Some state -> state :: states
      | None -> states
    ) [] nodes
  in
  Stdlib.List.rev states

let print_states states nodes =
  Stdlib.List.iter2 (fun state node ->
    match state with
    | {AbstractInterpreter.State.pre= Dom.Mem.Reachable _ as pre; post} ->
         F.fprintf F.std_formatter "%a loc: %a\n\n\n" Dom.Mem.pp pre Location.pp (CFG.Node.loc node)
    | _ -> ()
  ) states nodes

let get_all_pre_mems inv_map nodes =
  let states =
    Stdlib.List.fold_left (fun states node ->
      match BufferOverrunAnalysis.extract_pre (CFG.Node.id node) inv_map with
      | Some state -> state :: states
      | None -> states
    ) [] nodes
  in
  Stdlib.List.rev states
*)

(*
let get_pvar_allocsite_str pvar =
  match Var.get_pvar pvar with
  | None -> "" 
  | Some pvar ->
      let v = Mem.find (Loc.of_pvar pvar) mem |> Val.get_all_locs in
*)

let get_pvar_str pvar =
  match Var.get_pvar pvar with
  | None -> "" 
  | Some pvar -> Pvar.get_simplified_name pvar

let get_id_str id =
  match Var.get_ident id with
  | None -> "" 
  | Some id -> Ident.to_string id

let get_allocsite_str (allocsite : Allocsite.t) =
  match allocsite with
  | Unknown -> "" 
  | Symbol sym -> 
      (match Symb.SymbolPath.get_pvar sym with
      | None -> ""
      | Some pvar -> Pvar.get_simplified_name pvar 
      )
  | Known site -> site.proc_name
  | LiteralString literal_str -> literal_str

let rec get_loc_str (loc : Loc.t) =
  match loc with
  | Prim prim  ->
      (match prim with
      | Var var -> 
          let pvar_str = get_pvar_str var in
          let id_str = get_id_str var in
          if String.equal pvar_str "" then
            if String.equal id_str "" then ""
            else id_str
          else pvar_str
      | Allocsite allocsite -> get_allocsite_str allocsite
      | Maploc maploc ->
            let allocsite, _ = maploc in 
            get_allocsite_str allocsite
      )
  | Field field -> Fieldname.to_string field.fn
  | StarField star_field ->
      get_loc_str star_field.prefix

let is_cpp_map str = String.is_prefix ~prefix:"std::map" str

let is_map exp mem =
	let exp_locs = Sem.eval_locs exp mem in
	let loc_set = AbsLoc.PowLoc.to_set exp_locs in
	match AbsLoc.LocSet.elements loc_set |> List.hd with
	| None -> false
	| Some loc ->
			(*Printf.printf "is_map %s %b \n" (Util.get_loc_str loc) (Util.get_loc_str loc |> Util.is_cpp_map);*)
			get_loc_str loc |> is_cpp_map
