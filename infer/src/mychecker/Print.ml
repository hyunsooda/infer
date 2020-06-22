(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PrintDomain.NullTable

  module T = Domain
  module M = Domain.M
  module N = M.N 
  module V = M.V

  type analysis_data = PrintDomain.NullTable.t InterproceduralAnalysis.t


  let is_null (exp : HilExp.t) = 
    match exp with
    | Constant const ->
        (match const with
        | Cint cint -> 
            if IntLit.isnull cint then true else false
        | _ -> false
        )
    | _ -> false

  let get_access_exp (exp : HilExp.t) =
    match exp with
    | AccessExpression ae -> Some ae
    | _ -> None

  let rec get_var (ae : HilExp.access_expression) =
    match ae with
    | Base (var, typ) -> Some (var, typ)
    | AddressOf ae' | Dereference ae' -> get_var ae'
    | _ -> None

  let var_pp_impl = function
    | None -> ()
    | Some (v, _) -> Var.pp F.std_formatter v

  let var_pp (ae : HilExp.access_expression) (exp : HilExp.t) =
    let lvar = get_var ae in
    var_pp_impl lvar;
    let rvar = get_access_exp exp in
    match rvar with
    | Some rvar ->
        var_pp_impl (get_var rvar)
    | None -> ()

  let is_deref (ae : HilExp.access_expression) loc astate =
    match ae with
    | Dereference ae ->
        Printf.printf "DEREF!!!!!!!!!!\n";
        (*Location.pp F.std_formatter loc; *)
        let base = HilExp.AccessExpression.get_base ae in
        Var.pp F.std_formatter (fst base);
        let a = M.find loc astate in
        let _ = if N.eq a N.bottom then () else Printf.printf "************************\n" in
        None
    | _ -> None

  let get_var_ae (ae : HilExp.access_expression) =
    match get_var ae with
    | None -> Stdlib.failwith "not expected"
    | Some (var, typ) -> (var, typ)

  let get_var_exp (exp : HilExp.t) =
    match get_access_exp exp with
    | None -> Stdlib.failwith "not expected"
    | Some ae ->
        (match get_var ae with
        | None -> Stdlib.failwith "not expected"
        | Some (var, typ) -> (var, typ))

  let print_pred_nodes cfg = 
    let node = CFG.Node.underlying_node cfg in
    let preds_list = Procdesc.Node.get_preds node in
    Printf.printf "%s\n" (Location.to_string (CFG.Node.loc cfg));
    Stdlib.List.iteri (fun i n ->
      Printf.printf "%d %s\n" i (Location.to_string (CFG.Node.of_underlying_node n |> CFG.Node.loc))
    ) preds_list

  let print_succ_nodes cfg = 
    let node = CFG.Node.underlying_node cfg in
    let preds_list = Procdesc.Node.get_succs node in
    Printf.printf "%s\n" (Location.to_string (CFG.Node.loc cfg));
    Stdlib.List.iteri (fun i n ->
      Printf.printf "%d %s\n" i (Location.to_string (CFG.Node.of_underlying_node n |> CFG.Node.loc))
    ) preds_list

  let get_next_labels cfg =
    let node = CFG.Node.underlying_node cfg in
    Procdesc.Node.get_succs node

  let is_nextlabel_ifstmt next_labels =
    Stdlib.List.fold_left (fun (acc, check) next_label ->
      let next_instrs = Procdesc.Node.get_instrs next_label in
      if Stdlib.(=) (Instrs.count next_instrs) 0 then
        let next_next_labels =
          get_next_labels (CFG.Node.of_underlying_node next_label)
        in
        if Stdlib.List.length next_next_labels > 0 then
          let next_label = Stdlib.List.hd next_next_labels in
          (next_label :: acc, true && check)
        else
          (acc, false)
      else (acc, false)
    ) ([], true) next_labels

  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : T.t)
      {InterproceduralAnalysis.proc_desc= procdesc; tenv= _; analyze_dependency= _; _} cfg
      (instr : HilInstr.t) =

    let table, _ = astate in
    let { Location.line } = CFG.Node.loc cfg in
    let cur_mem = T.find line table in
    let node = CFG.Node.underlying_node cfg in

    (*
    let _ =
        T.pp F.std_formatter astate
    in
    *)

    match instr with
    | Call (_return_opt, Direct _callee_procname, _actuals, _, _loc) ->
        astate
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        let lhs_var = get_var_ae _lhs_access_path in
        let lhs_varinfo = V.of_variable lhs_var in 
        let new_mem, cur_nullness =
          if is_null _rhs_exp then
            (M.add lhs_varinfo N.null cur_mem, N.null)
          else (* 직접적인 널 대입이 아닐때 *) 
            (* TODO : rhs_exp 를 evaluation햇을때 널인지 아닌지 체크 *)
            let rhs_var = get_var_exp _rhs_exp in
            let rhs_varinfo = V.of_variable rhs_var in 
            let cur_nullness = M.find rhs_varinfo cur_mem in
            let joined_nullness = N.join N.nonnull cur_nullness in
            (M.add lhs_varinfo joined_nullness cur_mem, joined_nullness)
        in
        let table = T.add line new_mem table in
        let next_labels = get_next_labels cfg in

        let table =
          Stdlib.List.fold_left (fun table next_label ->
            let next_loc =
              CFG.Node.of_underlying_node next_label |> CFG.Node.loc
            in
            (*
            let m =
              M.add (V.of_variable lhs_var) cur_nullness new_mem
              (*M.add (V.of_variable lhs_var) cur_nullness cur_mem *)
            in
            *)
            T.add next_loc.line new_mem table
          ) table next_labels
        in
        (*
        let next_instrs =
          Stdlib.List.fold_left (fun acc next_label ->
            let loc = CFG.Node.of_underlying_node next_label |> CFG.Node.loc |> Location.to_string in
            let instrs = Procdesc.Node.get_instrs next_label in
            Printf.printf "loc : %s, count %d \n" loc (Instrs.count instrs);
            (*
            let _ =
                Instrs.iter instrs ~f:(fun instr ->
                  (* Sil.pp_instr ~print_types:false Pp.text F.std_formatter instr; Printf.printf "\n"; *)
                  (match instr with
                  | Prune (_, _, _, _) -> Printf.printf "PRUNE!!\n"
                  | Store _ -> Printf.printf "STORE!\n"
                  | Call (_, _, _, _, _) -> Printf.printf "CALL\n"
                  | Metadata _ -> Printf.printf "METADATA\n" 
                  | _ ->
                      Printf.printf "loc : %s\n" loc
                  )
                )
            in
            *)
            instrs :: acc 
          ) [] next_labels
        in
        *)
        (table, next_labels)

    | Assume (_assume_exp, br, if_kind, _loc) ->
        (* propagation *)
        let table, _ = astate in
        let next_labels = get_next_labels cfg in
        let table =
          Stdlib.List.fold_left (fun table next_label ->
            let next_loc =
              CFG.Node.of_underlying_node next_label |> CFG.Node.loc
            in
            T.add next_loc.line cur_mem table
          ) table next_labels
        in
        (table, next_labels)

        (*
        let _ =
          (match br, if_kind with
          | `Then, Ik_if ->  
              let a =
                Stdlib.List.nth next_labels 0 |> CFG.Node.of_underlying_node |> CFG.Node.loc
              in
              Printf.printf "Then : %s\n" (Location.to_string a)
          | `Else, Ik_if ->
              let a =
                Stdlib.List.nth next_labels 1 |> CFG.Node.of_underlying_node |> CFG.Node.loc
              in
              Printf.printf "Then : %s\n" (Location.to_string a)
          | _ -> Stdlib.failwith "not support other if kind for now")
        in
        *)
       
        (* 
        let start = Procdesc.get_start_node procdesc in
        let node = CFG.Node.underlying_node cfg in
        Procdesc.set_succs node ~normal:(Some [start]) ~exn:(Some []);
        *)
        
        (* TODO: 여기서 best abstraction을 해야하는 듯*)
        (* TODO: 여기서 다음에 실행되어야 할 레이블을 지정할 수 있어야함. *)
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)

        (*
        let new_astate, cur_nullness =
          if is_null _assume_exp then
            (Domain.add _loc (Domain.N.null) astate, Domain.N.null)
          else
            let cur_nullness = Domain.find _loc astate in
            (Domain.add _loc cur_nullness astate, cur_nullness)
        in
        *)
        (*
        let node = CFG.Node.underlying_node cfg in
        let succs_list = Procdesc.Node.get_succs node in
        List.fold_left (fun acc succ ->
          CFG.Node.
        ) [] succs_list
        *)
        (*Domain.add _loc (Domain.N.nonnull) astate*)
        (* astate *)
    | Call (_, Indirect _, _, _, _) ->
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        (* propagation *)
        let next_labels = get_next_labels cfg in
        let next_next_labels, check = is_nextlabel_ifstmt next_labels in
        let table =
          Stdlib.List.fold_left (fun table next_label ->
            let next_loc =
              if check then 
                let next_label = Stdlib.List.hd next_next_labels in
                let next_loc = CFG.Node.of_underlying_node next_label |> CFG.Node.loc in
                (*  let _ = Printf.printf "LOC : %s\n" (Location.to_string next_loc) in *)
                next_loc
              else 
                let next_loc = CFG.Node.of_underlying_node next_label |> CFG.Node.loc in
                next_loc
            in
            let instrs = Procdesc.Node.get_instrs node in
            if Stdlib.(=) (Instrs.count instrs) 0 then
              table
            else
              T.add next_loc.line cur_mem table
          ) table next_labels
        in
        if check then
          (table, next_next_labels)
        else
          (table, next_labels)

  let pp_session_name _node fmt = F.pp_print_string fmt "print catch!"
end

(** 5(a) Type of CFG to analyze--Exceptional to follow exceptional control-flow edges, Normal to
    ignore them *)

module CFG = ProcCfg.Normal
(* Create an intraprocedural abstract interpreter from the transfer functions we defined *)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(* let a = get_func_name (Procdesc.get_proc_name proc_desc) in *)

(** Report an error when we have acquired more resources than we have released *)
let report_if_nullable {InterproceduralAnalysis.proc_desc; err_log; _} post =
  let module D = PrintDomain.NullTable in
  let module T = PrintDomain.NullTable.T in
  let module M = PrintDomain.NullTable.M.M in

  let table, _ = post in
  (* Printf.printf "LEN %d !\n" (PrintDomain.NullTable.T.cardinal table)  *)

  D.pp F.std_formatter post
  

  (*
  T.iter (fun line mem ->
    Printf.printf "LEN %d, line : %d !\n" (M.cardinal mem) (D.get_line line)
  ) table
  *)


    
(*
    if PrintDomain.NullnessMemory.N.is_nullable value then
      let loc = PrintDomain.NullnessMemory.get_loc loc in
      let loc_string = Location.to_string loc in
      (*let loc_string = PrintDomain.NullnessMemory.M. to_string loc in*)
      let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
      let message = F.asprintf "%a" PrintDomain.NullnessMemory.N.pp value in
      Reporting.log_error proc_desc err_log ~loc:loc PrintCapture
        IssueType.print_capture (message ^ loc_string)
  ) post
  *)
  
  
(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  (*let result = Analyzer.compute_post analysis_data ~initial:PrintDomain.NullnessMemory.initial proc_desc in*)
  let result = Analyzer.compute_post analysis_data ~initial:PrintDomain.NullTable.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_nullable analysis_data post) ;
  result
  
