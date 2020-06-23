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

  let get_next_labels cfg =
    let node = CFG.Node.underlying_node cfg in
    Procdesc.Node.get_succs node

  let update_cur_table new_mem cur_mem cur_line table =
    if M.leq new_mem cur_mem then table
    else T.add cur_line new_mem table

  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : T.t)
      {InterproceduralAnalysis.proc_desc= procdesc; tenv= _; analyze_dependency= _; _} cfg
      (instr : HilInstr.t) =

    let cur_table, _ = astate in
    let cur_loc = CFG.Node.loc cfg in
    let cur_mem = T.find cur_loc.line cur_table in
    let cur_node = CFG.Node.underlying_node cfg in
    let next_labels = get_next_labels cfg in

    match instr with
    | Call (_return_opt, Direct _callee_procname, _actuals, _, _loc) ->
        astate
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        let lhs_var = Util.get_var_ae _lhs_access_path in
        let lhs_varinfo = V.of_variable lhs_var in 
        let new_mem =
          if Util.is_null _rhs_exp then
            M.add lhs_varinfo N.null cur_mem
          else (* 직접적인 널 대입이 아닐때 *) 
            (* TODO : rhs_exp 를 evaluation햇을때 널인지 아닌지 체크 *)
            let rhs_var = Util.get_var_exp _rhs_exp in
            let rhs_varinfo = V.of_variable rhs_var in 
            let cur_nullness = M.find rhs_varinfo cur_mem in
            let joined_nullness = N.join N.nonnull cur_nullness in
            M.add lhs_varinfo joined_nullness cur_mem
        in
        (* let table = T.add cur_loc.line new_mem cur_table in *)
        let table = update_cur_table new_mem cur_mem cur_loc.line cur_table in
        let table =
          Stdlib.List.fold_left (fun table next_label ->
            let next_loc =
              CFG.Node.of_underlying_node next_label |> CFG.Node.loc
            in
            T.add next_loc.line new_mem table
          ) table next_labels
        in
        (table, next_labels)

    | Assume (_assume_exp, br, if_kind, _loc) ->
        (* propagation *)
        let table =
          Stdlib.List.fold_left (fun table next_label ->
            let next_loc =
              CFG.Node.of_underlying_node next_label |> CFG.Node.loc
            in
            T.add next_loc.line cur_mem table
          ) cur_table next_labels
        in
        (table, next_labels)
    | Call (_, Indirect _, _, _, _) ->
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        (* propagation *)
        let next_next_labels, check = Util.is_nextlabel_ifstmt next_labels in
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
            let instrs = Procdesc.Node.get_instrs cur_node in
            if Stdlib.(=) (Instrs.count instrs) 0 then
              table
            else
              T.add next_loc.line cur_mem table
          ) cur_table next_labels
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
  
