(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Mapdomain

module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
	module CFG = CFG
	module Domain = Mapdomain.RangeMemTable

  module D = Domain
  module T = Domain.T

	type analysis_data = Domain.t InterproceduralAnalysis.t

	(* let exec_instr analysis_data astate _ (instr : Sil.instr) = *)
	let exec_instr astate analysis_data cfg(instr : Sil.instr) =

    let cur_table, _ = astate in
    let cur_loc = CFG.Node.loc cfg in
    let cur_mem = D.find cur_loc.line cur_table in
    let cur_node = CFG.Node.underlying_node cfg in

		match instr with
    | Call ((ident, typ), e, args, loc, callflag) ->
        (* Printf.printf "Call loc : %s,  exp: %s\n" (Location.to_string loc) (Exp.to_string e); *)
        
        let _ =
          (match Util.get_func_info e with
          | None -> ()
          | Some {class_name; kind; method_name; parameters; template_args} ->
              Printf.printf "method name :%s\n" method_name
          )
        in

        

        (* which_type_pp e; *)
				astate
    | Load {id; e; root_typ; typ; loc} ->
        (* Printf.printf "(%s): %s [%s]\n" (Location.to_string loc) (Exp.to_string e) (Typ.to_string root_typ);*)
        (* Printf.printf "load loc : %s\n" (Location.to_string loc); *)
				astate
		| Store {e1; e2; loc} ->
				astate
		| Prune (exp, loc, boolean, if_kind) ->
				astate
		| Metadata _ ->
				astate
    | _ -> Stdlib.failwith "not supported for now"

	let pp_session_name _node fmt = F.pp_print_string fmt "nothing.."

end

(* module CFG = ProcCfg.OneInstrPerNode (ProcCfg.Backward (ProcCfg.Exceptional)) *)

module CFG = ProcCfg.Normal

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

let report {InterproceduralAnalysis.proc_desc; err_log; _} post =
	()

(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  (*let result = Analyzer.compute_post analysis_data ~initial:PrintDomain.NullnessMemory.initial proc_desc in*)
  let result = Analyzer.compute_post analysis_data ~initial:Mapdomain.RangeMemTable.initial proc_desc in
  Option.iter result ~f:(fun post -> report analysis_data post) ;
  result
