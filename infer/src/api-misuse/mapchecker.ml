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
	module Domain = Mapdomain.MapDomain 

	type analysis_data = Domain.t InterproceduralAnalysis.t

	(* let exec_instr analysis_data astate _ (instr : Sil.instr) = *)
	let exec_instr astate analysis_data cfg(instr : Sil.instr) =

		Util.print_succ_nodes cfg;

		match instr with
		| Call (_, Const (Cfun callee), args, loc, _) ->
				astate
		| Load {e= exp; loc} | Prune (exp, loc, _, _) ->
				astate
		| Store {e1; e2; loc} ->
				astate
		| _ ->
				astate

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
  let result = Analyzer.compute_post analysis_data ~initial:Mapdomain.MapDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report analysis_data post) ;
  result
