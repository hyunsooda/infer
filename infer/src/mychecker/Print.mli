(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
  PrintDomain.NullTable.t InterproceduralAnalysis.t -> PrintDomain.NullTable.t option
  (*PrintDomain.NullnessMemory.summary InterproceduralAnalysis.t -> PrintDomain.NullnessMemory.summary option*)
