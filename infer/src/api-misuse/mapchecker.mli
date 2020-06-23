(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
  Mapdomain.RangeMemTable.t InterproceduralAnalysis.t -> Mapdomain.RangeMemTable.t option
