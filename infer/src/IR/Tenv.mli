(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Type Environments. *)

(** Type for type environment. *)
type t

val create : unit -> t
(** Create a new type environment. *)

val load : SourceFile.t -> t option
(** Load a type environment for a source file *)

val store_debug_file_for_source : SourceFile.t -> t -> unit

val read : DB.filename -> t option
(** read and return a type environment from the given file *)

val load_global : unit -> t option
(** load the global type environment (Java) *)

val store_global : t -> unit
(** save a global type environment (Java) *)

val lookup : t -> Typ.Name.t -> Struct.t option
(** Look up a name in the global type environment. *)

val mk_struct :
     t
  -> ?default:Struct.t
  -> ?fields:Struct.fields
  -> ?statics:Struct.fields
  -> ?methods:Procname.t list
  -> ?exported_objc_methods:Procname.t list
  -> ?supers:Typ.Name.t list
  -> ?annots:Annot.Item.t
  -> ?java_class_info:Struct.java_class_info
  -> ?dummy:bool
  -> Typ.Name.t
  -> Struct.t
(** Construct a struct_typ, normalizing field types *)

val add_field : t -> Typ.Name.t -> Struct.field -> unit
(** Add a field to a given struct in the global type environment. *)

val pp : Format.formatter -> t -> unit [@@warning "-32"]
(** print a type environment *)

type per_file = Global | FileLocal of t

val pp_per_file : Format.formatter -> per_file -> unit
  [@@warning "-32"]
(** print per file type environment *)

val merge : src:t -> dst:t -> unit
(** Merge [src] into [dst] possibly overwriting pre existing procs in [dst]. *)

val merge_per_file : src:per_file -> dst:per_file -> per_file
(** Best-effort merge of [src] into [dst]. If a procedure is both in [dst] and [src], the one in
    [dst] will get overwritten. *)

val get_summary_formals :
     t
  -> get_summary:(Procname.t -> 'summary option)
  -> get_formals:(Procname.t -> 'formals option)
  -> Procname.t
  -> [ `NotFound
     | `Found of 'summary * 'formals
     | `FoundFromSubclass of Procname.t * 'summary * 'formals ]
(** Get summary and formals of the given proc name with heuristics for finding a method in
    non-abstract sub-classes

    - If the class is an interface: Find its unique sub-class and apply the heuristics recursively.

    - If the class is an abstract class: Find/use its own summary if possible. If not found, find
      one (arbitrary but deterministic) summary from its sub-classes.

    - Otherwise: Find its own summary. *)

module SQLite : SqliteUtils.Data with type t = per_file
