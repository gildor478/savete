(*************************************************************************)
(* savete: Data dumper for backup system.                                *)
(*                                                                       *)
(* Copyright (C) 2015, Sylvain Le Gall                                   *)
(*                                                                       *)
(* This program is free software: you can redistribute it and/or modify  *)
(* it under the terms of the GNU General Public License as published by  *)
(* the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                   *)
(*                                                                       *)
(* This program is distributed in the hope that it will be useful,       *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(* GNU General Public License for more details.                          *)
(*                                                                       *)
(* You should have received a copy of the GNU General Public License     *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*************************************************************************)

type dirname = string

(** Configuration for Savete high level commands. *)
type t = {
  (** Don't run, just simulate running.
    *)
  dry_run: bool;

  (** System interface.
    *)
  log: [`Debug | `Info | `Warning | `Error] -> string -> unit;
}

(** Dump data to a directory. *)
val dump: t -> dirname -> unit

(** Restore data from a directory. *)
val restore: t -> dirname -> unit
