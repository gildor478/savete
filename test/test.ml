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

open OUnit2
open FileUtil
open Savete


let savete_exec = Conf.make_exec "darckup"


let test_dump test_ctxt =
  ()


let test_executable test_ctxt =
  ()


let tests =
  [
    "Dump" >:: test_dump;
    "Executable" >:: test_executable;
  ]


let () =
  run_test_tt_main
    ("Savete" >::: tests)
