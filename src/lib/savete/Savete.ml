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

type filename = string
type dirname = string

type t = {
  dry_run: bool;
  log: [`Debug | `Info | `Warning | `Error] -> string -> unit;
}


let default = {
  dry_run = false;
  log = (fun _ _ -> ());
}


let load_configuration t ?dir fn =
  t


let logf t lvl fmt =
  Printf.ksprintf (fun s -> t.log lvl s) fmt


let dump t dn =
  let cmd =
    {
      Command.default with
          Command.
          err =
            {(Command.Transfer.create (logf t `Warning "%s"))
               with
                   Command.Transfer.full_line = true};
          dry_run = t.dry_run;
    }
  in
  let capture cmd bin args bn =
    let fn = Filename.concat dn bn in
    let bin' =
      try
        FileUtil.which bin
      with Not_found ->
        failwith (Printf.sprintf "Unable to find executable %S." bin)
    in
    let tmpfn, fd =
      Filename.open_temp_file ~temp_dir:dn bn ".tmp"
    in
    let clean () =
      try
        Sys.remove tmpfn
      with e ->
        ()
    in
    let cmd =
      {cmd with
           Command.out = Command.Transfer.create (output_string fd)}
    in
      try
        logf t `Info "Run command '%s' and redirect output to %S."
          (Command.string_of_exec bin args) tmpfn;
        Command.exec cmd bin' args;
        close_out fd;
        if not t.dry_run then begin
          logf t `Info "Copy %S to %s." tmpfn fn;
          FileUtil.cp [tmpfn] fn;
        end;
        clean ()
      with e ->
        clean ();
        raise e
  in

  (* dpkg --get-selections *)
  let () =
    capture cmd
      "dpkg"
      Command.([A "--get-selections"])
      "dpkg-get-selections.txt"
  in

  (* mysqldump *)
  let () =
    capture cmd
      "mysqldump"
      Command.([Fn "--defaults-extra-file=/etc/mysql/debian.cnf";
       A "--all-databases";
       A "--add-drop-database";
       A "--add-drop-table";
       A "--events"])
      "mysqldump-all-databases.sql"
  in

  (* pg_dumpall *)
  let () =
    capture {cmd with Command.user = `User "postgres"}
      "pg_dumpall"
      Command.([A "--clean"; A "--no-password"])
      "pg_dumpall.sql"
  in

    ()


let restore t dn =
  failwith "Not implemented"
