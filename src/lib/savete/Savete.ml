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

module Driver =
struct
  type drv = {
    name: string;
    dir: dirname;
    test: FileUtil.test_file;
    bin: string;
    args: Command.arg list;
    user: Command.uid;
    output_basename: filename;
  }

  let has drv t =
    let b =
      logf t `Info "Testing if the system has data for driver %s" drv.name;
      FileUtil.find drv.test drv.dir
        (fun _ e ->
           logf t `Info "File %S match driver %s data pattern." e drv.name;
           true)
        false
    in
      if not b then begin
        logf t `Info "No data for driver %s, skipping it." drv.name
      end;
      b

  let dump drv t dn =
    let cmd =
      {
        Command.default with
            Command.
            err =
              {(Command.Transfer.create (logf t `Warning "%s"))
                 with
                     Command.Transfer.full_line = true};
            dry_run = t.dry_run;
            user = drv.user;
      }
    in
    let fn = Filename.concat dn drv.output_basename in
    let bin' =
      try
        FileUtil.which drv.bin
      with Not_found ->
        failwith (Printf.sprintf "Unable to find executable %S." drv.bin)
    in
    let tmpfn, fd =
      Filename.open_temp_file ~temp_dir:dn drv.output_basename ".tmp"
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
        Unix.chmod tmpfn 0o600;
        logf t `Info "Run command '%s' and redirect output to %S."
          (Command.string_of_exec bin' drv.args) tmpfn;
        Command.exec cmd bin' drv.args;
        close_out fd;
        if not t.dry_run then begin
          logf t `Info "Copy %S to %s." tmpfn fn;
          FileUtil.cp [tmpfn] fn;
          Unix.chmod fn 0o600;
        end;
        clean ()
      with e ->
        clean ();
        raise e

  let run drv t dn =
    if has drv t then begin
      try
        dump drv t dn
      with Failure msg ->
        logf t `Error "Cannot run driver %s: %s" drv.name msg
    end
end


let drivers = Driver.([
  {
    name = "dpkg-get-selections";
    dir = "/var/lib/dpkg";
    test = FileUtil.(And(Is_file, Basename_is "status"));
    user = `Same;
    bin = "dpkg";
    args =  Command.([A "--get-selections"]);
    output_basename = "dpkg-get-selections.txt";
  };
  {
    name = "mysqldump-all-databases";
    dir = "/var/lib/mysql";
    test = FileUtil.(And(Is_file, Has_extension "frm"));
    user = `Same;
    bin = "mysqldump";
    args = Command.([Fn "--defaults-extra-file=/etc/mysql/debian.cnf";
                     A "--all-databases";
                     A "--add-drop-database";
                     A "--add-drop-table";
                     A "--events"]);
    output_basename = "mysqldump-all-databases.sql";
  };
  {
    name = "pg_dumpall";
    dir = "/var/lib/postgresql";
    test = FileUtil.(And (Basename_is "postmaster.opts", Is_file));
    user = `User "postgres";
    bin = "pg_dumpall";
    args = Command.([A "--clean"; A "--no-password"]);
    output_basename = "pg_dumpall.sql";
  };
])


let dump t dn =
  List.iter (fun drv -> Driver.run drv t dn) drivers


let restore t dn =
  failwith "Not implemented"
