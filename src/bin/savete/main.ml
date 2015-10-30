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

open Cmdliner


type copts =
  {
    logging_filter: [`Error|`Warning|`Info|`Debug] -> bool;
    cdry_run: bool;
    ini: filename;
    ini_d: filename;
  }


let copts_sect = "COMMON OPTIONS"
let help_secs =
  [`S copts_sect;
   `P "These options are common to all commands.";
   `S "MORE HELP";
   `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
   `Noblank;
   `P "Use `$(mname) help hook' for help on hook execution.";
   `S "BUGS";
   `P "Check bug reports at https://forge.ocamlcore.org/projects/savete/."]


let help copts man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None)
  | Some topic -> begin
      let topics = "topics" :: "hook" :: cmds in
      let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
        match conv topic with
        | `Error e -> `Error (false, e)
        | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
        | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
        | `Ok t ->
            let page = [] in
            `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)
    end


let t ?(asopts=fun _ -> true) copts =
  let t = load_configuration default ~dir:copts.ini_d copts.ini in
  let if_opt e = function Some e -> e | None -> e in
    {
      t with
          dry_run = copts.cdry_run;
          log = (fun lvl s ->
                   if copts.logging_filter lvl then
                     Printf.eprintf "%c: %s\n%!"
                       (List.assoc lvl
                          [`Debug, 'D';
                           `Info, 'I';
                           `Warning, 'W';
                           `Error, 'E'])
                       s);
    }


let dump copts =
  Savete.dump (t copts);
  `Ok ()


let restore copts =
  Savete.restore (t copts);
  `Ok ()


(*
 * Options common to all commands
 *)


let copts_t =
  let docs = copts_sect in
  (** Logging definition. *)
  let normal =
    function
      | `Warning | `Error -> true
      | `Debug | `Info -> false
  in
  let quiet =
    let doc = "No logging." in
      (fun _ -> false),
      Arg.info ["quiet"] ~docs ~doc
  in
  let verbose =
    let doc = "Verbose logging (up to info level)." in
      ((<>) `Debug),
      Arg.info ["verbose"] ~docs ~doc
  in
  let debug =
    let doc = "Debug logging (up to debug level)." in
      (fun _ -> true),
      Arg.info ["debug"] ~docs ~doc
  in
  let logging_filter =
    Arg.(last & vflag_all [normal] [quiet; verbose; debug])
  in
  (** Other common options. *)
  let dry_run =
    let doc = "Just display what should be done." in
      Arg.(value & flag & info ["dry_run"] ~docs ~doc)
  in
  let ini =
    let doc = "INI file to load for configuration." in
      Arg.(value & opt file Conf.ini & info ["ini"] ~docs ~doc)
  in
  let ini_d =
    let doc = "Directory containing INI files to load for configuration." in
      Arg.(value & opt dir Conf.ini_d & info ["ini_d"] ~docs ~doc)
  in
  let copts logging_filter dry_run dar now_rfc3339 ini ini_d =
    {
      logging_filter;
      cdry_run = dry_run;
      ini;
      ini_d
    }
  in
    Term.(pure copts $ logging_filter $ dry_run $ ini $ ini_d)


(*
 * Commands
 *)


let default_cmd =
  let doc = "Dump data for backup system." in
  let man = help_secs in
    Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
    Term.info "savete" ~version:Conf.version ~sdocs:copts_sect ~doc ~man


let help_cmd =
  let doc = "Display help about savete." in
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
      Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let man =
    [`S "DESCRIPTION";
     `P "Print help about savete specific subjects."]
    @ help_secs
  in
    Term.(ret (pure help $ copts_t $ man_format $ choice_names $ topic)),
    Term.info "help" ~doc ~man


let dump_cmd =
  let doc = "Dump new dataset for backup." in
  let man =
    [`S "DESCRIPTION";
     `P "Dump live data into a directory, ready to be restored."]
    @ help_secs
  in
  let dest =
    let doc = "Destination directory for dumping data." in
      Arg.(required & pos 0 (some dir) None & info [] ~docv:"DEST"
             ~doc)
  in
    Term.(ret (pure dump $ copts_t $ dest)),
    Term.info "dump" ~sdocs:copts_sect ~doc ~man


let restore_cmd =
  let doc = "Restore a dataset from a backup." in
  let man =
    [`S "DESCRIPTION";
     `P "Restore live data from a directory, previously created with $(b,dump)."]
    @ help_secs
  in
  let src =
    let doc = "Directory where dumped data are located." in
      Arg.(required & pos 0 (some dir) None & info [] ~docv:"SRC"
             ~doc)
  in
    Term.(ret (pure restore $ copts_t $ src)),
    Term.info "restore" ~sdocs:copts_sect ~doc ~man


let cmds = [
  dump_cmd;
  restore_cmd;
  help_cmd;
]


let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
