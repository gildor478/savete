
(* TODO: merge with darckup/Command into a separate library. *)

type dirname = string
type filename = string
type arg = A of string | Fn of filename


(* When doing syscall, we can receive an EINTR, we should retry the call in this
   call.
 *)
let rec loop_non_intr f =
  try
    f ()
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    loop_non_intr f


(* TODO: Transfer.to_file... *)
module Transfer =
struct
  type t = {
    f: string -> unit;
    full_line: bool;
    buffer: string;
    remainder: Buffer.t;
  }

  let create ?(full_line=false) f =
    {
      f; full_line;
      buffer = String.create 4096;
      remainder = Buffer.create 13;
    }

  let input t fd =
    let read =
      loop_non_intr
        (fun () ->
           Unix.read fd t.buffer 0 (String.length t.buffer))
    in
    if read <= 0 then begin
      if Buffer.length t.remainder > 0 then begin
        t.f (Buffer.contents t.remainder);
        Buffer.reset t.remainder
      end;
      raise End_of_file;
    end else begin
      let str =
        if Buffer.length t.remainder = 0 then begin
          String.sub t.buffer 0 read
        end else begin
          Buffer.add_substring t.remainder t.buffer 0 read;
          Buffer.contents t.remainder
        end
      in
        Buffer.clear t.remainder;
        if t.full_line then begin
          let rec split_line from =
            if from < String.length str then begin
              try
                let i = String.index_from str from '\n' in
                  (* Drop the last char. *)
                  if from = i then
                    t.f ""
                  else
                    t.f (String.sub str from i);
                  split_line (i + 1)
              with Not_found ->
                Buffer.add_substring t.remainder str
                  from ((String.length str) - from)
            end
          in
            split_line 0
        end else begin
          t.f str
        end
    end
end

module Process =
struct
  type t = {
    pid: int;
    stdout: Unix.file_descr;
    stdin: Unix.file_descr;
    stderr: Unix.file_descr;
  }

  let create env chdir euid bin cli =
    let chdn = chdir in
    let open Unix in
    let fds_to_close = ref [] in
    let mkpipe () =
      let fd_read, fd_write = pipe () in
        fds_to_close :=  fd_read :: fd_write :: !fds_to_close;
        fd_read, fd_write
    in
    let replace_fd fd1 fd2 =
        dup2 fd1 fd2; close fd1
    in
    try
      let in_read, in_write = mkpipe () in
      let out_read, out_write = mkpipe () in
      let err_read, err_write = mkpipe () in
      (* Temporary channel to communicate exception in case of early failure. *)
      let tmp_read, tmp_write = mkpipe () in
      let close_in_child = [in_read; out_write; err_read] in
      let close_in_child_on_exec =
        List.for_all
          (fun fd ->
             try
               set_close_on_exec fd;
               true
             with _ ->
               false)
          close_in_child
      in
        match fork () with
        | 0 ->
            (* In the child process. *)
            replace_fd out_read stdin;
            replace_fd in_write stdout;
            replace_fd err_write stderr;
            if not close_in_child_on_exec then begin
              List.iter close close_in_child
            end;
            close tmp_read;
            begin
              let tmp_chn = out_channel_of_descr tmp_write in
              let v =
                try
                  begin
                    match chdn with
                    | Some dn -> chdir dn
                    | None -> ()
                  end;
                  begin
                    match euid with
                    | Some i -> setuid i
                    | None -> ()
                  end;
                  `Ok;
                with e ->
                  `Fail e
              in
                Marshal.to_channel tmp_chn v [];
                close_out tmp_chn;
            end;
            begin
              try
                execve bin cli env
              with _ ->
                exit 127
            end
        | pid ->
            (* In the parent. *)
            close out_read;
            close in_write;
            close err_write;
            close tmp_write;
            begin
              let tmp_chn = Unix.in_channel_of_descr tmp_read in
              match Marshal.from_channel tmp_chn with
              | `Ok ->
                  close_in tmp_chn
              | `Fail e ->
                  close_in tmp_chn;
                  raise e
            end;
            {
              pid;
              stdout = in_read;
              stdin = out_write;
              stderr = err_read;
            }
    with e ->
      List.iter close !fds_to_close;
      raise e

  let close t =
    let open Unix in
      List.iter
        (fun fd ->
           try
             close t.stdout
           with _ -> ())
        [t.stdin; t.stderr; t.stdout];
      loop_non_intr (fun () -> snd(waitpid [] t.pid))
end


type uid = [ `User of string | `Uid of int | `Same ]

type 'a t = {
  env: string array;
  err: Transfer.t;
  out: Transfer.t;
  exitf: string -> int -> 'a;
  user: uid;
  chdir: dirname option;
  dry_run: bool;
}


let default =
  {
    err = { (Transfer.create prerr_endline) with Transfer.full_line = true };
    out = Transfer.create print_string;
    env = Unix.environment ();
    exitf = (fun cmd i ->
               if i <> 0 then
                 failwith
                   (Printf.sprintf
                      "Command %S exited with error code %d." cmd i));
    user = `Same;
    chdir = None;
    dry_run = false;
  }


let command t cmd =
  if t.dry_run then begin
    t.exitf cmd 0
  end else begin
    let euid =
      match t.user with
      | `User s ->
          begin
            try
              Some (Unix.getpwnam s).Unix.pw_uid
            with Not_found ->
              failwith (Printf.sprintf "User %S not found." s)
          end
      | `Uid i -> Some i
      | `Same -> None
    in
    let proc =
      Process.create t.env t.chdir euid "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
    in
    let rec run_until_eof l =
      if l = [] then begin
        match Process.close proc with
        | Unix.WEXITED i ->
            t.exitf cmd i
        | Unix.WSIGNALED n ->
            failwith
              (Printf.sprintf "Command %S killed by signal %d." cmd n)
        | Unix.WSTOPPED n ->
            failwith
              (Printf.sprintf "Command %S stopped by signal %d." cmd n)
      end else begin
        let ready, _, _ =
          loop_non_intr
            (fun () -> Unix.select l [] [] 1.0)
        in
        let l =
          List.fold_left
            (fun l fd ->
               try
                 if fd = proc.Process.stderr then
                   Transfer.input t.err proc.Process.stderr
                 else
                   Transfer.input t.out proc.Process.stdout;
                 l
               with End_of_file ->
                 List.filter ((<>) fd) l)
            l ready
        in
          run_until_eof l
      end
    in
      Unix.close proc.Process.stdin;
      run_until_eof [proc.Process.stdout; proc.Process.stderr]
  end


let string_of_exec bin args =
  let l =
    List.map
      (function
         | A s -> s
         | Fn fn -> Filename.quote fn)
      (Fn bin :: args)
  in
    String.concat " " l


let exec t bin args = command t (string_of_exec bin args)
