open Pp

let lts (x : Names.Id.t) : unit =
  let env = Global.env () in
  let _sigma = Evd.from_env env in
  Feedback.msg_notice (strbrk "Hello World")

  (* try *)
  (*   let t = Simple_print.simple_body_access (Nametab.global r) in *)
  (*   Feedback.msg_notice (Printer.pr_econstr_env env sigma t) *)
  (* with Failure s -> *)
  (*   CErrors.user_err (str s) *)
