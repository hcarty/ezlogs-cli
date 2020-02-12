let now_fmt fmt () =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gmtime @@ Unix.gettimeofday ()
  in
  Fmt.pf fmt "%04d-%02d-%02d %02d:%02d:%02d " (tm_year + 1900) (tm_mon + 1)
    tm_mday tm_hour tm_min tm_sec

let pp_header =
  Fmt.suffix Fmt.(const string " ") @@ Fmt.prefix now_fmt Logs_fmt.pp_header

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  ()

(* Command line interface *)

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
