let now_fmt fmt () =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gmtime @@ Unix.gettimeofday ()
  in
  Fmt.pf fmt "%04d-%02d-%02d %02d:%02d:%02d " (tm_year + 1900) (tm_mon + 1)
    tm_mday tm_hour tm_min tm_sec

let pp_header =
  Fmt.suffix Fmt.(const string " ") @@ Fmt.prefix now_fmt Logs_fmt.pp_header

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let (app, app_flush) = buf_fmt ~like:Fmt.stdout in
  let (dst, dst_flush) = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~pp_header ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (lwt_reporter ());
  ()

(* Command line interface *)

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
