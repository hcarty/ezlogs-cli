module Line = struct
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

  let setup style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (lwt_reporter ());
    ()

  let logging =
    Cmdliner.Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level ())
end

module Json = struct
  let reporter ppf =
    let buf_fmt ~like =
      let like = Lwt_fmt.get_formatter like in
      let b = Buffer.create 512 in
      ( Fmt.with_buffer ~like b,
        fun () ->
          let m = Buffer.contents b in
          Buffer.reset b;
          m )
    in
    let (out, out_flush) = buf_fmt ~like:ppf in
    let reporter = Ezlogs_cli.Json.reporter out in
    let report src level ~over k msgf =
      let k () =
        let write () = Lwt_io.write Lwt_io.stderr (out_flush ()) in
        let unblock () =
          over ();
          Lwt.return_unit
        in
        Lwt.finalize write unblock |> Lwt.ignore_result;
        k ()
      in
      reporter.report src level ~over:(fun () -> ()) k msgf
    in
    { Logs.report }

  let setup level =
    Logs.set_level level;
    Logs.set_reporter (reporter Lwt_fmt.stderr)

  let logging = Cmdliner.Term.(const setup $ Logs_cli.level ())
end

type format =
  | Line
  | Json

let setup (format : format) style_renderer level =
  match format with
  | Line -> Line.setup style_renderer level
  | Json -> Json.setup level

let format_conv : format Cmdliner.Arg.conv =
  Cmdliner.Arg.enum [ ("line", Line); ("json", Json) ]

let log_format default =
  let doc = "Log format" in
  let docv = "LOG_FORMAT" in
  Cmdliner.Arg.(
    value & opt format_conv default & info [ "log-format" ] ~doc ~docv)

let logging ~default =
  let format = log_format default in
  Cmdliner.Term.(
    const setup $ format $ Fmt_cli.style_renderer () $ Logs_cli.level ())
