let timestamp epoch_time =
  (* NOTE: This assumes the host is UTC *)
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    Unix.gmtime epoch_time
  in
  let seconds = Float.of_int tm_sec +. fst (Float.modf epoch_time) in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%06.3fZ" (tm_year + 1900) (tm_mon + 1)
    tm_mday tm_hour tm_min seconds

module Line = struct
  let now_fmt fmt () =
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
      Unix.gmtime @@ Unix.gettimeofday ()
    in
    Fmt.pf fmt "%04d-%02d-%02d %02d:%02d:%02d " (tm_year + 1900) (tm_mon + 1)
      tm_mday tm_hour tm_min tm_sec

  let pp_header =
    Fmt.suffix Fmt.(const string " ") @@ Fmt.prefix now_fmt Logs_fmt.pp_header

  let setup style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
    ()

  let logging =
    Cmdliner.Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level ())
end

module Json = struct
  module Json = Yojson.Basic

  let dict_of_tags (tags : Logs.Tag.set) =
    Logs.Tag.fold
      (fun tag dict ->
        match tag with
        | V (tag_definition, tag_value) ->
          let name = Logs.Tag.name tag_definition in
          let tag_string =
            Fmt.str "%a" (Logs.Tag.printer tag_definition) tag_value
          in
          (name, `String tag_string) :: dict)
      tags []

  let reporter ppf =
    let report src level ~over k msgf =
      let continuation _ =
        over ();
        k ()
      in
      let as_json _header tags k ppf fmt =
        Fmt.kstr
          (fun message ->
            let tags_dict =
              match tags with
              | None -> []
              | Some set -> dict_of_tags set
            in
            let json : Json.t =
              `Assoc
                (("@timestamp", `String (timestamp (Unix.gettimeofday ())))
                :: ("log.level", `String (Logs.level_to_string (Some level)))
                :: ("log.logger", `String (Logs.Src.name src))
                :: ("message", `String message)
                :: tags_dict
                )
            in
            Fmt.kpf k ppf "%s" (Json.to_string json))
          fmt
      in
      msgf @@ fun ?header ?tags fmt -> as_json header tags continuation ppf fmt
    in
    { Logs.report }

  let setup level =
    Logs.set_level level;
    Logs.set_reporter (reporter Fmt.stderr)

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
