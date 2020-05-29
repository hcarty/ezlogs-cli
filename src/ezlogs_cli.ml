module Common_tags = struct
  let epoch_to_timestamp epoch_time =
    (* NOTE: This assumes the host is UTC *)
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
      Unix.gmtime epoch_time
    in
    let seconds = Float.of_int tm_sec +. fst (Float.modf epoch_time) in
    Fmt.str "%04d-%02d-%02dT%02d:%02d:%06.3fZ" (tm_year + 1900) (tm_mon + 1)
      tm_mday tm_hour tm_min seconds

  let pp_epoch = Fmt.of_to_string epoch_to_timestamp

  let epoch =
    Logs.Tag.def ~doc:"Unix epoch to use as the log time" "epoch" pp_epoch
end

let timestamp_of_tags_or_now (tags : Logs.Tag.set option) =
  let epoch =
    match tags with
    | None -> Unix.gettimeofday ()
    | Some tags ->
      ( match Logs.Tag.find Common_tags.epoch tags with
      | None -> Unix.gettimeofday ()
      | Some epoch -> epoch
      )
  in
  Fmt.str "%a" (Logs.Tag.printer Common_tags.epoch) epoch

module Line = struct
  let reporter ppf =
    let report _src level ~over k msgf =
      let continuation _ =
        over ();
        k ()
      in
      let write header tags k ppf fmt =
        Fmt.kstr
          (fun message ->
            let timestamp = timestamp_of_tags_or_now tags in
            let level =
              String.uppercase_ascii (Logs.level_to_string (Some level))
            in
            Fmt.kpf k ppf "%s%s [%s] %s@." header timestamp level message)
          fmt
      in
      msgf @@ fun ?(header = "") ?tags fmt ->
      write header tags continuation ppf fmt
    in
    { Logs.report }

  let setup style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (reporter Fmt.stderr);
    ()

  let logging =
    Cmdliner.Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level ())
end

module Json = struct
  module Json = Yojson.Basic

  let dict_of_tags (tags : Logs.Tag.set) =
    let tags = Logs.Tag.rem Common_tags.epoch tags in
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
                (("@timestamp", `String (timestamp_of_tags_or_now tags))
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
