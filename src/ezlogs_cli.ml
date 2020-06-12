module Json = Yojson.Basic
module String_map = Map.Make (String)

let timestamp_of_tags_or_now (tags : Logs.Tag.set) =
  match Logs.Tag.find Ecs.tag tags with
  | None -> Ecs.Epoch.to_timestamp (Ptime_clock.now ())
  | Some ecs ->
    ( match Ecs.Fields.find (Base (Timestamp Ptime.epoch)) ecs with
    | None -> Ecs.Epoch.to_timestamp (Ptime_clock.now ())
    | Some timestamp -> Fmt.str "%a" Ecs.pp timestamp
    )

module Line_output = struct
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
      msgf @@ fun ?(header = "") ?(tags = Logs.Tag.empty) fmt ->
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

module Json_output = struct
  let labels_of_tags (tags : Logs.Tag.set) : Json.t String_map.t =
    Logs.Tag.fold
      (fun tag map ->
        match tag with
        | V (tag_definition, tag_value) ->
          let name = Logs.Tag.name tag_definition in
          let tag_string =
            Fmt.str "%a" (Logs.Tag.printer tag_definition) tag_value
          in
          String_map.update name (fun _v -> Some (`String tag_string)) map)
      tags String_map.empty

  let json_fields_of_tags (tags : Logs.Tag.set) : Json.t String_map.t =
    let fields = Ecs.fields_of_tags tags in
    let tags = Logs.Tag.rem Ecs.tag tags in
    let labels = labels_of_tags tags |> String_map.bindings in
    match labels with
    | [] -> fields
    | _ -> String_map.add "labels" (`Assoc labels) fields

  let add_basic_fields (fields : Json.t String_map.t) level src message =
    let add_if_new name thunk map =
      if String_map.mem name map then
        map
      else
        String_map.add name (thunk ()) map
    in
    let replace key value map =
      String_map.remove key map |> String_map.add key value
    in
    add_if_new "@timestamp"
      (fun () -> `String (Ecs.Epoch.to_timestamp (Ptime_clock.now ())))
      fields
    |> add_if_new "log.level" (fun () ->
           `String (Logs.level_to_string (Some level)))
    |> add_if_new "log.logger" (fun () -> `String (Logs.Src.name src))
    (* Always include the log message *)
    |> replace "message" (`String message)
    (* Always include the ECS version we're targeting *)
    |> replace "ecs.version" (`String Ecs.ecs_version)

  let reporter ppf =
    let report src level ~over k msgf =
      let continuation _ =
        over ();
        k ()
      in
      let as_json _header tags k ppf fmt =
        Fmt.kstr
          (fun message ->
            let fields =
              let ecs_fields = json_fields_of_tags tags in
              add_basic_fields ecs_fields level src message
            in
            let json : Json.t = `Assoc (String_map.bindings fields) in
            Fmt.kpf k ppf "%s" (Json.to_string json))
          fmt
      in
      msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
      as_json header tags continuation ppf fmt
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
  | Line -> Line_output.setup style_renderer level
  | Json -> Json_output.setup level

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
