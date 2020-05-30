module Json = Yojson.Basic
module String_map = Map.Make (String)

module Epoch = struct
  let to_timestamp epoch_time =
    (* NOTE: This assumes the host is UTC *)
    let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
      Unix.gmtime epoch_time
    in
    let seconds = Float.of_int tm_sec +. fst (Float.modf epoch_time) in
    Fmt.str "%04d-%02d-%02dT%02d:%02d:%06.3fZ" (tm_year + 1900) (tm_mon + 1)
      tm_mday tm_hour tm_min seconds

  let pp_timestamp = Fmt.of_to_string to_timestamp
end

module type Ecs_field_def = sig
  type t
  val to_name : t -> string
  val pp : t Fmt.t
  val to_json : t -> Json.t
end

module Ecs = struct
  module Base = struct
    type t =
      | Timestamp of float
      | Tags of string list
      | Labels of (string * string) list

    let to_name (field : t) =
      match field with
      | Timestamp _ -> "@timestamp"
      | Tags _ -> "tags"
      | Labels _ -> "labels"

    let pp ppf (field : t) =
      match field with
      | Timestamp epoch -> Epoch.pp_timestamp ppf epoch
      | Tags tags -> Fmt.(Dump.list string) ppf tags
      | Labels labels -> Fmt.(Dump.list (Dump.pair string string)) ppf labels

    let to_json (field : t) : Json.t =
      match field with
      | Timestamp epoch -> `String (Epoch.to_timestamp epoch)
      | Tags tags -> `List (List.map (fun tag -> `String tag) tags)
      | Labels labels -> `Assoc (List.map (fun (k, v) -> (k, `String v)) labels)
  end
  module _ : Ecs_field_def = Base

  module Error = struct
    type t =
      | Code of string
      | Id of string
      | Message of string
      | Stack_trace of (exn * Printexc.raw_backtrace)
      | Type of exn

    let to_name (field : t) =
      let suffix =
        match field with
        | Code _ -> "code"
        | Id _ -> "id"
        | Message _ -> "message"
        | Stack_trace _ -> "stack_trace"
        | Type _ -> "type"
      in
      "error." ^ suffix

    let pp ppf (field : t) =
      match field with
      | Code v
      | Id v
      | Message v ->
        Fmt.string ppf v
      | Stack_trace st -> Fmt.exn_backtrace ppf st
      | Type e -> Fmt.exn ppf e

    let to_json (field : t) : Json.t = `String (Fmt.str "%a" pp field)
  end
  module _ : Ecs_field_def = Error

  module type Custom_field = sig
    module Def : Ecs_field_def
    val value : Def.t
  end

  type t =
    | Base of Base.t
    | Error of Error.t
    | Custom of (module Custom_field)

  let to_name (field : t) =
    match field with
    | Base b -> Base.to_name b
    | Error e -> Error.to_name e
    | Custom (module M) -> M.Def.to_name M.value

  let pp ppf (field : t) =
    match field with
    | Base b -> Base.pp ppf b
    | Error e -> Error.pp ppf e
    | Custom (module M) -> M.Def.pp ppf M.value

  let to_json (field : t) =
    match field with
    | Base b -> Base.to_json b
    | Error e -> Error.to_json e
    | Custom (module M) -> M.Def.to_json M.value

  module Fields = struct
    include Set.Make (struct
      type nonrec t = t

      let compare a b = String.compare (to_name a) (to_name b)
    end)

    let replace field set = remove field set |> add field

    let add_if_new field set = add field set

    let find field set = find_opt field set

    let pp ppf fields =
      Fmt.iter iter
        (fun ppf field -> Fmt.pf ppf "%s: %a" (to_name field) pp field)
        ppf fields
  end

  let tag =
    Logs.Tag.def ~doc:"Elastic Common Schema (ECS) fields" "ecs" Fields.pp

  let of_list fields = Fields.of_list fields

  let tags_of_list fields = Logs.Tag.add tag (of_list fields) Logs.Tag.empty

  let ecs_fields_of_tags (tags : Logs.Tag.set) : Json.t String_map.t =
    match Logs.Tag.find tag tags with
    | None -> String_map.empty
    | Some ecs ->
      Fields.to_seq ecs
      |> Seq.map (fun field -> (to_name field, to_json field))
      |> String_map.of_seq
end
module _ : Ecs_field_def = Ecs

let timestamp_of_tags_or_now (tags : Logs.Tag.set) =
  match Logs.Tag.find Ecs.tag tags with
  | None -> Epoch.to_timestamp (Unix.gettimeofday ())
  | Some ecs ->
    ( match Ecs.Fields.find (Base (Timestamp 0.0)) ecs with
    | None -> Epoch.to_timestamp (Unix.gettimeofday ())
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
    let fields = Ecs.ecs_fields_of_tags tags in
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
      (fun () -> `String (Epoch.to_timestamp (Unix.gettimeofday ())))
      fields
    |> add_if_new "log.level" (fun () ->
           `String (Logs.level_to_string (Some level)))
    |> add_if_new "log.logger" (fun () -> `String (Logs.Src.name src))
    |> replace "message" (`String message)

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
