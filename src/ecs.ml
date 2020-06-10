module Json = Yojson.Basic
module String_map = Map.Make (String)

(* What ECS version is the current code based on? *)
let ecs_version = "1.5.0"

module Epoch = struct
  let to_timestamp ptime = Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 ptime

  let pp_timestamp = Fmt.of_to_string to_timestamp
end

module type Field_def = sig
  type t
  val to_name : t -> string
  val pp : t Fmt.t
  val to_json : t -> Json.t
end

module Base = struct
  type t =
    | Timestamp of Ptime.t
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
module _ : Field_def = Base

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
module _ : Field_def = Error

module Hash = struct
  type t =
    | Md5 of string
    | Sha1 of string
    | Sha256 of string
    | Sha512 of string

  let to_name (field : t) =
    let suffix =
      match field with
      | Md5 _ -> "md5"
      | Sha1 _ -> "sha1"
      | Sha256 _ -> "sha256"
      | Sha512 _ -> "sha512"
    in
    "hash." ^ suffix

  let pp ppf (field : t) =
    match field with
    | Md5 hash
    | Sha1 hash
    | Sha256 hash
    | Sha512 hash ->
      Fmt.string ppf hash

  let to_json (field : t) : Json.t =
    match field with
    | Md5 hash
    | Sha1 hash
    | Sha256 hash
    | Sha512 hash ->
      `String hash
end
module _ : Field_def = Hash

module File = struct
  type t =
    | Hash of Hash.t
    | Size of int

  let to_name (field : t) =
    let suffix =
      match field with
      | Hash hash -> Hash.to_name hash
      | Size _ -> "size"
    in
    "file." ^ suffix

  let pp ppf (field : t) =
    match field with
    | Hash hash -> Hash.pp ppf hash
    | Size size -> Fmt.int ppf size

  let to_json (field : t) : Json.t =
    match field with
    | Hash hash -> Hash.to_json hash
    | Size size -> `Int size
end
module _ : Field_def = File

module Log = struct
  type t =
    | Origin_file of string
    | Origin_line of int
    | Origin_function of string

  let to_name (field : t) =
    match field with
    | Origin_file _ -> "log.origin.file"
    | Origin_line _ -> "log.origin.line"
    | Origin_function _ -> "log.origin.function"

  let pp ppf (field : t) =
    match field with
    | Origin_file file -> Fmt.string ppf file
    | Origin_line line -> Fmt.int ppf line
    | Origin_function f -> Fmt.string ppf f

  let to_json (field : t) : Json.t =
    match field with
    | Origin_file file -> `String file
    | Origin_line line -> `Int line
    | Origin_function f -> `String f
end
module _ : Field_def = Log

module Trace = struct
  type t =
    | Trace_id of string
    | Transaction_id of string

  let to_name (field : t) =
    match field with
    | Trace_id _ -> "trace.id"
    | Transaction_id _ -> "transaction.id"

  let pp ppf (field : t) =
    match field with
    | Trace_id id
    | Transaction_id id ->
      Fmt.string ppf id

  let to_json (field : t) : Json.t = `String (Fmt.str "%a" pp field)
end
module _ : Field_def = Trace

module Url = struct
  type t =
    | Domain of string
    | Fragment of string
    | Full of Uri.t
    | Password of string
    | Path of string
    | Query of string
    | Scheme of string
    | Username of string

  let to_name (field : t) =
    let suffix =
      match field with
      | Domain _ -> "domain"
      | Fragment _ -> "fragment"
      | Full _ -> "full"
      | Password _ -> "password"
      | Path _ -> "path"
      | Query _ -> "query"
      | Scheme _ -> "scheme"
      | Username _ -> "username"
    in
    "url." ^ suffix

  let pp ppf (field : t) =
    match field with
    | Domain s
    | Fragment s
    | Password s
    | Path s
    | Query s
    | Scheme s
    | Username s ->
      Fmt.string ppf s
    | Full uri -> Uri.pp ppf uri

  let to_json (field : t) : Json.t = `String (Fmt.str "%a" pp field)

  let of_uri ?(keep_password = false) (uri : Uri.t) : t list =
    let uri =
      if keep_password then
        uri
      else
        (* Remove the password if there is one *)
        Uri.with_password uri None
    in
    [
      Option.map (fun x -> Domain x) (Uri.host uri);
      Option.map (fun x -> Fragment x) (Uri.fragment uri);
      Some (Full uri);
      Option.map (fun x -> Password x) (Uri.password uri);
      Some (Path (Uri.path uri));
      Option.map (fun x -> Query x) (Uri.verbatim_query uri);
      Option.map (fun x -> Scheme x) (Uri.scheme uri);
      Option.map (fun x -> Username x) (Uri.user uri);
    ]
    |> List.filter_map Fun.id
end
module _ : Field_def = Url

module type Custom_field = sig
  module Def : Field_def
  val value : Def.t
end

type t =
  | Base of Base.t
  | Error of Error.t
  | File of File.t
  | Log of Log.t
  | Trace of Trace.t
  | Url of Url.t
  | Custom of (module Custom_field)

let to_name (field : t) =
  match field with
  | Base b -> Base.to_name b
  | Error e -> Error.to_name e
  | File f -> File.to_name f
  | Log l -> Log.to_name l
  | Trace t -> Trace.to_name t
  | Url u -> Url.to_name u
  | Custom (module M) -> M.Def.to_name M.value

let pp ppf (field : t) =
  match field with
  | Base b -> Base.pp ppf b
  | Error e -> Error.pp ppf e
  | File f -> File.pp ppf f
  | Log l -> Log.pp ppf l
  | Trace t -> Trace.pp ppf t
  | Url u -> Url.pp ppf u
  | Custom (module M) -> M.Def.pp ppf M.value

let to_json (field : t) =
  match field with
  | Base b -> Base.to_json b
  | Error e -> Error.to_json e
  | File f -> File.to_json f
  | Log l -> Log.to_json l
  | Trace t -> Trace.to_json t
  | Url u -> Url.to_json u
  | Custom (module M) -> M.Def.to_json M.value

let of_uri ?keep_password uri : t list =
  let fields = Url.of_uri ?keep_password uri in
  List.map (fun field -> Url field) fields

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

let tag = Logs.Tag.def ~doc:"Elastic Common Schema (ECS) fields" "ecs" Fields.pp

let of_list fields = Fields.of_list fields

let tags_of_list fields = Logs.Tag.add tag (of_list fields) Logs.Tag.empty

let add_tag field tags =
  let existing_fields =
    match Logs.Tag.find tag tags with
    | None -> Fields.empty
    | Some fields -> fields
  in
  let fields = Fields.add_if_new field existing_fields in
  Logs.Tag.rem tag tags |> Logs.Tag.add tag fields

let add_tags fields tags =
  let existing_fields =
    match Logs.Tag.find tag tags with
    | None -> Fields.empty
    | Some fields -> fields
  in
  let fields = Fields.add_seq (List.to_seq fields) existing_fields in
  Logs.Tag.rem tag tags |> Logs.Tag.add tag fields

let fields_of_tags (tags : Logs.Tag.set) : Json.t String_map.t =
  match Logs.Tag.find tag tags with
  | None -> String_map.empty
  | Some ecs ->
    Fields.to_seq ecs
    |> Seq.map (fun field -> (to_name field, to_json field))
    |> String_map.of_seq
