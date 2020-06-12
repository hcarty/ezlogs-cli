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

module Event = struct
  type category =
    | Authentication
    | Database
    | Driver
    | File
    | Host
    | Iam
    | Intrusion_detection
    | Malware
    | Network
    | Package
    | Process
    | Web
  [@@deriving show { with_path = false }]
  let pp_category ppf (c : category) =
    let s = Fmt.str "%a" pp_category c in
    Fmt.string ppf (String.lowercase_ascii s)
  let category_to_string (c : category) =
    String.lowercase_ascii (show_category c)

  type kind =
    | Alert
    | Event
    | Metric
    | State
    | Pipeline_error
    | Signal
  [@@deriving show { with_path = false }]
  let pp_kind ppf (k : kind) =
    let s = Fmt.str "%a" pp_kind k in
    Fmt.string ppf (String.lowercase_ascii s)
  let kind_to_string (k : kind) = String.lowercase_ascii (show_kind k)

  type outcome =
    | Failure
    | Success
    | Unknown
  [@@deriving show { with_path = false }]
  let pp_outcome ppf (o : outcome) =
    let s = Fmt.str "%a" pp_outcome o in
    Fmt.string ppf (String.lowercase_ascii s)
  let outcome_to_string (o : outcome) = String.lowercase_ascii (show_outcome o)

  type type_ =
    | Access
    | Admin
    | Allowed
    | Change
    | Connection
    | Creation
    | Deletion
    | Denied
    | End
    | Error
    | Group
    | Info
    | Installation
    | Protocol
    | Start
    | User
  [@@deriving show { with_path = false }]
  let pp_type_ ppf (t : type_) =
    let s = Fmt.str "%a" pp_type_ t in
    Fmt.string ppf (String.lowercase_ascii s)
  let type__to_string (t : type_) = String.lowercase_ascii (show_type_ t)

  type t =
    | Action of string
    | Category of category list
    | Code of string
    | Created of Ptime.t
    | Dataset of string
    | Duration of int
    | End of Ptime.t
    | Hash of string
    | Id of string
    | Ingested of Ptime.t
    | Kind of kind
    | Module of string
    | Original of string
    | Outcome of outcome
    | Provider of string
    | Reference of Uri.t
    | Risk_score of float
    | Risk_score_norm of float
    | Sequence of int
    | Severity of int
    | Start of Ptime.t
    | Timezone of string
    | Type of type_ list
    | Url of Uri.t

  let to_name (field : t) =
    let suffix =
      match field with
      | Action _ -> "action"
      | Category _ -> "category"
      | Code _ -> "code"
      | Created _ -> "created"
      | Dataset _ -> "dataset"
      | Duration _ -> "duration"
      | End _ -> "end"
      | Hash _ -> "hash"
      | Id _ -> "id"
      | Ingested _ -> "ingested"
      | Kind _ -> "kind"
      | Module _ -> "module"
      | Original _ -> "original"
      | Outcome _ -> "outcome"
      | Provider _ -> "provider"
      | Reference _ -> "reference"
      | Risk_score _ -> "risk_score"
      | Risk_score_norm _ -> "risk_score_norm"
      | Sequence _ -> "sequence"
      | Severity _ -> "severity"
      | Start _ -> "start"
      | Timezone _ -> "timezone"
      | Type _ -> "type"
      | Url _ -> "url"
    in
    "event." ^ suffix

  let pp ppf (field : t) =
    match field with
    | Action s
    | Code s
    | Dataset s
    | Hash s
    | Id s
    | Module s
    | Original s
    | Provider s
    | Timezone s ->
      Fmt.string ppf s
    | Category cs -> Fmt.(Dump.list pp_category) ppf cs
    | Created epoch
    | End epoch
    | Ingested epoch
    | Start epoch ->
      Epoch.pp_timestamp ppf epoch
    | Duration i
    | Sequence i
    | Severity i ->
      Fmt.int ppf i
    | Kind k -> pp_kind ppf k
    | Outcome o -> pp_outcome ppf o
    | Reference u
    | Url u ->
      Uri.pp ppf u
    | Risk_score f
    | Risk_score_norm f ->
      Fmt.float_dfrac 3 ppf f
    | Type ts -> Fmt.(Dump.list pp_type_) ppf ts

  let to_json (field : t) : Json.t =
    match field with
    | Action s
    | Code s
    | Dataset s
    | Hash s
    | Id s
    | Module s
    | Original s
    | Provider s
    | Timezone s ->
      `String s
    | Category cs ->
      `List (List.map (fun c -> `String (category_to_string c)) cs)
    | Created epoch
    | End epoch
    | Ingested epoch
    | Start epoch ->
      `String (Epoch.to_timestamp epoch)
    | Duration i
    | Sequence i
    | Severity i ->
      `Int i
    | Kind k -> `String (kind_to_string k)
    | Outcome o -> `String (outcome_to_string o)
    | Reference u
    | Url u ->
      `String (Uri.to_string u)
    | Risk_score f
    | Risk_score_norm f ->
      `Float f
    | Type ts -> `List (List.map (fun t -> `String (type__to_string t)) ts)
end
module _ : Field_def = Event

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

module Http = struct
  type request =
    | Body_bytes of int
    | Body_content of string
    | Bytes of int
    | Method of string
    | Referrer of Uri.t

  type response =
    | Body_bytes of int
    | Body_content of string
    | Bytes of int
    | Status_code of int

  type t =
    | Request of request
    | Response of response
    | Version of string

  let request_to_name (field : request) =
    match field with
    | Body_bytes _ -> "body.bytes"
    | Body_content _ -> "body.content"
    | Bytes _ -> "bytes"
    | Method _ -> "method"
    | Referrer _ -> "referrer"

  let pp_request ppf (field : request) =
    match field with
    | Body_bytes i
    | Bytes i ->
      Fmt.int ppf i
    | Body_content s
    | Method s ->
      Fmt.string ppf s
    | Referrer u -> Uri.pp ppf u

  let request_to_json (field : request) : Json.t =
    match field with
    | Body_bytes i
    | Bytes i ->
      `Int i
    | Body_content s
    | Method s ->
      `String s
    | Referrer u -> `String (Uri.to_string u)

  let response_to_name (field : response) =
    match field with
    | Body_bytes _ -> "body.bytes"
    | Body_content _ -> "body.content"
    | Bytes _ -> "bytes"
    | Status_code _ -> "status_code"

  let pp_response ppf (field : response) =
    match field with
    | Body_bytes i
    | Bytes i
    | Status_code i ->
      Fmt.int ppf i
    | Body_content s -> Fmt.string ppf s

  let response_to_json (field : response) : Json.t =
    match field with
    | Body_bytes i
    | Bytes i
    | Status_code i ->
      `Int i
    | Body_content s -> `String s

  let to_name (field : t) =
    match field with
    | Request r -> "http.request" ^ request_to_name r
    | Response r -> "http.response" ^ response_to_name r
    | Version _ -> "http.version"

  let pp ppf (field : t) =
    match field with
    | Request r -> pp_request ppf r
    | Response r -> pp_response ppf r
    | Version v -> Fmt.string ppf v

  let to_json (field : t) : Json.t =
    match field with
    | Request r -> request_to_json r
    | Response r -> response_to_json r
    | Version v -> `String v

  let request (fields : request list) = List.map (fun f -> Request f) fields
  let response (fields : response list) = List.map (fun f -> Response f) fields
end
module _ : Field_def = Http

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

module Service = struct
  type t =
    | Ephemeral_id of string
    | Id of string
    | Name of string
    | Node_name of string
    | State of string
    | Type of string
    | Version of string

  let to_name (field : t) =
    let suffix =
      match field with
      | Ephemeral_id _ -> "ephemeral_id"
      | Id _ -> "id"
      | Name _ -> "name"
      | Node_name _ -> "node.name"
      | State _ -> "state"
      | Type _ -> "type"
      | Version _ -> "version"
    in
    "service." ^ suffix

  let pp ppf (field : t) =
    match field with
    | Ephemeral_id s
    | Id s
    | Name s
    | Node_name s
    | State s
    | Type s
    | Version s ->
      Fmt.string ppf s

  let to_json (field : t) : Json.t = `String (Fmt.str "%a" pp field)
end
module _ : Field_def = Service

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
  | Event of Event.t
  | File of File.t
  | Http of Http.t
  | Log of Log.t
  | Service of Service.t
  | Trace of Trace.t
  | Url of Url.t
  | Custom of (module Custom_field)

let base fields = List.map (fun f -> Base f) fields
let error fields = List.map (fun f -> Error f) fields
let event fields = List.map (fun f -> Event f) fields
let file fields = List.map (fun f -> File f) fields
let http fields = List.map (fun f -> Http f) fields
let log fields = List.map (fun f -> Log f) fields
let service fields = List.map (fun f -> Service f) fields
let trace fields = List.map (fun f -> Trace f) fields
let url fields = List.map (fun f -> Url f) fields
let custom fields = List.map (fun f -> Custom f) fields

let to_name (field : t) =
  match field with
  | Base b -> Base.to_name b
  | Event e -> Event.to_name e
  | Error e -> Error.to_name e
  | File f -> File.to_name f
  | Http h -> Http.to_name h
  | Log l -> Log.to_name l
  | Service s -> Service.to_name s
  | Trace t -> Trace.to_name t
  | Url u -> Url.to_name u
  | Custom (module M) -> M.Def.to_name M.value

let pp ppf (field : t) =
  match field with
  | Base b -> Base.pp ppf b
  | Event e -> Event.pp ppf e
  | Error e -> Error.pp ppf e
  | File f -> File.pp ppf f
  | Http h -> Http.pp ppf h
  | Log l -> Log.pp ppf l
  | Service s -> Service.pp ppf s
  | Trace t -> Trace.pp ppf t
  | Url u -> Url.pp ppf u
  | Custom (module M) -> M.Def.pp ppf M.value

let to_json (field : t) =
  match field with
  | Base b -> Base.to_json b
  | Event e -> Event.to_json e
  | Error e -> Error.to_json e
  | File f -> File.to_json f
  | Http h -> Http.to_json h
  | Log l -> Log.to_json l
  | Service s -> Service.to_json s
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
  let fields =
    List.fold_left
      (fun accu fields -> Fields.add_seq (List.to_seq fields) accu)
      existing_fields fields
  in
  Logs.Tag.rem tag tags |> Logs.Tag.add tag fields

let fields_of_tags (tags : Logs.Tag.set) : Json.t String_map.t =
  match Logs.Tag.find tag tags with
  | None -> String_map.empty
  | Some ecs ->
    Fields.to_seq ecs
    |> Seq.map (fun field -> (to_name field, to_json field))
    |> String_map.of_seq
