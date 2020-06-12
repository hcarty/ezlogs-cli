(** {1 Elastic Common Schema (ECS) field definitions}

    This library provides typed mappings from OCaml values to JSON fields
    according to ECS. See
    {{:https://www.elastic.co/guide/en/ecs/current/ecs-reference.html} the ECS
    reference site} for more information on ECS. *)

module Json = Yojson.Basic

val ecs_version : string
(** The ECS schema version this library conforms to *)

module Epoch : sig
  (** {1 POSIX epoch times as ECS timestamps} *)

  val to_timestamp : Ptime.t -> string
  (** [to_timestamp t] is [t] in the form [YYYY-MM-DDTHH:MM:SS.sssZ] *)

  val pp_timestamp : Ptime.t Fmt.t
  (** A timestamp pretty printer. Outputs in the same format as {!to_timestamp}. *)
end

module type Field_def = sig
  (** {1 Field definitions}

      All value to ECS mappings must conform to this module type. *)

  type t
  (** An OCaml value to map to an ECS field *)

  val to_name : t -> string
  (** [to_name x] is the name of the JSON field under which [x] will be stored. *)

  val pp : t Fmt.t
  (** Pretty-printer for [t] values *)

  val to_json : t -> Json.t
  (** [to_json x] is the JSON value associated with [to_name x] in an ECS JSON
      document. *)
end

module Base : sig
  (** {1 Base fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-base.html} *)

  type t =
    | Timestamp of Ptime.t
    | Tags of string list
    | Labels of (string * string) list
end
module Error : sig
  (** {1 Error fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-error.html} *)

  type t =
    | Code of string
    | Id of string
    | Message of string
    | Stack_trace of (exn * Printexc.raw_backtrace)
    | Type of exn
end
module Event : sig
  (** {1 Event fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-event.html} *)

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

  type kind =
    | Alert
    | Event
    | Metric
    | State
    | Pipeline_error
    | Signal

  type outcome =
    | Failure
    | Success
    | Unknown

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
end
module Hash : sig
  (** {1 Hash fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-hash.html} *)

  type t =
    | Md5 of string
    | Sha1 of string
    | Sha256 of string
    | Sha512 of string
end
module Http : sig
  (** {1 HTTP fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-http.html} *)

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

  val request : request list -> t list
  val response : response list -> t list
end
module File : sig
  (** {1 File fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-file.html} *)

  type t =
    | Hash of Hash.t
    | Size of int
end
module Log : sig
  (** {1 Log fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-log.html} *)

  type t =
    | Origin_file of string
    | Origin_line of int
    | Origin_function of string
end
module Service : sig
  (** {1 Service fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-service.html} *)

  type t =
    | Ephemeral_id of string
    | Id of string
    | Name of string
    | Node_name of string
    | State of string
    | Type of string
    | Version of string
end
module Trace : sig
  (** {1 Trace fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-trace.html} *)

  type t =
    | Trace_id of string
    | Transaction_id of string
end
module Url : sig
  (** {1 URL fields}

      Reference: {:https://www.elastic.co/guide/en/ecs/current/ecs-url.html} *)

  type t =
    | Domain of string
    | Fragment of string
    | Full of Uri.t
    | Password of string
    | Path of string
    | Query of string
    | Scheme of string
    | Username of string

  val of_uri : ?keep_password:bool -> Uri.t -> t list
end

module type Custom_field = sig
  (** {1 Custom fields}

      This module type gives a path to defining fields which are not already
      defined in {!Ecs}. *)

  module Def : Field_def
  val value : Def.t
end

(** {2 ECS documents} *)

(** All supported ECS fields *)
type t =
  | Base of Base.t
  | Error of Error.t
  | Event of Event.t
  | File of File.t
  | Log of Log.t
  | Service of Service.t
  | Trace of Trace.t
  | Url of Url.t
  | Custom of (module Custom_field)

val pp : t Fmt.t
(** Pretty-printer with an unspecified output format *)

(** {3 Convenience constructor functions}

    These functions may be useful when defining multiple fields at once from the
    same category. *)

val base : Base.t list -> t list
val error : Error.t list -> t list
val event : Event.t list -> t list
val file : File.t list -> t list
val log : Log.t list -> t list
val service : Service.t list -> t list
val trace : Trace.t list -> t list
val url : Url.t list -> t list
val custom : (module Custom_field) list -> t list

module Fields : sig
  (** {1 Sets of ECS fields} *)

  type elt = t
  (** An ECS field *)

  type t
  (** A set of ECS fields *)

  val empty : t
  (** The empty set *)

  val replace : elt -> t -> t
  (** [replace field set] adds [field] to [set], replacing [field] if it already
      exists in [set]. *)

  val add_if_new : elt -> t -> t
  (** [add_if_new field set] adds [field] to [set] if [field] is not already
      defined in [set]. If [field] is already defined in [set] then [set] is
      returned as-is. *)

  val find : elt -> t -> elt option
  (** [find field set] returns a field matching [field] in [set] if it exists,
      otherwise it returns [None]. *)
end

val of_uri : ?keep_password:bool -> Uri.t -> t list
(** [of_uri ?keep_password uri] splits [uri] into a list of fields based on the
    components in [uri]. *)

val tags_of_list : t list -> Logs.Tag.set
(** [tags_of_list fields] is a set of {!Logs} tags corresponding to [fields]. *)

val add_tag : t -> Logs.Tag.set -> Logs.Tag.set
(** [add_tag field tags] is [tags] with [field] added to the ECS fields defined
    in [tags]. *)

val add_tags : t list list -> Logs.Tag.set -> Logs.Tag.set
(** [add_tags fields tags] is [tags] with each field in [fields] added to the
    ECS fields defined in [tags]. *)

val tag : Fields.t Logs.Tag.def
(** {!Logs} tag definition for ECS fields *)

val fields_of_tags : Logs.Tag.set -> Json.t Map.Make(String).t
(** [fields_of_tags tags] is a [name -> JSON] mapping for all of the ECS fields
    defined in [tags]. If no ECS fields are defined in [tags] the map will be
    empty. *)
