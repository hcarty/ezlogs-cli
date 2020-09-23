(** {1 Elastic Application Performance Monitoring (APM) definitions}

    This library provides typed mappings from OCaml values to JSON fields
    according to the document structure expected by Elastic APM. See
    {{:https://www.elastic.co/guide/en/apm/server/current/index.html} the APM
    Server reference site} for more information on APM. *)

module Json = Yojson.Basic

module Agent : sig
  (** {1 APM agent} *)

  val agent_name : string
  (** APM agent name, reported to the APM server *)

  val agent_version : string
  (** APM agent version, reported to the APM server *)
end

module Cloud : sig
  (** {1 Cloud provider fields} *)

  type t =
    | Availability_zone of string
    | Provider of string
    | Region of string
end

module Destination : sig
  (** {1 Destination fields} *)

  type t = Address of string

  include Ecs.Field_def with type t := t
end

module Error : sig
  (** {1 Error fields} *)

  type t =
    | Group_id of string
    | Culprit of string
    | Log_level of string
    | Log_message of string
end

module Observer : sig
  (** {1 Observer fields} *)

  type t =
    | Hostname of string
    | Version_major of string
    | Listening of string
end

module Parent : sig
  (** {1 Parent fields} *)

  type t = Id of string
end

module Processor : sig
  (** {1 Processor fields} *)

  type t = Event of string
end

module Service : sig
  (** {1 Service fields} *)

  type framework = {
    name : string;
    version : string;
  }

  val language_name : string
  (** Always "OCaml" *)

  val language_version : string
  (** Always {!Sys.ocaml_version} *)

  type t =
    | Name of string
    | Environment of string
    | Framework of framework
    | Node_name of string
    | Version of string
end

module Span : sig
  (** {1 Span fields} *)

  type t =
    | Duration_us of int
    | Type of string
    | Subtype of string
    | Self_time_us of int
    | Action of string
    | Name of string
    | Id of string
    | Destination_service_resource of string
end

module Transaction : sig
  (** {1 Transaction fields} *)

  type t =
    | Duration_us of int
    | Type of string
    | Result of string
    | Name of string
    | Id of string
    | Sampled of bool
    | Root of bool
end

module User : sig
  (** {1 User fields} *)

  type t = Id of string
end

module User_agent : sig
  (** {1 User agent fields} *)

  type t =
    | Name of string
    | Original of string
end

module Event = Ecs.Event
module Http = Ecs.Http
module Url = Ecs.Url

(** {2 APM documents} *)

(** All supported APM fields *)
type t =
  | Cloud of Cloud.t
  | Destination of Destination.t
  | Error of Error.t
  | Event of Event.t
  | Http of Http.t
  | Observer of Observer.t
  | Parent of Parent.t
  | Processor of Processor.t
  | Service of Service.t
  | Span of Span.t
  | Transaction of Transaction.t
  | Url of Url.t
  | User of User.t
  | User_agent of User_agent.t

val pp : t Fmt.t
(** Pretty-printer with an unspecified output format *)

(** {3 Convenience constructor functions}

    These functions may be useful when defining multiple fields at once from the
    same category. *)

val cloud : Cloud.t list -> t list
val error : Error.t list -> t list
val event : Event.t list -> t list
val http : Http.t list -> t list
val observer : Observer.t list -> t list
val parent : Parent.t list -> t list
val processor : Processor.t list -> t list
val service : Service.t list -> t list
val span : Span.t list -> t list
val url : Url.t list -> t list
val user : User.t list -> t list
val user_agent : User_agent.t list -> t list

module Fields : sig
  (** {1 Sets of APM fields} *)

  type elt = t
  (** An APM field *)

  type t
  (** A set of APM fields *)

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
(** [fields_of_tags tags] is a [name -> JSON] mapping for all of the APM fields
    defined in [tags]. If no APM fields are defined in [tags] the map will be
    empty. *)
