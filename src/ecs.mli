module Json = Yojson.Basic

val ecs_version : string

module Epoch : sig
  val to_timestamp : Ptime.t -> string
  val pp_timestamp : Ptime.t Fmt.t
end

module type Field_def = sig
  type t
  val to_name : t -> string
  val pp : t Fmt.t
  val to_json : t -> Json.t
end

module Base : sig
  type t =
    | Timestamp of Ptime.t
    | Tags of string list
    | Labels of (string * string) list
end
module Error : sig
  type t =
    | Code of string
    | Id of string
    | Message of string
    | Stack_trace of (exn * Printexc.raw_backtrace)
    | Type of exn
end
module Trace : sig
  type t =
    | Trace_id of string
    | Transaction_id of string
end
module Url : sig
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
  module Def : Field_def
  val value : Def.t
end

type t =
  | Base of Base.t
  | Error of Error.t
  | Trace of Trace.t
  | Url of Url.t
  | Custom of (module Custom_field)

val pp : t Fmt.t

module Fields : sig
  type elt = t
  type t

  val empty : t
  val replace : elt -> t -> t
  val add_if_new : elt -> t -> t
  val find : elt -> t -> elt option
end

val of_uri : ?keep_password:bool -> Uri.t -> t list

val of_list : t list -> Fields.t

val tags_of_list : t list -> Logs.Tag.set

val add_tag : t -> Logs.Tag.set -> Logs.Tag.set

val add_tags : t list -> Logs.Tag.set -> Logs.Tag.set

val tag : Fields.t Logs.Tag.def

val fields_of_tags : Logs.Tag.set -> Json.t Map.Make(String).t
