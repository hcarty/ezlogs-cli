module Json = Yojson.Basic

module type Ecs_field_def = sig
  type t
  val to_name : t -> string
  val pp : t Fmt.t
  val to_json : t -> Json.t
end

module Ecs : sig
  module Base : sig
    type t =
      | Timestamp of float
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

  module type Custom_field = sig
    module Def : Ecs_field_def
    val value : Def.t
  end

  type t =
    | Base of Base.t
    | Error of Error.t
    | Custom of (module Custom_field)

  module Fields : sig
    type elt = t
    type t

    val empty : t
    val replace : elt -> t -> t
    val add_if_new : elt -> t -> t
    val find : elt -> t -> elt option
  end

  val tag : Fields.t Logs.Tag.def
end

module Line_output : sig
  val reporter : Format.formatter -> Logs.reporter

  val logging : unit Cmdliner.Term.t
  (** Use with Cmdliner to get automatic logging setup. Log messages will be
      prefixed with a date and time stamp along with their log level.

      This also pulls in the standard Logs and Fmt CLI configuration options
      such as enabling/disabling color support and setting the log level. *)
end

module Json_output : sig
  val reporter : Format.formatter -> Logs.reporter

  val logging : unit Cmdliner.Term.t
  (** Use with Cmdliner to get automatic logging setup. Log messages will be
      single-line JSON objects with fields specifying the log message, level,
      source and any tags.

      This also pulls in the standard Logs configuration options for setting the
      log level.

      All logs will be written to [stderr]. *)
end

type format =
  | Line
  | Json

val logging : default:format -> unit Cmdliner.Term.t
(** [logging format] can be used with Cmdliner to get automatic logging setup.
    Log message structure ({!Line} or {!Json}) can be selected from the command
    line. If no option is specified on the command line then [default] will be
    used.

    This acts as {!Line.logging} or {!Json.logging}, depending on which format
    is selected. *)
