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
