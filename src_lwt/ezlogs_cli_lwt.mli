module Line : sig
  val logging : unit Cmdliner.Term.t
  (** Use with Cmdliner to get automatic logging setup. Log messages will be
      prefixed with a date and time stamp along with their log level.

      This also pulls in the standard Logs and Fmt CLI configuration options
      such as enabling/disabling color support and setting the log level. *)
end

module Json : sig
  val reporter : Lwt_fmt.formatter -> Logs.reporter

  val logging : unit Cmdliner.Term.t
  (** Use with Cmdliner to get automatic logging setup. Log messages will be
      single-line JSON objects with fields specifying the log message, level,
      source and any tags.

      This also pulls in the standard Logs configuration options for setting the
      log level.

      All logs will be written to [stdout]. *)
end
