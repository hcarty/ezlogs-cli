val setup_log : unit Cmdliner.Term.t
(** Use with Cmdliner to get automatic logging setup. Log messages will be
    prefixed with a date and time stamp along with their log level.

    This also pulls in the standard Logs and Fmt CLI configuration options such
    as enabling/disabling color support and setting the log level. *)
