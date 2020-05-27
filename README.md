# Easy logging setup for OCaml + Logs + Cmdliner

Let's load up the library and set a logging level.
```ocaml
# #require "ezlogs-cli";;
# #require "ezlogs-cli-lwt";;
# Logs.set_level (Some Info);;
- : unit = ()
```

Setup a logger with simple, unstructured logging and give it a try!
```ocaml
# Logs.set_reporter (Ezlogs_cli.Line.reporter Fmt.stdout);;
- : unit = ()
# Logs.info (fun m -> m "Everything is fine");
2020-05-27 23:22:13 [INFO] Everything is fine
- : unit = ()
```

[ECS] compliant JSON logging is also an option if we pick the correct reporter.
```ocaml
# Logs.set_reporter (Ezlogs_cli.Json.reporter Fmt.stdout);;
- : unit = ()
# Logs.info (fun m -> m "Everything is fine");;
{"@timestamp":"2020-05-27T23:22:13.632Z","log.level":"info","log.logger":"application","message":"Everything is fine"}
- : unit = ()
```

[ECS]: https://www.elastic.co/guide/en/ecs/current/ecs-reference.html
