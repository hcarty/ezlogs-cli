# Easy logging setup for OCaml + Logs + Cmdliner

Let's load up the library and set a logging level.
```ocaml
# #require "ezlogs-cli";;
# #require "ezlogs-cli-lwt";;
# Logs.set_level (Some Info);;
- : unit = ()
```

And we'll define a fixed timestamp to use so the output here in the README is
stable over time.
```ocaml
# let ecs = Ezlogs_cli.Ecs.Fields.empty |> Ezlogs_cli.Ecs.Fields.replace (Base (Timestamp 2590779494.386));;
# let tags = Logs.Tag.empty |> Logs.Tag.add Ezlogs_cli.Ecs.tag ecs;;
val tags : Logs.Tag.set = <abstr>
```

Setup a logger with simple, unstructured logging and give it a try!
```ocaml
# Logs.set_reporter (Ezlogs_cli.Line_output.reporter Fmt.stdout);;
- : unit = ()
# Logs.info (fun m -> m "Hello from the future" ~tags);
2052-02-05T20:58:14.386Z [INFO] Hello from the future
- : unit = ()
```

[ECS] compliant JSON logging is also an option if we pick the correct reporter.
```ocaml
# Logs.set_reporter (Ezlogs_cli.Json_output.reporter Fmt.stdout);;
- : unit = ()
# Logs.info (fun m -> m "Hello from the future" ~tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","log.level":"info","log.logger":"application","message":"Hello from the future"}
- : unit = ()
```

[ECS]: https://www.elastic.co/guide/en/ecs/current/ecs-reference.html
