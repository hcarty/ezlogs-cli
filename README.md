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
# let timestamp = Ptime.of_float_s 2590779494.386 |> Option.get;;
val timestamp : Ptime.t = <abstr>
# let tags = Ezlogs_cli.Ecs.tags_of_list [Base (Timestamp timestamp)];;
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

We can add fields to include more details.
```ocaml
# let error_tags = Ezlogs_cli.Ecs.add_tag (Error (Code "failure-101")) tags;;
val error_tags : Logs.Tag.set = <abstr>
# Logs.err (fun m -> m "There was a failure" ~tags:error_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","error.code":"failure-101","log.level":"error","log.logger":"application","message":"There was a failure"}
- : unit = ()
```

Maybe we're working with URLs - we can log that too!
```ocaml
# let uri = Uri.of_string "https://me:secret@example.com:9090/path?to=success#downhere";;
val uri : Uri.t = <abstr>
# let tags = Ezlogs_cli.Ecs.add_tags (Ezlogs_cli.Ecs.of_uri uri) tags;;
val tags : Logs.Tag.set = <abstr>
# Logs.info (fun m -> m "Finished request" ~tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","log.level":"info","log.logger":"application","message":"Finished request","url.domain":"example.com","url.fragment":"downhere","url.full":"https://me@example.com:9090/path?to=success#downhere","url.path":"/path","url.query":"to=success","url.scheme":"https","url.username":"me"}
- : unit = ()
```
Look, no passwords!

[ECS]: https://www.elastic.co/guide/en/ecs/current/ecs-reference.html
