# Easy logging setup for OCaml + Logs + Cmdliner

Let's load up the library and set a logging level.
```ocaml
# #require "ezlogs-cli";;
# #require "ezlogs-cli-lwt";;
# module Ecs = Ezlogs_cli.Ecs;;
module Ecs = Ezlogs_cli.Ecs
# Logs.set_level (Some Info);;
- : unit = ()
```

And we'll define a fixed timestamp to use so the output here in the README is
stable over time.
```ocaml
# let timestamp = Ptime.of_float_s 2590779494.386 |> Option.get;;
val timestamp : Ptime.t = <abstr>
# let tags = Ecs.tags_of_list [Base (Timestamp timestamp)];;
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
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","log.level":"info","log.logger":"application","message":"Hello from the future"}
- : unit = ()
```

We can add fields to include more details.
```ocaml
# let error_tags = Ecs.add_tag (Error (Code "failure-101")) tags;;
val error_tags : Logs.Tag.set = <abstr>
# Logs.err (fun m -> m "There was a failure" ~tags:error_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","error.code":"failure-101","log.level":"error","log.logger":"application","message":"There was a failure"}
- : unit = ()
```

Event logs can be quite descriptive.
```ocaml
# let file_uri = Uri.of_string "file:///home/ecs/newfile.ext";;
val file_uri : Uri.t = <abstr>
# let event_tags = Ecs.add_tags [Service (Name "file_saver"); Service (Version "git commit hash"); Event (Category [File]); Event (Kind Event); Event (Outcome Success); Event (Type [Creation]); Event (Url file_uri); File (Hash (Md5 "NOT A HASH")); Trace (Trace_id "unique 'global' id"); Trace (Transaction_id "unique 'local' id")] tags;;
val event_tags : Logs.Tag.set = <abstr>
# Logs.info (fun m -> m "Not sure this message is necessary" ~tags:event_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","event.category":["file"],"event.kind":"event","event.outcome":"success","event.type":["creation"],"event.url":"file:///home/ecs/newfile.ext","file.hash.md5":"NOT A HASH","log.level":"info","log.logger":"application","message":"Not sure this message is necessary","service.name":"file_saver","service.version":"git commit hash","trace.id":"unique 'global' id","transaction.id":"unique 'local' id"}
- : unit = ()
```

Maybe we're working with URLs - we can log that too!
```ocaml
# let uri = Uri.of_string "https://me:secret@example.com:9090/path?to=success#downhere";;
val uri : Uri.t = <abstr>
# let url_tags = Ecs.add_tags (Ecs.of_uri uri) tags;;
val url_tags : Logs.Tag.set = <abstr>
# Logs.info (fun m -> m "Finished request" ~tags:url_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","log.level":"info","log.logger":"application","message":"Finished request","url.domain":"example.com","url.fragment":"downhere","url.full":"https://me@example.com:9090/path?to=success#downhere","url.path":"/path","url.query":"to=success","url.scheme":"https","url.username":"me"}
- : unit = ()
```
Look, no passwords!

File information can be relevant.
```ocaml
# let file_tags = Ecs.add_tags [File (Hash (Md5 "INVALID MD5")); File (Size 8192)] tags;;
val file_tags : Logs.Tag.set = <abstr>
# Logs.info (fun m -> m "Saved file" ~tags:file_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","file.hash.md5":"INVALID MD5","file.size":8192,"log.level":"info","log.logger":"application","message":"Saved file"}
- : unit = ()
```

Callsite information is useful but will need a ppx to be less verbose.
```ocaml
# let log_tags = Ecs.add_tags [Log (Origin_file "README.md")] tags;;
val log_tags : Logs.Tag.set = <abstr>
# Logs.warn (fun m -> m "Made it this far" ~tags:log_tags);;
{"@timestamp":"2052-02-05T20:58:14.386Z","ecs.version":"1.5.0","log.level":"warning","log.logger":"application","log.origin.file":"README.md","message":"Made it this far"}
- : unit = ()
```

[ECS]: https://www.elastic.co/guide/en/ecs/current/ecs-reference.html
