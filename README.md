eventlogger
=====

A logger to record events to various files.


HOW TO USE
----------

Add a dep:

```
{eventlogger, {git, "git://github.com/yowcow/eventlogger.git", {branch, "master"}}}
```

Add arbitrary logger configurations:

```
{eventlogger, [
               {loggers, [
                          {eventlogger_writer,
                           foo_logger,
                           [{event, foo},
                            {file, "log/foo.log"},
                            {maxbytes, 10485760}, %% 10 MB per file
                            {count, 3} %% rotation keeps 3 generations
                           ]},
                          {eventlogger_writer,
                           bar_logger,
                           [{event, bar},
                            {file, "log/bar.log"},
                            {maxbytes, 1048576} %% 1 MB per file
                            {count, infinity} %% no limit on rotation count
                           ]},
                          {eventlogger_writer,
                           buz_logger,
                           [{event, buz},
                            {file, "log/buz.log"},
                            {maxbytes, infinity} %% no limit no file size
                           ]}
                         ]}
              ]}
```

Write events:

```
eventlogger:write(foo, <<"Hello log/foo.log">>).
eventlogger:write(bar, <<"Hi log/bar.log">>).
eventlogger:write(buz, <<"Hi log/buz.log">>).
```


BUILD
-----

    $ rebar3 compile
