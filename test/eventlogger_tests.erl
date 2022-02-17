-module(eventlogger_tests).

-include_lib("eunit/include/eunit.hrl").

log_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link({local, eventlogger_manager}),
        LogFile = [TmpDir, "/test1.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
                                   {maxbytes, 1000},
                                   {count, 2}]),
        [{tmpdir, TmpDir}, {logfile, LogFile}, {gen_event, Pid}]
     end,
     fun(Args) ->
             Pid = proplists:get_value(gen_event, Args),
             gen_event:stop(Pid)
     end,
     fun(Args) ->
             LogFile = proplists:get_value(logfile, Args),
             Cases = [
                      {"log binary() input",
                       <<"hoge">>,
                       <<"hoge\n"/utf8>>},
                      {"log CJK binary() input",
                       <<"こんちは"/utf8>>,
                       <<
                         "hoge\n",
                         "こんちは\n"/utf8
                       >>},
                      {"log string() input",
                       "fuga",
                       <<
                         "hoge\n",
                         "こんちは\n"/utf8,
                         "fuga\n"
                       >>},
                      {"log CJK string() input",
                       unicode:characters_to_list("ばーい"),
                       <<
                         "hoge\n",
                         "こんちは\n"/utf8,
                         "fuga\n",
                         "ばーい\n"/utf8
                       >>}
                     ],
             F = fun({Title, Input, Expected}) ->
                         ok = eventlogger:log(foo, Input),
                         FileData = file:read_file(LogFile),
                         [{Title, ?_assertEqual({ok, Expected}, FileData)}]
                 end,
             lists:map(F, Cases)
     end}.
