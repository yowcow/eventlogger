-module(eventlogger_writer_tests).

-include_lib("eunit/include/eunit.hrl").

handler_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link({local, ?MODULE}),
        LogFile = [TmpDir, "/test2.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
                                   {maxbytes, 15},
                                   {count, 2}]),
        [{tmpdir, TmpDir}, {logfile, LogFile}, {gen_event, Pid}]
     end,
     fun(Args) ->
        Pid = proplists:get_value(gen_event, Args),
        gen_event:stop(Pid)
     end,
     fun(Args) ->
        LogFile = proplists:get_value(logfile, Args),
        Pid = proplists:get_value(gen_event, Args),
        Cases =
            [{"unhandled event is just ignored",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {bar, <<"foobar000">>}),
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output", ?_assertEqual({ok, <<>>}, FileData)},
                  {Title ++ ": state",
                   ?_assertMatch(#{modes := [append, raw, write], wbytes := 0}, State)}]
              end},
             {"handled event is written to file",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar111">>}),
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output", ?_assertEqual({ok, <<"foobar111\n">>}, FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 10}, State)}]
              end},
             {"rotates when maxbytes reaches",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar222">>}), %% 20 bytes
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar33333">>}), %% 30 bytes
                 FileData = file:read_file(LogFile),
                 File1Data = file:read_file([LogFile, ".1"]),
                 State = gen_event:call(Pid, {eventlogger_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output",
                   ?_assertEqual({ok, <<"foobar33333\n">>}, FileData)},
                  {Title ++ ": test2.log.1 output",
                   ?_assertEqual({ok, <<"foobar111\nfoobar222\n">>}, File1Data)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 12}, State)}]
              end}],
        F = fun({Title, Test}) -> Test(Title) end,
        lists:map(F, Cases)
     end}.
