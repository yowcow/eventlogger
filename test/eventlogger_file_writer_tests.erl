-module(eventlogger_file_writer_tests).

-include_lib("eunit/include/eunit.hrl").

handler_with_maxbytes_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link({local, eventlogger_file_writer_tests_1}),
        LogFile = [TmpDir, "/test1.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
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
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test1.log output", ?_assertEqual({ok, <<>>}, FileData)},
                  {Title ++ ": state",
                   ?_assertMatch(#{modes := [append, raw, write], wbytes := 0}, State)}]
              end},
             {"handled event is written to file",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar111">>}),
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test1.log output", ?_assertEqual({ok, <<"foobar111\n">>}, FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 10}, State)}]
              end},
             {"rotates when maxbytes reaches",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar222">>}), %% 20 bytes
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar33333">>}), %% 30 bytes
                 FileData = file:read_file(LogFile),
                 File0Data = file:read_file([LogFile, ".0"]),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test1.log output",
                   ?_assertEqual({ok, <<"foobar33333\n">>}, FileData)},
                  {Title ++ ": test1.log.0 output",
                   ?_assertEqual({ok, <<"foobar111\nfoobar222\n">>}, File0Data)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 12}, State)}]
              end},
             {"somehow file has vanished but write continues",
              fun(Title) ->
                 file:delete(LogFile),
                 ok = gen_event:sync_notify(Pid, {foo, <<"vanished111">>}), %% 20 bytes
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test1.log output",
                   ?_assertEqual({ok, <<"vanished111\n">>}, FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 12}, State)}]
              end},
             {"somehow file is recreated but write continues",
              fun(Title) ->
                 ok = file:delete(LogFile),
                 ok = file:write_file(LogFile, <<"foo1\n">>, [write, raw]),
                 ok = gen_event:sync_notify(Pid, {foo, <<"foo2">>}), %% 12 bytes
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test1.log output",
                   ?_assertEqual({ok, <<"foo1\nfoo2\n">>}, FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 10}, State)}]
              end}],
        F = fun({Title, Test}) -> Test(Title) end,
        lists:map(F, Cases)
     end}.

handler_without_maxbytes_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link({local, eventlogger_file_writer_tests_2}),
        LogFile = [TmpDir, "/test2.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
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
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output", ?_assertEqual({ok, <<>>}, FileData)},
                  {Title ++ ": state",
                   ?_assertMatch(#{modes := [append, raw, write], wbytes := 0}, State)}]
              end},
             {"handled event is written to file",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar111">>}),
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output", ?_assertEqual({ok, <<"foobar111\n">>}, FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 10}, State)}]
              end},
             {"handler keeps writing to the same file",
              fun(Title) ->
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar222">>}),
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar333">>}),
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar444">>}),
                 ok = gen_event:sync_notify(Pid, {foo, <<"foobar555">>}),
                 FileData = file:read_file(LogFile),
                 State = gen_event:call(Pid, {eventlogger_file_writer, 1}, dump_state),
                 [{Title ++ ": test2.log output",
                   ?_assertEqual({ok,
                                  <<"foobar111\n",
                                    "foobar222\n",
                                    "foobar333\n",
                                    "foobar444\n",
                                    "foobar555\n">>},
                                 FileData)},
                  {Title ++ ": state", ?_assertMatch(#{wbytes := 50}, State)}]
              end}],
        F = fun({Title, Test}) -> Test(Title) end,
        lists:map(F, Cases)
     end}.
