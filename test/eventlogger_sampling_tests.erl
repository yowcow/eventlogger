-module(eventlogger_sampling_tests).

-include_lib("eunit/include/eunit.hrl").

sampling_rate_1_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link(),
        LogFile = [TmpDir, "/sampling_1.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
                                   {sampling_rate, 1.0}]),
        [{tmpdir, TmpDir}, {logfile, LogFile}, {gen_event, Pid}]
     end,
     fun(Args) ->
        Pid = proplists:get_value(gen_event, Args),
        gen_event:stop(Pid)
     end,
     fun(Args) ->
        LogFile = proplists:get_value(logfile, Args),
        Pid = proplists:get_value(gen_event, Args),
        [{"sampling_rate 1.0 writes all logs",
          fun() ->
             [ ok = gen_event:sync_notify(Pid, {foo, <<"log">>}) || _ <- lists:seq(1, 100) ],
             {ok, Data} = file:read_file(LogFile),
             Lines = binary:split(Data, <<"
">>, [global, trim]),
             ?assertEqual(100, length(Lines))
          end}]
     end}.

sampling_rate_0_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link(),
        LogFile = [TmpDir, "/sampling_0.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
                                   {sampling_rate, 0.0}]),
        [{tmpdir, TmpDir}, {logfile, LogFile}, {gen_event, Pid}]
     end,
     fun(Args) ->
        Pid = proplists:get_value(gen_event, Args),
        gen_event:stop(Pid)
     end,
     fun(Args) ->
        LogFile = proplists:get_value(logfile, Args),
        Pid = proplists:get_value(gen_event, Args),
        [{"sampling_rate 0.0 writes no logs",
          fun() ->
             [ ok = gen_event:sync_notify(Pid, {foo, <<"log">>}) || _ <- lists:seq(1, 100) ],
             {ok, Data} = file:read_file(LogFile),
             ?assertEqual(<<>>, Data)
          end}]
     end}.

sampling_rate_half_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        {ok, Pid} = gen_event:start_link(),
        LogFile = [TmpDir, "/sampling_half.log"],
        ok =
            gen_event:add_handler(Pid,
                                  {eventlogger_file_writer, 1},
                                  [{event, foo},
                                   {file, LogFile},
                                   {modes, [append, raw, write]},
                                   {sampling_rate, 0.5}]),
        [{tmpdir, TmpDir}, {logfile, LogFile}, {gen_event, Pid}]
     end,
     fun(Args) ->
        Pid = proplists:get_value(gen_event, Args),
        gen_event:stop(Pid)
     end,
     fun(Args) ->
        LogFile = proplists:get_value(logfile, Args),
        Pid = proplists:get_value(gen_event, Args),
        [{"sampling_rate 0.5 writes approx 50% logs",
          fun() ->
             Count = 1000,
             [ ok = gen_event:sync_notify(Pid, {foo, <<"log">>}) || _ <- lists:seq(1, Count) ],
             {ok, Data} = file:read_file(LogFile),
             Lines = binary:split(Data, <<"
">>, [global, trim]),
             Len = length(Lines),
             %% Over 1000 samples, 0.5 should be within [400, 600] with very high probability
             ?assert(Len > 400),
             ?assert(Len < 600)
          end}]
     end}.
