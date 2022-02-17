-module(eventlogger_file_rotator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAXBYTES, 10).
-define(COUNT, 2).

open_close_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        [{tmpdir, TmpDir}]
     end,
     fun(_) -> ok end,
     fun(Args) ->
        TmpDir = proplists:get_value(tmpdir, Args),
        Cases =
            [{"initial open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              ?COUNT),
                 ok = file:write(IoDevice, <<"hoge\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\n">>}, FileData)}]
              end},
             {"re-open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              ?COUNT),
                 ok = file:write(IoDevice, <<"fuga\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 [{Title ++ ": wbytes", ?_assertEqual(5, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, FileData)}]
              end},
             {"rotate creates test1.log.1 and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              ?COUNT),
                 ok = file:write(IoDevice, <<"foobarbuz123\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 File1Data = file:read_file([TmpDir, "/test1.log.1"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz123\n">>}, FileData)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File1Data)}]
              end},
             {"rotate creates test1.log.2 and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              ?COUNT),
                 ok = file:write(IoDevice, <<"foobarbuz234\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 File1Data = file:read_file([TmpDir, "/test1.log.1"]),
                 File2Data = file:read_file([TmpDir, "/test1.log.2"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz234\n">>}, FileData)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File1Data)},
                  {Title ++ ": test1.log.2", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File2Data)}]
              end},
             {"rotate deletes oldest generation and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              ?COUNT),
                 ok = file:write(IoDevice, <<"foobarbuz345\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 File1Data = file:read_file([TmpDir, "/test1.log.1"]),
                 File2Data = file:read_file([TmpDir, "/test1.log.2"]),
                 File3Exists = filelib:is_regular([tmpDir, "/test1.log.3"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz345\n">>}, FileData)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"foobarbuz234\n">>}, File1Data)},
                  {Title ++ ": test1.log.2", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File2Data)},
                  {Title ++ ": deletes oldest", ?_assertNot(File3Exists)}]
              end},
             {"count=infinity rotates current file to the greatest index",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open([TmpDir, "/test1.log"],
                                              [write, append, raw],
                                              ?MAXBYTES,
                                              infinity),
                 ok = file:write(IoDevice, <<"foobarbuz456\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 File1Data = file:read_file([TmpDir, "/test1.log.1"]),
                 File2Data = file:read_file([TmpDir, "/test1.log.2"]),
                 File3Data = file:read_file([TmpDir, "/test1.log.3"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz456\n">>}, FileData)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"foobarbuz234\n">>}, File1Data)},
                  {Title ++ ": test1.log.2", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File2Data)},
                  {Title ++ ": test1.log.3", ?_assertEqual({ok, <<"foobarbuz345\n">>}, File3Data)}]
              end}],
        F = fun({Title, Fun}) -> Fun(Title) end,
        lists:map(F, Cases)
     end}.
