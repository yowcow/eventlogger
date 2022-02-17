-module(eventlogger_file_rotator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAXBYTES, 10).
-define(COUNT, 2).

finite_rotation_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        [{tmpdir, TmpDir}]
     end,
     fun(_) -> ok end,
     fun(Args) ->
        TmpDir = proplists:get_value(tmpdir, Args),
        TmpPath = [TmpDir, "/test1.log"],
        Modes = [write, append, raw],
        Count = ?COUNT,
        Cases =
            [{"initial open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"hoge\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file([TmpDir, "/test1.log"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\n">>}, FileData)}]
              end},
             {"re-open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"fuga\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 [{Title ++ ": wbytes", ?_assertEqual(5, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, FileData)}]
              end},
             {"rotate creates test1.log.0 and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"foobarbuz123\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 File1Data = file:read_file([TmpPath, ".0"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz123\n">>}, FileData)},
                  {Title ++ ": test1.log.0", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File1Data)}]
              end},
             {"rotate creates test1.log.1 and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"foobarbuz234\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 File0Data = file:read_file([TmpPath, ".0"]),
                 File1Data = file:read_file([TmpPath, ".1"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz234\n">>}, FileData)},
                  {Title ++ ": test1.log.0", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File0Data)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File1Data)}]
              end},
             {"rotate deletes oldest generation and open test1.log",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"foobarbuz345\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 File0Data = file:read_file([TmpPath, ".0"]),
                 File1Data = file:read_file([TmpPath, ".1"]),
                 File2Exists = filelib:is_regular([tmpDir, "/test1.log.2"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz345\n">>}, FileData)},
                  {Title ++ ": test1.log.0", ?_assertEqual({ok, <<"foobarbuz234\n">>}, File0Data)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File1Data)},
                  {Title ++ ": deletes oldest", ?_assertNot(File2Exists)}]
              end}],
        F = fun({Title, Fun}) -> Fun(Title) end,
        lists:map(F, Cases)
     end}.

infinite_rotation_test_() ->
    {setup,
     fun() ->
        {ok, TmpDir} = eventlogger_util:tmpdir(["/tmp/", ?MODULE]),
        [{tmpdir, TmpDir}]
     end,
     fun(_) -> ok end,
     fun(Args) ->
        TmpDir = proplists:get_value(tmpdir, Args),
        TmpPath = [TmpDir, "/test2.log"],
        Modes = [write, append, raw],
        Count = infinity,
        Cases =
            [{"opens log file",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"hoge\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\n">>}, FileData)}]
              end},
             {"re-open log file",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"fuga\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 [{Title ++ ": wbytes", ?_assertEqual(5, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, FileData)}]
              end},
             {"rotate creates file w/ suffix .0 and opens log file",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"foobarbuz123\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 File0Data = file:read_file([TmpPath, ".0"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz123\n">>}, FileData)},
                  {Title ++ ": test1.log.0", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File0Data)}]
              end},
             {"rotate creates file w/ suffix .1 and opens log file",
              fun(Title) ->
                 {{ok, IoDevice}, WBytes} =
                     eventlogger_file_rotator:open(TmpPath, Modes, ?MAXBYTES, Count),
                 ok = file:write(IoDevice, <<"foobarbuz234\n">>),
                 ok = eventlogger_file_rotator:close(IoDevice),
                 FileData = file:read_file(TmpPath),
                 File0Data = file:read_file([TmpPath, ".0"]),
                 File1Data = file:read_file([TmpPath, ".1"]),
                 [{Title ++ ": wbytes", ?_assertEqual(0, WBytes)},
                  {Title ++ ": test1.log", ?_assertEqual({ok, <<"foobarbuz234\n">>}, FileData)},
                  {Title ++ ": test1.log.0", ?_assertEqual({ok, <<"hoge\nfuga\n">>}, File0Data)},
                  {Title ++ ": test1.log.1", ?_assertEqual({ok, <<"foobarbuz123\n">>}, File1Data)}]
              end}],
        F = fun({Title, Fun}) -> Fun(Title) end,
        lists:map(F, Cases)
     end}.
