-module(eventlogger_rotator).

-export([open/4, close/1]).

-include_lib("kernel/include/logger.hrl").

-spec open(File :: string(),
           Modes :: [file:mode()],
           MaxBytes :: non_neg_integer(),
           Count :: non_neg_integer()) ->
              {ok, file:io_device()} | {error, term()}.
open(File, Modes, MaxBytes, Count) ->
    WrittenBytes =
        case {filelib:is_regular(File), MaxBytes} of
            {false, _} ->
                %% if file not exists, then just create a new file
                0;
            {_, 0} ->
                %% if no max bytes is set, then just open the existing file
                filelib:file_size(File);
            _ ->
                CurWrittenBytes = filelib:file_size(File),
                case CurWrittenBytes < MaxBytes of
                    true ->
                        CurWrittenBytes;
                    _ ->
                        rotate_files(File, Count),
                        0
                end
        end,
    {file:open(File, Modes), WrittenBytes}.

rotate_files(File, 0) ->
    %% without limit on rotation count
    rotate_infinite(File, 1);
rotate_files(File, Count) ->
    %% with limit on rotation count
    rotate_finite(File, Count, Count).

rotate_infinite(File, Index) ->
    CurFile = [File, ".", integer_to_list(Index)],
    case filelib:is_regular(CurFile) of
        false ->
            ?LOG_DEBUG("(~p) renaming ~p -> ~p", [?MODULE, File, CurFile]),
            file:rename(File, CurFile);
        _ ->
            rotate_infinite(File, Index + 1)
    end.

rotate_finite(_File, _Rotate, 0) ->
    ok;
rotate_finite(File, Count, Index) ->
    CurFile = [File, ".", integer_to_list(Index)],
    NextFile =
        case Index of
            1 ->
                File;
            _ ->
                [File, ".", integer_to_list(Index - 1)]
        end,
    case filelib:is_regular(NextFile) of
        true ->
            ?LOG_DEBUG("(~p) renaming ~p -> ~p", [?MODULE, NextFile, CurFile]),
            file:rename(NextFile, CurFile);
        _ ->
            ok
    end,
    rotate_finite(File, Count, Index - 1).

-spec close(IoDevice :: file:io_device()) -> ok.
close(IoDevice) ->
    %% flush and ensure close
    _ = file:datasync(IoDevice),
    _ = file:close(IoDevice),
    _ = file:close(IoDevice),
    ok.
