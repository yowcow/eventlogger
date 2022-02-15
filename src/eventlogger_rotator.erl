-module(eventlogger_rotator).

-export([open/4, close/1]).

-spec open(File :: string(),
           Modes :: [file:mode()],
           MaxBytes :: non_neg_integer(),
           Rotate :: non_neg_integer()) ->
              {ok, file:io_device()} | {error, term()}.
open(File, Modes, MaxBytes, Rotate) ->
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
                        rotate_files(File, Rotate, Rotate),
                        0
                end
        end,
    {file:open(File, Modes), WrittenBytes}.

rotate_files(_File, _Rotate, 0) ->
    ok;
rotate_files(File, Rotate, Index) ->
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
            case filelib:is_regular(CurFile) of
                true ->
                    file:delete(CurFile, [raw]);
                _ ->
                    ok
            end,
            file:rename(NextFile, CurFile);
        _ ->
            ok
    end,
    rotate_files(File, Rotate, Index - 1).

-spec close(IoDevice :: file:io_device()) -> ok.
close(IoDevice) ->
    %% flush and ensure close
    _ = file:datasync(IoDevice),
    _ = file:close(IoDevice),
    _ = file:close(IoDevice),
    ok.
