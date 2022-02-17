-module(eventlogger_file_writer).

-behavior(gen_event).

-export([init/1, terminate/2, handle_call/2, handle_event/2, handle_info/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

%% Taken from lager_file_backend
-define(DEFAULT_SYNC_INTERVAL, 1000).
-define(DEFAULT_SYNC_SIZE, 1024 * 64). %% 64kb

-record(state,
        {event = default :: atom(),
         file = undefined :: file:name_all() | undefined,
         modes = [append, raw, {delayed_write, ?DEFAULT_SYNC_SIZE, ?DEFAULT_SYNC_INTERVAL}] ::
             [file:mode()],
         maxbytes = infinity :: maxbytes(),
         count = infinity :: count(),
         delimiter = <<"\n">> :: binary(),
         io = undefined :: io() | undefined,
         wbytes = 0 :: integer()}).

-type inode() :: non_neg_integer().
-type io() :: {file:io_device(), inode()}.
-type maxbytes() :: eventlogger_file_rotator:maxbytes().
-type count() :: eventlogger_file_rotator:count().
-type state() :: #state{}.
-type args() ::
    [{event, atom()} |
     {file, file:name_all()} |
     {modes, [file:mode()]} |
     {maxbytes, maxbytes()} |
     {count, count()} |
     {delimiter, binary()}].

-spec init(Args :: args()) -> {ok, state()}.
init(Args) ->
    State =
        lists:foldl(fun ({event, V}, Acc) ->
                            Acc#state{event = V};
                        ({file, V}, Acc) ->
                            Acc#state{file = V};
                        ({modes, V}, Acc) ->
                            Acc#state{modes = V};
                        ({maxbytes, V}, Acc) ->
                            Acc#state{maxbytes = V};
                        ({count, V}, Acc) ->
                            Acc#state{count = V};
                        ({delimiter, V}, Acc) ->
                            Acc#state{delimiter = V};
                        (_, Acc) ->
                            Acc
                    end,
                    #state{},
                    Args),
    case open_file(State) of
        {ok, {Io, WrittenBytes}} ->
            {ok, State#state{io = Io, wbytes = WrittenBytes}};
        Err ->
            Err
    end.

-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(Reason, #state{io = {IoDevice, _}} = State) ->
    ?LOG_INFO("terminate (~p, ~p)", [Reason, State]),
    file:close(IoDevice),
    ok.

handle_call(dump_state, State) ->
    {ok,
     #{event => State#state.event,
       file => State#state.file,
       modes => State#state.modes,
       maxbytes => State#state.maxbytes,
       count => State#state.count,
       delimiter => State#state.delimiter,
       io => State#state.io,
       wbytes => State#state.wbytes},
     State};
handle_call(Req, State) ->
    ?LOG_WARNING("unhandled call (~p, ~p)", [Req, State]),
    {ok, {error, {unhandled_call, Req}}, State}.

handle_event({Event, Output} = Req, #state{event = Event} = State) ->
    case ensure_file(State) of
        {ok, {Io, WrittenBytes}} ->
            case write_to_file(Output, State#state{io = Io, wbytes = WrittenBytes}) of
                {ok, NewState} ->
                    {ok, NewState};
                {error, Reason2} ->
                    ?LOG_ERROR("failed writing to file: ~p (~p, ~p)", [Reason2, Req, State]),
                    remove_handler
            end;
        {error, Reason1} ->
            ?LOG_ERROR("failed ensuring an open file: ~p (~p, ~p)", [Reason1, Req, State]),
            remove_handler
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(Info, State) ->
    ?LOG_WARNING("unhandled info (~p, ~p)", [Info, State]),
    {ok, State}.

%% private funs
-spec open_file(State :: state()) -> {ok, {io(), non_neg_integer()}} | {error, term()}.
open_file(State) ->
    case eventlogger_file_rotator:open(State#state.file,
                                       State#state.modes,
                                       State#state.maxbytes,
                                       State#state.count)
    of
        {{ok, IoDevice}, WrittenBytes} ->
            {ok, #file_info{inode = Inode}} = file:read_file_info(State#state.file, [raw]),
            {ok, {{IoDevice, Inode}, WrittenBytes}};
        {{error, Reason}, _} ->
            {error, Reason}
    end.

-spec ensure_file(State :: state()) -> {ok, {io(), non_neg_integer()}} | {error, term()}.
ensure_file(#state{file = File, io = {_, Inode0}} = State) ->
    case is_file_changed(File, Inode0) of
        true ->
            ?LOG_DEBUG("(~p) detected file change on ~ts", [File]),
            open_file(State);
        _ ->
            {ok, {State#state.io, State#state.wbytes}}
    end.

-spec is_file_changed(file:name_all(), inode()) -> boolean().
is_file_changed(File, Inode0) ->
    case file:read_file_info(File, [raw]) of
        {ok, #file_info{inode = Inode1}} ->
            Inode0 =/= Inode1;
        _ ->
            true
    end.

-spec write_to_file(Output0 :: binary(), State :: state()) ->
                       {ok, state()} | {error, term()}.
write_to_file(Output0,
              #state{maxbytes = MaxBytes,
                     delimiter = Delimiter,
                     io = {IoDevice0, _} = Io0,
                     wbytes = WrittenBytes0} =
                  State) ->
    Output = <<Output0/binary, Delimiter/binary>>,
    case file:write(IoDevice0, Output) of
        ok ->
            Result =
                case MaxBytes of
                    infinity ->
                        {ok, {Io0, WrittenBytes0 + byte_size(Output)}};
                    _ ->
                        CurWrittenBytes = WrittenBytes0 + byte_size(Output),
                        case CurWrittenBytes < MaxBytes of
                            true ->
                                {ok, {Io0, CurWrittenBytes}};
                            _ ->
                                eventlogger_file_rotator:close(IoDevice0),
                                open_file(State)
                        end
                end,
            case Result of
                {ok, {Io1, WrittenBytes1}} ->
                    {ok, State#state{io = Io1, wbytes = WrittenBytes1}};
                Err2 ->
                    Err2
            end;
        Err1 ->
            Err1
    end.
