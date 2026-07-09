-module(eventlogger_file_writer).

-behavior(gen_event).

-export([init/1, terminate/2, handle_call/2, handle_event/2, handle_info/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").

%% Taken from lager_file_backend
-define(DEFAULT_SYNC_INTERVAL, 1000).
-define(DEFAULT_SYNC_SIZE, 1024 * 64). %% 64kb

%% F6: detecting an externally-rotated/replaced file (e.g. by logrotate or a
%% symlink swap) requires stat'ing the file via file:read_file_info/2. Doing
%% this on every single event serializes an extra syscall per log line
%% through the single gen_event process. Instead, the check runs on a timer
%% and its result is cached in state; handle_event/2 always writes through
%% the cached io without stat'ing first.
-define(DEFAULT_CHECK_INTERVAL, 1000). %% ms
-define(CHECK_FILE_CHANGED, '$eventlogger_check_file_changed').

-record(state,
        {event = default :: atom(),
         file = undefined :: file:name_all() | undefined,
         modes = [append, raw, {delayed_write, ?DEFAULT_SYNC_SIZE, ?DEFAULT_SYNC_INTERVAL}] ::
             [file:mode()],
         maxbytes = infinity :: maxbytes(),
         count = infinity :: count(),
         delimiter = <<"\n">> :: binary(),
         sampling_rate = 1.0 :: float(),
         check_interval = ?DEFAULT_CHECK_INTERVAL :: non_neg_integer(),
         timer_ref = undefined :: reference() | undefined,
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
     {delimiter, binary()} |
     {sampling_rate, float()} |
     {check_interval, non_neg_integer()}].

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
                        ({sampling_rate, V}, Acc) ->
                            Acc#state{sampling_rate = float(V)};
                        ({check_interval, V}, Acc) ->
                            Acc#state{check_interval = V};
                        (_, Acc) ->
                            Acc
                    end,
                    #state{},
                    Args),
    case open_file(State) of
        {ok, {Io, WrittenBytes}} ->
            {ok, schedule_check(State#state{io = Io, wbytes = WrittenBytes})};
        Err ->
            Err
    end.

-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(Reason, #state{io = {IoDevice, _}, timer_ref = TimerRef} = State) ->
    ?LOG_INFO("terminate (~p, ~p)", [Reason, State]),
    cancel_check(TimerRef),
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
       sampling_rate => State#state.sampling_rate,
       check_interval => State#state.check_interval,
       timer_ref => State#state.timer_ref,
       io => State#state.io,
       wbytes => State#state.wbytes},
     State};
handle_call(Req, State) ->
    ?LOG_WARNING("unhandled call (~p, ~p)", [Req, State]),
    {ok, {error, {unhandled_call, Req}}, State}.

handle_event({Event, Output} = Req, #state{event = Event, sampling_rate = Rate} = State) ->
    case eventlogger_utils:is_sampled(Rate) of
        true ->
            case write_to_file(Output, State) of
                {ok, NewState} ->
                    {ok, NewState};
                {error, Reason} ->
                    ?LOG_ERROR("failed writing to file: ~p (~p, ~p)", [Reason, Req, State]),
                    remove_handler
            end;
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% Periodic, decoupled-from-writes check for an externally rotated/replaced
%% file (F6). Runs on ?DEFAULT_CHECK_INTERVAL (or the configured
%% check_interval) instead of on every handle_event/2 call.
%%
%% Several eventlogger_file_writer instances (e.g. rtb_v2_log and
%% req_res_log) are typically installed on the SAME gen_event manager, which
%% broadcasts any raw message (including our timer message) to every
%% installed handler's handle_info/2, not just the one that scheduled it.
%% erlang:start_timer/3 embeds a fresh reference in the delivered
%% {timeout, Ref, _} message, so we can tell "this is the check I scheduled"
%% (Ref matches our own timer_ref) apart from another handler's timer (or a
%% stale one of our own) and ignore the latter without rescheduling.
handle_info({timeout, Ref, ?CHECK_FILE_CHANGED}, #state{timer_ref = Ref} = State) ->
    NewState =
        case ensure_file(State) of
            {ok, {Io, WrittenBytes}} ->
                State#state{io = Io, wbytes = WrittenBytes};
            {error, Reason} ->
                ?LOG_ERROR("failed ensuring an open file: ~p (~p)", [Reason, State]),
                State
        end,
    {ok, schedule_check(NewState)};
handle_info({timeout, _OtherRef, ?CHECK_FILE_CHANGED}, State) ->
    %% Belongs to a different handler instance on the same manager (or a
    %% stale timer of our own); we already have our own timer pending.
    {ok, State};
handle_info(Info, State) ->
    ?LOG_WARNING("unhandled info (~p, ~p)", [Info, State]),
    {ok, State}.

%% private funs
-spec schedule_check(State :: state()) -> state().
schedule_check(#state{check_interval = Interval} = State) ->
    %% Clamp to avoid a tight reschedule loop (CPU spike) if check_interval
    %% is misconfigured to 0 or a negative value in sys.config.
    TimerRef = erlang:start_timer(max(1, Interval), self(), ?CHECK_FILE_CHANGED),
    State#state{timer_ref = TimerRef}.

-spec cancel_check(TimerRef :: reference() | undefined) -> ok.
cancel_check(undefined) ->
    ok;
cancel_check(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    ok.

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
ensure_file(#state{file = File, io = {OldIoDevice, Inode0}} = State) ->
    case is_file_changed(File, Inode0) of
        true ->
            ?LOG_DEBUG("detected file change on ~ts", [File]),
            %% Close the old fd only after a new one is confirmed open, so a
            %% failed reopen leaves the (still valid) old fd in place rather
            %% than dropping to zero open file handles.
            case open_file(State) of
                {ok, _} = Result ->
                    eventlogger_file_rotator:close(OldIoDevice),
                    Result;
                Err ->
                    Err
            end;
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
    %% Pass iodata straight to file:write/2 instead of concatenating into a
    %% new binary first -- avoids copying the whole (often multi-KB) Output0
    %% just to append a delimiter.
    OutputSize = byte_size(Output0) + byte_size(Delimiter),
    case file:write(IoDevice0, [Output0, Delimiter]) of
        ok ->
            Result =
                case MaxBytes of
                    infinity ->
                        {ok, {Io0, WrittenBytes0 + OutputSize}};
                    _ ->
                        CurWrittenBytes = WrittenBytes0 + OutputSize,
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
