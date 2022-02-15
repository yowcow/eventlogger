-module(eventlogger_writer).

-behavior(gen_event).

-include_lib("kernel/include/logger.hrl").

-export([init/1, terminate/2, handle_call/2, handle_event/2, handle_info/2]).

-record(state,
        {event = default :: atom(),
         file :: string(),
         modes = [append, raw, delayed_write] :: [file:mode()],
         maxbytes = 0 :: non_neg_integer(),
         rotate = 1 :: non_neg_integer(),
         delim = <<"\n">> :: binary(),
         iodev :: file:io_device(),
         wbytes = 0 :: integer()}).

-type state() :: #state{}.
-type args() :: [term()].

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
                        ({rotate, V}, Acc) ->
                            Acc#state{rotate = V};
                        ({delim, V}, Acc) ->
                            Acc#state{delim = V};
                        (_, Acc) ->
                            Acc
                    end,
                    #state{},
                    Args),
    case eventlogger_rotator:open_rotated(State#state.file,
                                         State#state.modes,
                                         State#state.maxbytes,
                                         State#state.rotate)
    of
        {{ok, IoDevice}, WrittenBytes} ->
            {ok, State#state{iodev = IoDevice, wbytes = WrittenBytes}};
        {Err, _} ->
            Err
    end.

-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("terminate (~p, ~p)", [Reason, State]),
    ok.

handle_call(Req, State) ->
    ?LOG_WARNING("unhandled call (~p, ~p)", [Req, State]),
    {reply, {error, {unhandled_call, Req}}, State}.

handle_event({Event, Bytes} = Req, #state{event = Event} = State) ->
    #state{maxbytes = MaxBytes,
           iodev = IoDevice,
           delim = Delimiter,
           wbytes = WrittenBytes} =
        State,
    Output = <<Bytes/binary, Delimiter/binary>>,
    case file:write(IoDevice, Output) of
        ok ->
            Ret = case MaxBytes of
                      0 ->
                          {ok, {WrittenBytes + byte_size(Output), IoDevice}};
                      _ ->
                          CurWrittenBytes = WrittenBytes + byte_size(Output),
                          case CurWrittenBytes < MaxBytes of
                              true ->
                                  {ok, {CurWrittenBytes, IoDevice}};
                              _ ->
                                  _ = file:close(IoDevice),
                                  case eventlogger_rotator:open_rotated(State#state.file,
                                                                       State#state.modes,
                                                                       State#state.maxbytes,
                                                                       State#state.rotate)
                                  of
                                      {{ok, IoD}, WBytes} ->
                                          {ok, {WBytes, IoD}};
                                      Err ->
                                          Err
                                  end
                          end
                  end,
            case Ret of
                {ok, {NewWrittenBytes, NewIoDevice}} ->
                    {ok, State#state{iodev = NewIoDevice, wbytes = NewWrittenBytes}};
                Err2 ->
                    ?LOG_ERROR("failed rotating files: ~p (~p, ~p)", [Err2, Req, State]),
                    remove_handler
            end;
        Err1 ->
            ?LOG_ERROR("failed writing to the io_device: ~p (~p, ~p)", [Err1, Req, State]),
            remove_handler
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(Info, State) ->
    ?LOG_WARNING("unhandled info (~p, ~p)", [Info, State]),
    {ok, State}.
