-module(eventlogger).

-export([format/3, log/2]).

-spec format(Event :: atom(), Format :: string(), Args :: [term()]) -> ok.
format(Event, Format, Args) ->
    log(Event, io_lib:format(Format, Args)).

-spec log(Event :: atom(), Output :: binary()|string()) -> ok.
log(Event, Output) when is_list(Output) ->
    log(Event, unicode:characters_to_binary(Output));
log(Event, Output) ->
    eventlogger_sup:notify(Event, Output).
