-module(eventlogger).

-export([log/2]).

-spec log(Event :: atom(), Output :: binary()) -> ok.
log(Event, Output) when is_list(Output) ->
    log(Event, unicode:characters_to_binary(Output));
log(Event, Output) ->
    eventlogger_sup:notify(Event, Output).
