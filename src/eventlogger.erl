-module(eventlogger).

-export([write/2]).

-spec write(Event :: atom(), Output :: binary()) -> ok.
write(Event, Output) ->
    eventlogger_sup:notify(Event, Output).
