-module(eventlogger_utils).

-export([is_sampled/1]).

-spec is_sampled(float()) -> boolean().
is_sampled(Rate) when Rate =< 0.0 ->
    false;
is_sampled(Rate) when Rate >= 1.0 ->
    true;
is_sampled(Rate) ->
    rand:uniform() =< Rate.
