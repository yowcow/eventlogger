%%%-------------------------------------------------------------------
%% @doc eventlogger public API
%% @end
%%%-------------------------------------------------------------------

-module(eventlogger_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eventlogger_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
