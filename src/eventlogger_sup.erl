%%%-------------------------------------------------------------------
%% @doc eventlogger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eventlogger_sup).

-behaviour(supervisor).

-export([start_link/0, add_writer/3, delete_writer/2, notify/2]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MANAGER, eventlogger_manager).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    Ret = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    case Ret of
        {ok, _} ->
            case application:get_env(eventlogger, loggers) of
                {ok, Loggers} ->
                    lists:map(fun({Writer, Id, Args}) -> add_writer(Writer, Id, Args) end, Loggers);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    Ret.

-spec add_writer(Writer :: module(), Id :: term(), Args :: proplists:proplist()) -> ok.
add_writer(Writer, Id, Args) ->
    gen_event:add_handler(?MANAGER, {Writer, Id}, Args).

-spec delete_writer(Module :: module(), Id :: term()) -> ok | {error, term()}.
delete_writer(Module, Id) ->
    gen_event:delete_handler(?MANAGER, {Module, Id}).

-spec notify(Event :: atom(), Log :: binary()) -> ok.
notify(Event, Log) ->
    gen_event:notify(?MANAGER, {Event, Log}).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 0,
          period => 1},
    ChildSpecs =
        [#{id => ?MANAGER,
           start => {gen_event, start_link, [{local, ?MANAGER}]},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => []}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
