%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2012, Paolo
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(wolf_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-include("espresso_beam.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    %% Create pg group for all wolves
    pg:create(wolves),

    %% Spawn a certain number of wolves when initializing
    spawn_link(fun start_wolves/0),

    AWolf = {wolf, {wolf, start_link, []},
              Restart, Shutdown, Type, [wolf]},

    {ok, {SupFlags, [AWolf]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_wolves() ->
    [start_wolf() || _ <- lists:seq(1, ?NUM_WOLVES)],
    ok.

start_wolf() ->
    supervisor:start_child(?MODULE, []).
