%%%-------------------------------------------------------------------
%%% @author Paolo <paolo@ubuntu.ubuntu-domain>
%%% @copyright (C) 2012, Paolo
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo <paolo@ubuntu.ubuntu-domain>
%%%-------------------------------------------------------------------
-module(carrot_sup).

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

    %% Restart is 'permanent' so that if a carrot is eaten
    %% by a rabbit, it gets automatically respwaned
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    %% Spawn a certain number of carrots when initializing
    spawn_link(?MODULE, start_carrots, [?NUM_CARROTS]),

    ACarrot = {carrot, {carrot, start_link, []},
              Restart, Shutdown, Type, [carrot]},

    {ok, {SupFlags, [ACarrot]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_carrots(Num_Carrots) ->
    [start_carrot() || _ <- lists:seq(1, Num_Carrots)],
    ok.

start_carrot() ->
    supervisor:start_child(?MODULE, []).
