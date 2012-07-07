%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2012, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo D'incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(espresso_beam_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    EnvManager = {env_manager, {env_manager, start_link, []},
                  Restart, Shutdown, worker, [env_manager]},

    WolfSup = {wolf_sup, {wolf_sup, start_link, []},
                 Restart, Shutdown, Type, [wolf_sup]},
    
    RabbitSup = {rabbit_sup, {rabbit_sup, start_link, []},
                  Restart, Shutdown, Type, [rabbit_sup]},
    
    CarrotSup = {carrot_sup, {carrot_sup, start_link, []},
                  Restart, Shutdown, Type, [carrot_sup]},
    
    {ok, {SupFlags, [EnvManager, CarrotSup, RabbitSup, WolfSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
