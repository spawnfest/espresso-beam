%%%-------------------------------------------------------------------
%%% @author Loris Fichera <loris.fichera@gmail.com> 
%%% @author Mirko Bonadei <mirko.bonadei@gmail.com>
%%% @author Paolo D'Incau <paolo.dincau@gmail.com> 
%%% @copyright (C) 2012, Loris Fichera, Mirko Bonadei, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Loris Fichera <loris.fichera@gmail.com>
%%%-------------------------------------------------------------------
-module(env_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([allocate_me/2, 
	 give_me_close_cells_status/1,
	 update_me/2,
	 deallocate_location/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {actors}).
-record(actor, {pid, type, location}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

allocate_me(ActorPid, ActorType) ->
    gen_server:call(?SERVER, {allocate_me, ActorPid, ActorType}).

give_me_close_cells_status(ActorPid) ->
    gen_server:call(?SERVER, {give_me_close_cells_status, ActorPid}).

update_me(ActorPid, NewPos) ->
    gen_server:call(?SERVER, {update_me, ActorPid}).

deallocate_me(ActorPid) ->
    gen_server:cast(?SERVER, {deallocate_me, ActorPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A,B,C),
    {ok, #state{actors=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% !FIXME to be implemented
handle_call({allocate_me, ActorPid, ActorType}, _From, #state{actors=Actors} = State) ->
    Location = get_free_location(Actors),
    Actor = #actor{pid=ActorPid, type=ActorType, location=Location},
    {reply, Location, State#state{actors=[Actor|Actors]}};

handle_call(_Request, _From, State) ->
    io:format("~p~n", [State#state.actors]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% !FIXME to be implemented
handle_cast({deallocate_me, ActorPid}, #state{actors=Actors} = State) ->
    NewActors = lists:keydelete(ActorPid, #actor.pid, Actors),
    {noreply, State#state{actors=NewActors}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_free_location(Actors) ->
    RandomLocation = #location{x=random:uniform(?MAX_X), y=random:uniform(?MAX_Y)},
    case lists:keysearch(RandomLocation, #actor.location, Actors) of
	false ->
	    RandomLocation;
	_Actor ->
	    get_free_location(Actors)
    end.




