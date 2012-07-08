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
-module(wolf).

-behaviour(gen_fsm).

%% API
-export([start_link/0, 
	 move/3,
	 act/2]).

%% gen_fsm callbacks
-export([init/1, 
	 wander/2,
	 wander/3,
	 handle_event/3, handle_sync_event/4, 
         handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, { kinematics=nil, health=nil, target=nil }).

-include("../include/espresso_beam.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

move(Pid, Nearby, NearbyLocations) ->
    gen_fsm:send_event(Pid, {move, Nearby, NearbyLocations}).

act(Pid, CellStatus) ->
    gen_fsm:sync_send_event(Pid, {act, CellStatus}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    pg:join(wolves, self()),
    io:format("Spawned new wolf: ~p ~n", [self()]),
    Pos = env_manager:allocate_me(self(), wolf),


    Kin = #kin{ position = Pos },
    {ok, wander, #state{ kinematics=Kin, health=10 }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
wander({move, Nearby, NearbyLocations}, State) ->
    %% %% get the kinematics and the current position
    Kin = State#state.kinematics,

    Rabbits = sense(Nearby),
    
    {NewState, NextState} = 
	if length(Rabbits) > 0 ->
		%% if there are rabbits nearby...
		%% chase them!
		%% !FIXME implement WOLF PACK HERE!
		[R|_] = Rabbits,
		NewKin = kinematics:pursue(Kin, R),
		
		%% next state is pursue
		{State#state { kinematics = NewKin, target = R }, pursue};
	   
	   true ->
		%% if there's nothing around...	
		%% keep wandering!
		NewKin = kinematics:wander(Kin, NearbyLocations),
		
		%% next state is wander!
		{State#state { kinematics = NewKin }, wander}
	   end,

    env_manager:update_me(self(), NewKin#kin.position),
    {next_state, NextState, NewState}.


wander({act, _}, _From, State) ->
    %% was wandering, nobody around, nothing to do
    %% keep wandering
    Health = State#state.health,
    UpdatedHealth = Health - 1,
    NewState = State#state { health = UpdatedHealth },
    {reply, ok, wander, NewState}.


pursue({move, Nearby, NearbyLocations}, State) ->
    Kin = State#state.kinematics,
    CurrentTarget = State#state.target,
    TargetPid = CurrentTarget#actor.pid,
    IsAlive = is_process_alive(TargetPid),
    

    {NewState, NextState} = 
	if IsAlive ->
		%% try to eat the rabbit!
		NewKin = kinematics:pursue(Kin, CurrentTarget),
		
		%% next state is eat
		{State#state { kinematics = NewKin }, eat};
	   
	   true ->
		%% else, someone else ate the rabbit
		%% start wandering again
		NewKin = kinematics:wander(Kin, NearbyLocations),
		
		%% next state is wander!
		{State#state { kinematics = NewKin, target = nil }, wander}
	end,
    
    env_manager:update_me(self(), NewKin#kin.position),
    {next_state, NextState, NewState}.		


eat({act, CellContent}, _From, State) ->
    Health = State#state.health,
    CurrentTarget = State#state.target,
    TargetPid = CurrentTarget#actor.pid,
    IsAlive = is_process_alive(TargetPid),

    %% is there a rabbit out there?
    if IsAlive ->
	    io:format("Other Actors in my cell: ~p ~n", [CellContent]),
	    
	    %% try to eat a rabbit and update the health
	    Rabbits = lists:filter(fun(A) -> T = A#actor.type,
					     T == rabbit
				   end,
				   CellContent),
	    
	    {UpdatedHealth, NewTarget} = 
		if length(Rabbits) =/= 0 ->
			%% eat it
			[R|_] = Rabbits,
			carrot:eat(R#actor.pid, self()),
			{Health + 2, nil};
		   
		   true ->
			%% else, the rabbit is not here
			{Health - 1, CurrentTarget}
		end,
	    
	    NewState = State#state { health = UpdatedHealth, 
				     target = NewTarget};

       %% the rabbit is no more alive, go back to wander
       true ->
	    UpdatedHealth = Health - 1,
	    NewState = State#state { health = Health - 1, 
				     target = nil}
    end,
    
    if UpdatedHealth == 0 ->
	    {stop, normal, deallocate_me, NewState}; %% die 
       
       UpdatedHealth > 25 ->	    
	    %% spawn a new wolf %% !FIXME to be implemented!
	    ok
       %%{reply, ok, wander, NewState};
    end,
    
    case NewState#state.target of nil ->
	    {reply, ok, wander, NewState};
	_ ->
	    {reply, ok, pursue, NewState}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
sense(Nearby) ->
    Rabbits = 
	lists:filter(fun({{X,Y}, Content}) ->
			     lists:any(fun(What) ->
					       T = What#actor.type,
					       case T of wolf -> true;
						   _ -> false
					       end
				       end,
				       Content)
		     end,
		     Nearby).
