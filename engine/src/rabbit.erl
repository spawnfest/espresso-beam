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
-module(rabbit).

-behaviour(gen_fsm).

%% API
-export([start_link/0, 
	 wander/2,
	 wander/3,
	 eat/3,
	 flee/2,
	 flee/3,
	 move/2,
	 act/2
	]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, { kinematics=nil,
		 health=nil
	       }).

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

eat(RabbitPid, EaterPid) ->
    gen_fsm:send_all_state_event(RabbitPid, {eaten, EaterPid}).

move(Pid, Nearby) ->
    gen_fsm:send_event(Pid, {move, Nearby}).

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
    pg:join(rabbits, self()),
    io:format("Spawned new rabbit: ~p ~n", [self()]),
    Pos = env_manager:allocate_me(self(), rabbit),


    Kin = #kin{ position = Pos },

    {ok, wander, #state{ kinematics=Kin,
		       health=10 }}.

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
wander({move, Nearby}, State) ->
    Kin = State#state.kinematics,
    
    {Carrots, Wolves} = sense(Nearby), %%!FIXME sense to be implemented
    
    {NewState, NextState} = 
	if length(Wolves) > 0 ->
		%% if there are wolves nearby...
		%% run away from the first wolf of the list!
		[W|_] = Wolves,
		NewKin = kinematics:flee(Kin, W),
		
		%% next state is flee
		{State#state { kinematics = NewKin }, flee};
	   
	   length(Carrots) > 0 ->
		%% no wolves, just carrots around...
		%% approach the first carrot of the list and eat it!
		[C|_] = Carrots,
		NewKin = kinematics:seek(Kin, C),
		
		%% next state is eat!
		{State#state { kinematics = NewKin }, eat};
	   
	   true ->
		%% if there's nothing around...	
		%% keep wandering!
		NewKin = kinematics:wander(Kin, Nearby),
		
		%% next state is wander!
		{State#state { kinematics = NewKin }, wander}
	   end,
    
    %% update the env_manager on my new position
    env_manager:update_me(self(), NewKin#kin.position),
    {next_state, NextState, NewState}.


wander({act, _}, _From, State) ->
    %% was wandering, nobody around, nothing to do
    %% keep wandering
    Health = State#state.health,
    UpdatedHealth = Health - 1,
    NewState = State#state { health = UpdatedHealth },
    {reply, ok, wander, NewState}.


eat({act, CellContent}, _From, State) ->
    Health = State#state.health,
    
    %% is there a carrot out there?
    io:format("Other Actors in my cell: ~p ~n", [CellContent]),
    
    %% try to eat a carrot and update the health
    Carrots = lists:filter(fun({_, T}) -> T == carrot end,
			   CellContent),
   
    UpdatedHealth = 
	if length(Carrots) =/= 0 ->
		%% eat it
		[{C, _}|_] = Carrots,
		carrot:eat(C, self()),
		Health + 2;
	   
	   true ->
		%% else, the carrot has been eaten by someone else
		Health - 1
	end,

    NewState = State#state { health = UpdatedHealth },

    if UpdatedHealth == 0 ->
	    {stop, normal, deallocate_me, NewState}; %% die 
       
       UpdatedHealth > 25 ->	    
	    %% spawn a new rabbit %% !FIXME to be implemented!
	    {reply, ok, wander, NewState};

       true ->
	    %% go back to wander
	    {reply, ok, wander, NewState}
    end.


flee({move, Nearby}, State) ->
    Kin = State#state.kinematics,
    
    %% running away from wolves, don't care about carrots
    {_, Wolves} = sense(Nearby), %%!FIXME sense to be implemented
    
    {NewState, NextState} = 
	if length(Wolves) > 0 ->
		%% if there are wolves nearby...
		%% keep fleeing away!
		[W|_] = Wolves,
		NewKin = kinematics:flee(Kin, W),
		
		%% next state is flee
		{State#state { kinematics = NewKin }, flee};
	   	   
	   true ->
		%% if there's nothing around...	
		%% go wandering!
		NewKin = kinematics:wander(Kin, Nearby),
		
		%% next state is wander!
		{State#state { kinematics = NewKin }, wander}
	   end,
    
    %% update the env_manager on my new position
    env_manager:update_me(self(), NewKin#kin.position),
    {next_state, NextState, NewState}.    
    

flee({act, _}, _From, State) ->
    %% fleeing, nothing to do, just decrement the health
    Health = State#state.health,
    UpdatedHealth = Health - 1,
    NewState = State#state { health = UpdatedHealth },
    {reply, ok, wander, NewState}. 


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
handle_event({eaten, Pid}, StateName, State) ->
    io:format("~nRabbit ~p has been eaten by ~p. While it was 
        in state ~p, and its internal state was ~p~n", 
	      [self(), Pid, StateName, State]
	     ),
    env_manager:deallocate_me(self()),
    {stop, normal, State}.

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
handle_info(_Info, StateName, State) ->
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
%% sense the nearby cells for carrots and wolves
sense(Nearby) ->
    Wolves = 
	lists:filter(fun({{X,Y}, Content}) ->
			     lists:any(fun(What) ->
					       case What of {_, wolf} ->
						       true;
						   _ -> false
					       end
				       end,
				       Content)
		     end,
		     Nearby),
    
    Carrots = 
	lists:filter(fun({{X,Y}, Content}) ->
			     lists:any(fun(What) ->
					       case What of {_, carrot} ->
						       true;
						   _ -> false
					       end
				       end,
				       Content)
		     end,
		     Nearby),        
    
    {Carrots, Wolves}.

