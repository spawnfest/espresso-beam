%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2012, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(espresso_beam_ws).  

-behaviour(cowboy_http_handler).  
-behaviour(cowboy_http_websocket_handler).  
  
% Behaviour cowboy_http_handler API
-export([init/3, handle/2, terminate/2]).  
  
% Behaviour cowboy_http_websocket_handler API 
-export([websocket_init/3, websocket_handle/3,  
	 websocket_info/3, websocket_terminate/3]).

-include("espresso_beam.hrl").

%%--------------------------------------------------------------------
%% @doc
%% This function is called to know how to dispatch a new connection.
%%
%% @spec init(_Any, Req, []) -> {ok, Req2, undefined} |
%%                              {upgrade, protocol, cowboy_http_websocket}
%% @end
%%-------------------------------------------------------------------- 
init({tcp, http}, Req, _Opts) ->
    case cowboy_http_req:header('Upgrade', Req) of
	{undefined, Req2} -> {ok, Req2, undefined};
	{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
	{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.
 
handle(Req, State) ->
    Reply = <<"<http><body>Not implemented.</body></http>">>,
    Headers = [{<<"Content-Type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_http_req:reply(404, Headers,  Reply, Req), 
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function is called for every new websocket connection.
%%
%% @spec websocket_init(_Any, Req, []) -> {ok, Req2, undefined, hibernate}
%% @end
%%-------------------------------------------------------------------- 
websocket_init(_Any, Req, []) ->
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a text message arrives. 
%%
%% @spec websocket_handle(Msg, Req, State) -> {reply, Reply, Req, State, hibernate} |
%%                                            {ok, Req, State}
%% @end
%%-------------------------------------------------------------------- 
websocket_handle({text, <<"step">>}, Req, State) ->
    env_manager:step(),
    Actors = env_manager:active_actors(),
    Reply = convert_to_json(Actors),
    {reply, {text, Reply}, Req, State, hibernate};

websocket_handle({text, <<"stop">>}, Req, State) ->
    application:stop(espresso_beam),
    halt(),
    {ok, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
    %% here we handle for example binaries
    {ok, Req, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when other messages from the system arrives.
%%
%% @spec websocket_info(Msg, Req, State) -> {reply, Reply, Req, State, hibernate} |
%%                                          {ok, Req, State, hibernate}
%% @end
%%-------------------------------------------------------------------- 
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function converts into json, the list of the actors in the field.
%%
%% @spec convert_to_json(Actors) -> Json
%% @end
%%-------------------------------------------------------------------- 
convert_to_json(Actors) ->
    Data = [{obj,
	     [{type, Actor#actor.type}, 
	      {location, tuple_to_list(Actor#actor.location)}]}
	    || Actor <- Actors],
    JsonData = {obj, [{env_state, Data}]},
    rfc4627:encode(JsonData).

    
actors() ->
    [{actor, pid1,wolf,{26,4}},
     {actor, pid2,rabbit,{12,2}},
     {actor,pid3,carrot,{13,16}}].
