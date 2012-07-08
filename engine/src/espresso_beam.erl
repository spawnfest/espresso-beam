%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paol.dincauo@gmail.com>
%%% @copyright (C) 2012, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(espresso_beam).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),

    case espresso_beam_sup:start_link() of
	{ok, Pid} ->
	    %% add websocket support
	    Dispatch = [{'_', [{[<<"websocket">>], espresso_beam_ws, []}]}],
	    cowboy:start_listener(my_http_listener, 100,
	      			  cowboy_tcp_transport, [{port, 8080}],
				  cowboy_http_protocol, [{dispatch, Dispatch}]
	     			 ),
	    io:format("* Websockets interface at: http://localhost:8080/websocket~n", []),
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
