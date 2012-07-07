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
    Page = get_page(),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}], Page, Req),  
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
    io:format("~p~n", [Msg]),
    Reply = <<"will make all actors to take a step">>,
    {reply, {text, Reply}, Req, State, hibernate};

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

get_page() ->
<<"<!DOCTYPE HTML>
<html>
<head>  
<title>Espresso Beam</title>  
<script language=\"javascript\" type=\"text/javascript\">
var wsUri = \"ws://localhost:8080/websocket\";
var output;  

function init() { 
    output = document.getElementById(\"output\"); 
    testWebSocket(); 
    writeToScreen(\"INITIALIZED\"); 
}  

function testWebSocket() { 
    websocket = new WebSocket(wsUri); 
    websocket.onopen = function(evt) { onOpen(evt) }; 
    websocket.onclose = function(evt) { onClose(evt) }; 
    websocket.onmessage = function(evt) { onMessage(evt) }; 
    websocket.onerror = function(evt) { onError(evt) }; 
}  

function onOpen(evt) { 
    writeToScreen(\"CONNECTED\"); 
    doSend(\"step\"); 
}  

function onClose(evt) {     writeToScreen(evt); writeToScreen(\"DISCONNECTED\"); }  

function onMessage(evt) { 
    writeToScreen('<span style=\"color: blue;\">RESPONSE: ' + evt.data+'</span>'); 
    //websocket.close(); 
}  

function onError(evt) { 
    writeToScreen('<span style=\"color: red;\">ERROR:</span> ' + evt.data); 
}  

function doSend(message) { 
    writeToScreen(\"SENT: \" + message);  
    websocket.send(message); 
}  

function writeToScreen(message) { 
    var pre = document.createElement(\"p\"); 
    pre.style.wordWrap = \"break-word\"; 
    pre.innerHTML = message; 
    output.appendChild(pre); 
}  

window.addEventListener(\"load\", init, false);  
</script>  
</head>
<body>
    <h2>Espresso Beam</h2>  
    <div id=\"output\"></div>
</body>
</html>">>.
