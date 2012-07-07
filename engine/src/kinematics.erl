%%%-------------------------------------------------------------------
%%% @author Loris Fichera <loris.fichera@gmail.com> 
%%% @author Mirko Bonadei <mirko.bonadei@gmail.com>
%%% @copyright (C) 2012, Loris Fichera, Mirko Bonadei
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Loris Fichera <loris.fichera@gmail.com
%%%-------------------------------------------------------------------
-module(kinematics).

%% API
-export([wander/2,
	 seek/2,
	 flee/2,
	 pursue/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Implements the wander behaviour.
%%
%% @spec wander(ActorKineamtics, Nearby) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
wander(ActorKinematics, Nearby) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Implements the seek behaviour.
%%
%% @spec seek(ActorKineamtics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
seek(ActorKinematics, Target) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Implements the flee behaviour.
%%
%% @spec flee(ActorKineamtics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
flee(ActorKinematics, Target) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Implements the pursue behaviour.
%%
%% @spec pursue(ActorKineamtics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
pursue(ActorKinematics, Target) ->
    ok.

