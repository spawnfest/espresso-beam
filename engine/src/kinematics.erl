%%%-------------------------------------------------------------------
%%% @author Loris Fichera <loris.fichera@gmail.com> 
%%% @author Mirko Bonadei <mirko.bonadei@gmail.com>
%%% @copyright (C) 2012, Loris Fichera, Mirko Bonadei
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2012 by Loris Fichera <loris.fichera@gmail.com>
%%%-------------------------------------------------------------------
-module(kinematics).

-include("../include/espresso_beam.hrl").

%% API
-export([wander/3,
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
%% @spec wander(ActorKinematics, Nearby) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
wander(Kinematics, CurPosition, Nearby) ->
    O = random:uniform(360),
    NewOrientation = normalize_orientation(O),
    NewPos = orientation2position(CurPosition, NewOrientation),
    
    case lists:any(fun({X,Y}) -> {X,Y} == NewPos end, Nearby) of
	true -> {NewPos, Kinematics};
	_ -> wander(nil, CurPosition, Nearby)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Implements the seek behaviour.
%%
%% @spec seek(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
seek(Kinematics, Target) ->
    %% !FIXME to be implemented
    PrevOrientation = Kinematics#actor_kin.orientation,
    {nil, Kinematics}.


%%--------------------------------------------------------------------
%% @doc
%% Implements the flee behaviour.
%%
%% @spec flee(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
flee(Kinematics, Target) ->
    %% !FIXME to be implemented
    PrevOrientation = Kinematics#actor_kin.orientation,
    {nil, Kinematics}.


%%--------------------------------------------------------------------
%% @doc
%% Implements the pursue behaviour.
%%
%% @spec pursue(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
pursue(Kinematics, Target) ->
    ok.


%%%===================================================================
%%% Ancillary functions
%%%===================================================================
%% 337.5 e 22.5 -> 0
%% 22.5 e 67.5 -> 45
%% 67.5 112.5 -> 90
%% 112.5 157.5 -> 135
%% 157.5 202.5 -> 180
%% 202.5 247.5 -> 225
%% 247.5 292.5 -> 270
%% 292.5 337.5 -> 315
normalize_orientation(Orient) when (Orient >= 337.5) and (Orient < 22.5) -> 0.0;
normalize_orientation(Orient) when (Orient >= 22.5) and (Orient < 67.5) -> 45.0;
normalize_orientation(Orient) when (Orient >= 67.5) and (Orient < 112.5) -> 90.0;
normalize_orientation(Orient) when (Orient >= 112.5) and (Orient < 157.5) -> 135.0;
normalize_orientation(Orient) when (Orient >= 157.5) and (Orient < 202.5) -> 180.0;
normalize_orientation(Orient) when (Orient >= 202.5) and (Orient < 247.5) -> 225.0;
normalize_orientation(Orient) when (Orient >= 247.5) and (Orient < 292.5) -> 270.0;
normalize_orientation(Orient) when (Orient >= 292.5) and (Orient < 337.5) -> 315.0.

%% orientation2position
orientation2position({X, Y}, Orient) when Orient == 0 -> {X + 1, Y};
orientation2position({X, Y}, Orient) when Orient == 45 -> {X + 1, Y + 1};
orientation2position({X, Y}, Orient) when Orient == 90 -> {X, Y + 1};
orientation2position({X, Y}, Orient) when Orient == 135 -> {X - 1, Y + 1};
orientation2position({X, Y}, Orient) when Orient == 180 -> {X - 1, Y};
orientation2position({X, Y}, Orient) when Orient == 225 -> {X - 1, Y - 1};
orientation2position({X, Y}, Orient) when Orient == 270 -> {X , Y - 1};
orientation2position({X, Y}, Orient) when Orient == 315 -> {X + 1, Y - 1}.
