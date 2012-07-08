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
%% @spec wander(ActorKinematics, Nearby) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
wander(Kinematics, Nearby) ->
    CurPosition = Kinematics#kin.position,
    CurOrientation = Kinematics#kin.orientation,

    %% !FIXME maybe we should implement a limit here, based on
    %% previous orientation?
    random:seed(erlang:now()),
    OrientationDelta = random:uniform(360),
    NewOrientation = normalize_orientation(CurOrientation + OrientationDelta),
    NewPos = orientation2position(CurPosition, NewOrientation),
    
    NewKinematics =
	%% if the position is valid
	case lists:any(fun({X,Y}) -> {X,Y} == NewPos end, Nearby) of
	    true -> 
		io:format("moving ~p ~p~n", [NewPos, CurPosition]),
		#kin{ position = NewPos,
		      orientation = NewOrientation };
	    
	    _ -> %% stay still
		%% !FIXME canvas borders are going to be a TRAP!
		io:format("staying still~n", []),
		Kinematics
	end.


%%--------------------------------------------------------------------
%% @doc
%% Implements the seek behaviour.
%%
%% @spec seek(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
seek(Kinematics, Target) ->
    %% compute the new orientation, according to the move we are going
    %% to perform
    TargetPosition = Target#actor.location,
    
    CurPosition = Kinematics#kin.position,
    NewOrientation = position2orientation(CurPosition, TargetPosition),
    
    Kinematics#kin{ orientation = NewOrientation,
		    position = TargetPosition }.


%%--------------------------------------------------------------------
%% @doc
%% Implements the flee behaviour.
%%
%% @spec flee(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
flee(Kinematics, Target) ->
    %% run away from target!
    CurPosition = Kinematics#kin.position,
    PrevOrientation = Kinematics#kin.orientation,
    TargetPosition = Target#actor.location,
    
    %% if possible, keep running in the same direction
    NextPos = orientation2position(CurPosition, PrevOrientation),
    
    NewKinematics = 
	if NextPos == TargetPosition ->
		%% try some other position
		random:seed(erlang:now()),
		O = random:uniform(360),
		NewOrientation = normalize_orientation(O),
		NewPos = orientation2position(CurPosition, NewOrientation),
		
		#kin { position = NewPos,
		       orientation = NewOrientation };
	   
	   true ->
		Kinematics#kin { position = NextPos }
	end.


%%--------------------------------------------------------------------
%% @doc
%% Implements the pursue behaviour.
%%
%% @spec pursue(ActorKinematics, Target) -> NewPosition.
%% @end
%%--------------------------------------------------------------------
pursue(Kinematics, Target) ->
    %% !FIXME to be extended when sensing_distance > 1
    CurPosition = Kinematics#kin.position,
    TargetPosition = Target#actor.location,
    NewOrientation = pursue_orientation(CurPosition, TargetPosition),
    NewPos = orientation2position(CurPosition, NewOrientation),
    
    #kin{ position = NewPos,
	  orientation = NewOrientation }.


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
normalize_orientation(Orient) when (Orient >= 337.5) or (Orient < 22.5) -> 0.0;
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

%% position2orientation
position2orientation({X, Y}, {A, Z}) when (A == X) and (Z == Y) -> 0.0;
position2orientation({X, Y}, {A, Z}) when (A == X + 1) and (Z == Y) -> 0.0;
position2orientation({X, Y}, {A, Z}) when (A == X + 1) and (Z == Y + 1) -> 45.0;
position2orientation({X, Y}, {A, Z}) when (A == X) and (Z == Y + 1) -> 90.0;
position2orientation({X, Y}, {A, Z}) when (A == X - 1) and (Z == Y + 1) -> 135.0;
position2orientation({X, Y}, {A, Z}) when (A == X - 1) and (Z == Y) -> 180.0;
position2orientation({X, Y}, {A, Z}) when (A == X - 1) and (Z == Y - 1) -> 225.0;
position2orientation({X, Y}, {A, Z}) when (A == X) and (Z == Y - 1) -> 270.0;
position2orientation({X, Y}, {A, Z}) when (A == X + 1) and (Z == Y - 1) -> 315.0.


%% degrees2radians
degrees2radians(Deg) -> Deg * math:pi() / 180.

%% radians2degrees
radians2degrees(Rad) -> Rad * 180 / math:pi().

%% pursue_orientation
pursue_orientation({X, Y}, {A, Z}) ->
    C = math:sqrt(math:pow(X - A, 2) + math:pow(Y - Z, 2)),
    D = A - X,
    
    case D of 0 ->
	    0;
	_ ->
	    Alpha = radians2degrees(math:asin(D / C)),
	    180 - Alpha
    end.
	

