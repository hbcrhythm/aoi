-module(aoi).

-include("aoi.hrl").

%% API exports
-export([aoi/5, aoi/6, add_obj/1, add_obj/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc aoi Api function, default process dict save
aoi(Width, Height, TowerWidth, TowerHeight, Range) ->
	aoi(Width, Height, TowerWidth, TowerHeight, Range, ?DEFAULT_CALLBACK_GET).
aoi(Width, Height, TowerWidth, TowerHeight, Range, Callback) ->
	Aoi = #aoi{
		width = Width
		,height = Height
		,tower_width = TowerWidth
		,tower_height = TowerHeight
		,range_limit = Range
		,max_x = trunc(Width / TowerWidth) - 1
		,max_y = trunc(Height / TowerHeight) - 1
		% ,towers = [aoi_tower:aoi_tower(X, Y) || X <- trunc(Width / TowerWidth), Y <- trunc(Height / TowerHeight) ]
		,towers = aoi_towers(trunc(Width / TowerWidth), trunc(Height / TowerHeight))
	},
	Callback(Aoi).

aoi_towers(X, Y) ->
	F = fun({I, J}, Acc) ->
		gb_trees:insert({I, J}, aoi_tower:aoi_tower(I, J), Acc)
	end,
	lists:foldl(F, gb_trees:empty(), [{I, J} || I <- lists:seq(0, X) , J <- lists:seq(0, Y)]).

%% @spec add_obj(Obj, Pos) -> bool()
add_obj(Obj) ->
	add_obj(Obj, ?DEFAULT_CALLBACK_GET, ?DEFAULT_CALLBACK_PUT).
add_obj(Obj = #aoi_obj{id = Id, type = Type, pos = Pos}, Aoi = #aoi{towers = Towers}, Callback) ->
	check_pos(Pos, Aoi) andalso begin 
		#aoi_pos{x = X, y = Y} = trans_pos(Pos, Aoi),
		{value, Tower} = gb_trees:lookup({X, Y}, Towers),
		case aoi_tower:add(Obj, Tower) of
			Tower2 = #aoi_tower{watchers = Watchers} ->
				cluster_event_stdlib:event2_trigger(aoi, add_obj, [{Id, Type, Watchers}]),
				Towers2 = gb_trees:insert({X, Y}, Tower2, Towers),
				Aoi2 = Aoi#aoi{towers = Towers2},
				Callback(Aoi2),
				true;
			false ->
				false
		end
	end.


check_pos(#aoi_pos{x = X, y = Y}, _) when X =:= undefined orelse Y =:= undefined -> false;
check_pos(#aoi_pos{x = X, y = Y}, #aoi{width = Width, height = Height}) when X < 0 orelse Y < 0 orelse X > Width orelse Y > Height -> false;
check_pos(_, _) -> true.

trans_pos(#aoi_pos{x = X, y = Y}, #aoi{tower_width = TowerWidth, tower_height = TowerHeight}) ->
	#aoi_pos{x = trunc(X / TowerWidth), y = trunc(Y / TowerHeight)}.	

%%====================================================================
%% Internal functions
%%====================================================================
