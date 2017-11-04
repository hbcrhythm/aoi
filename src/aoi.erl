-module(aoi).

-include("aoi.hrl").

%% API exports
-export([aoi/5, aoi/6, add_obj/1, add_obj/3, remove_obj/1, remove_obj/3, update_obj/2, update_obj/4]).
-export([get_ids_by_pos/2, get_ids_by_pos/3, get_ids_by_type/3, get_ids_by_type/4]).
-export([param2obj/5, param2pos/3, obj2param/1, pos2param/1]).

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
	cluster_event_stdlib:init(?AOI_EVENT_DICT),
	Callback(Aoi).

aoi_towers(X, Y) ->
	F = fun({I, J}, Acc) ->
		gb_trees:insert({I, J}, aoi_tower:aoi_tower(I, J), Acc)
	end,
	lists:foldl(F, gb_trees:empty(), [{I, J} || I <- lists:seq(0, X) , J <- lists:seq(0, Y)]).

%% @spec add_obj(Obj, Pos) -> bool()
add_obj(Obj = #aoi_obj{}) ->
	add_obj(Obj, ?DEFAULT_CALLBACK_GET, ?DEFAULT_CALLBACK_PUT).
add_obj(Obj = #aoi_obj{pos = Pos}, Aoi = #aoi{towers = Towers}, Callback) ->
	check_pos(Pos, Aoi) andalso begin 
		#aoi_pos{x = X, y = Y} = trans_pos(Pos, Aoi),
		{value, Tower} = gb_trees:lookup({X, Y}, Towers),
		case aoi_tower:add(Obj, Tower) of
			Tower2 = #aoi_tower{watchers = Watchers} ->
				Towers2 = gb_trees:insert({X, Y}, Tower2, Towers),
				Aoi2 = Aoi#aoi{towers = Towers2},
				Callback(Aoi2),
				cluster_event_stdlib:event2_trigger(?AOI_EVENT_DICT, ?AOI_EVENT_ADD_OBJECT, [{Obj, Watchers}]),
				true;
			true ->
				true
		end
	end.

remove_obj(Obj) ->
	remove_obj(Obj, ?DEFAULT_CALLBACK_GET, ?DEFAULT_CALLBACK_PUT).
remove_obj(Obj = #aoi_obj{pos = Pos}, Aoi = #aoi{towers = Towers}, Callback) ->
	check_pos(Pos, Aoi) andalso begin
		#aoi_pos{x = X, y = Y} = trans_pos(Pos, Aoi),
		{value, Tower} = gb_trees:lookup({X, Y}, Towers),
		case aoi_tower:remove(Obj, Tower) of
			Tower2 = #aoi_tower{watchers = Watchers} ->				
				Towers2 = gb_trees:insert({X, Y}, Tower2, Towers),
				Aoi2 = Aoi#aoi{towers = Towers2},
				Callback(Aoi2),
				cluster_event_stdlib:event2_trigger(?AOI_EVENT_DICT, ?AOI_EVENT_REMOVE_OBJECT, [{Obj, Watchers}]),
				true;
			true ->
				true
		end
	end.

update_obj(Obj, NewPos) ->
	update_obj(Obj, NewPos, ?DEFAULT_CALLBACK_GET, ?DEFAULT_CALLBACK_PUT).
update_obj(Obj = #aoi_obj{pos = OldPos}, NewPos, Aoi = #aoi{towers = Towers}, Callback) ->
	check_pos(OldPos, Aoi) andalso check_pos(NewPos, Aoi) andalso
	begin
		#aoi_pos{x = OldX, y = OldY} = trans_pos(OldPos, Aoi),
		#aoi_pos{x = NewX, y = NewY} = trans_pos(NewPos, Aoi),
		case OldX =:= NewX andalso OldY =:= NewY of
			false ->
				NewObj = Obj#aoi_obj{pos = NewPos},
				{value, OldTower = #aoi_tower{watchers = OldWatchers}} = gb_trees:lookup({OldX, OldY}, Towers),
				{value, NewTower = #aoi_tower{watchers = NewWatchers}} = gb_trees:lookup({NewX, NewY}, Towers),
				OldTower2 = aoi_tower:remove(Obj, OldTower),
				NewTower2 = aoi_tower:add(NewObj, NewTower),
				Towers2 = gb_trees:insert({OldX, OldY}, OldTower2, Towers),
				Towers3 = gb_trees:insert({NewX, NewY}, NewTower2, Towers2),
				Aoi2 = Aoi#aoi{towers = Towers3},
				Callback(Aoi2),
				cluster_event_stdlib:event2_trigger(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_OBJECT, [{Obj, NewObj, OldWatchers, NewWatchers}]),
				true;
			true ->
				{value, #aoi_tower{watchers = OldWatchers}} = gb_trees:lookup({OldX, OldY}, Towers),
				cluster_event_stdlib:event2_trigger(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_OBJECT, [{Obj, OldWatchers}]),
				true				
		end
	end.

add_watcher(Watcher, Pos, Range) ->
	

param2obj(Id, Type, X, Y, Dir) ->
	#aoi_obj{id = Id, type = Type, pos = param2pos(X, Y, Dir)}.
param2pos(X, Y, Dir) ->
	#aoi_pos{x = X, y = Y, dir = Dir}.

obj2param(#aoi_obj{id = Id, type = Type, pos = Pos}) ->
	{X, Y, Dir} = pos2param(Pos),
	{Id, Type, X, Y, Dir}.
pos2param(#aoi_pos{x = X, y = Y, dir = Dir}) ->
	{X, Y, Dir}.

%% @spec get_ids_by_pos(Pos, Range) -> list()
%% @doc get all object ids from tower aoi by pos and range
get_ids_by_pos(Pos, Range) ->
	get_ids_by_pos(Pos, Range, ?DEFAULT_CALLBACK_GET).
get_ids_by_pos(Pos, Range, Aoi = #aoi{max_x = MaxX, max_y = MaxY, towers = Towers}) ->
	check_pos(Pos, Aoi) andalso Range > 0 andalso begin 
		P = trans_pos(Pos, Aoi),
		{{StartX, StartY}, {EndX, EndY}} = get_pos_limit(P, Range, {MaxX, MaxY}),
		F = fun({X, Y} , Acc) ->
			{value, Tower} = gb_trees:lookup({X, Y}, Towers),
			Ids = aoi_tower:get_ids(Tower),
			Ids ++ Acc
		end,
		lists:foldl(F, [], [{X,Y} || X <- lists:seq(StartX, EndX), Y <- lists:seq(StartY, EndY)])
	end.

get_ids_by_type(Pos, Range, Types) ->
	get_ids_by_type(Pos, Range, Types, ?DEFAULT_CALLBACK_GET).
get_ids_by_type(Pos, Range, Types, Aoi = #aoi{range_limit = RangeLimit, max_x = MaxX, max_y = MaxY, towers = Towers}) ->
	check_pos(Pos, Aoi) andalso Range > 0 andalso Range =< RangeLimit andalso begin
		P = trans_pos(Pos, Aoi),
		{{StartX, StartY}, {EndX, EndY}} = get_pos_limit(P, Range, {MaxX, MaxY}),
		F = fun({X, Y} , Acc) ->
			{value, Tower} = gb_trees:lookup({X, Y}, Towers),
			Result = aoi_tower:get_ids_by_types(Types, Tower),
			add_map_by_types(Result, Acc)
		end,
		lists:foldl(F, [], [{X,Y} || X <- lists:seq(StartX, EndX), Y <- lists:seq(StartY, EndY)])
	end.

add_map_by_types(TypeMap, Result) ->
	F = fun({Type, TypeList}, Acc) ->
		case lists:keyfind(Type, 1, Acc) of
			false ->
				[{Type, TypeList} | Acc];
			{Type, AccList} ->
				AccList2 = TypeList ++ AccList,
				lists:keyreplace(Type, 1, Acc, {Type, AccList2})
		end
	end,
	lists:foldl(F, Result, TypeMap).


check_pos(#aoi_pos{x = X, y = Y}, _) when X =:= undefined orelse Y =:= undefined -> false;
check_pos(#aoi_pos{x = X, y = Y}, #aoi{width = Width, height = Height}) when X < 0 orelse Y < 0 orelse X > Width orelse Y > Height -> false;
check_pos(_, _) -> true.

trans_pos(#aoi_pos{x = X, y = Y}, #aoi{tower_width = TowerWidth, tower_height = TowerHeight}) ->
	#aoi_pos{x = trunc(X / TowerWidth), y = trunc(Y / TowerHeight)}.	

get_pos_limit(#aoi_pos{x = X, y = Y}, Range, {MaxX, MaxY}) ->
	{StartX, EndX} = if
		X - Range < 0 ->
			{0, X + Range};
		X + Range > MaxX ->
			{X - Range, MaxX};
		true ->
			{X - Range, X + Range}
	end,

	{StartY, EndY} = if
		Y - Range < 0 ->
			{0, Y + Range};
		Y + Range > MaxY ->
			{Y - Range, MaxY};
		true ->
			{Y - Range, Y + Range}
	end,
	{{StartX, StartY}, {EndX, EndY}}.


%%====================================================================
%% Internal functions
%%====================================================================
