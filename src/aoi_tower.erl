-module(aoi_tower).
-author('labihbc@gmail.com').
-include("aoi.hrl").

-export([aoi_tower/2, add/2, remove/2, get_ids/1, get_ids_by_types/2, add_watcher/2, remove_watcher/2]).

aoi_tower(X, Y) ->
	#aoi_tower{x = X, y = Y}.

add(#aoi_obj{id = Id, type = Type}, Tower = #aoi_tower{ids = Ids, type_map = TypeMap, size = Size}) ->
	case lists:member(Id, Ids) of
		false ->
			Ids2 = [Id | Ids],
			case lists:keyfind(Type, 1, TypeMap) of
				{_, TypeList} ->
					TypeMap2 = lists:keyreplace(Type, 1, TypeMap, {Type, [Id | TypeList]}),
					Size2 = Size + 1,
					Tower2 = Tower#aoi_tower{ids = Ids2, type_map = TypeMap2, size = Size2},
					Tower2;
				false ->
					TypeMap2 = [{Type, [Id]} | TypeMap],
					Size2 = Size + 1,
					Tower2 = Tower#aoi_tower{ids = Ids2, type_map = TypeMap2, size = Size2},
					Tower2
			end;
		true ->
			Tower
	end.

remove(#aoi_obj{id = Id, type = Type}, Tower = #aoi_tower{ids = Ids, type_map = TypeMap, size = Size}) ->
	case lists:member(Id, Ids) of
		true ->
			Ids2 = lists:delete(Id, Ids),
			case lists:keyfind(Type, 1, TypeMap) of
				{_, TypeList} ->
					TypeList2 = lists:delete(Id, TypeList),
					TypeMap2 = lists:keyreplace(Type, 1, TypeMap, {Type, TypeList2}),
					Size2 = Size - 1,
					Tower2 = Tower#aoi_tower{ids = Ids2, type_map = TypeMap2, size = Size2},
					Tower2;
				false ->
					Tower	
			end;
		false ->
			Tower
	end.

add_watcher(#aoi_obj{id = Id, type = Type}, Tower = #aoi_tower{watchers = Watchers}) ->
	case lists:keyfind(Type, 1, Watchers) of
		false ->
			Watchers2 = [{Type, [Id]} | Watchers],
			Tower2 = Tower#aoi_tower{watchers = Watchers2},
			Tower2;
		{Type, TypeList} ->
			case lists:member(Id, TypeList) of
				true ->
					Tower;
				false ->
					TypeList2 = [Id | TypeList],
					Watchers2 = lists:keyreplace(Type, 1, Watchers, {Type, TypeList2}),
					Tower2 = Tower#aoi_tower{watchers = Watchers2},
					Tower2
			end
	end.

remove_watcher(#aoi_obj{id = Id, type = Type}, Tower = #aoi_tower{watchers = Watchers}) ->
	case lists:keyfind(Type, 1, Watchers) of
		false ->
			Tower;
		{Type, TypeList} ->
			TypeList2 = lists:delete(Id, TypeList),
			Watchers2 = lists:keyreplace(Type, 1, Watchers, {Type, TypeList2}),
			Tower2 = Tower#aoi_tower{watchers = Watchers2},
			Tower2
	end.


get_ids(#aoi_tower{ids = Ids}) ->
	Ids.

get_ids_by_types(Types, #aoi_tower{type_map = TypeMap}) ->
	F = fun(Type, Acc) ->
		case lists:keyfind(Type, 1, TypeMap) of
			Term = {_, _} ->
				[Term | Acc];
			false ->
				Acc
		end
	end,
	lists:foldl(F, [], Types).
