-module(aoi_tower).

-include("aoi.hrl").

-export([aoi_tower/2, add/2]).

aoi_tower(X, Y) ->
	#aoi_tower{x = X, y = Y}.

%% @doc Add an object to tower
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
			false
	end.