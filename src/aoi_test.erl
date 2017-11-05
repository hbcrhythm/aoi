-module(aoi_test).
-author('labihbc@gmail.com').
-include("aoi.hrl").

-export([start/0, test/0, get_ids_by_pos/2, get_ids_by_type/3]).
-export([add_obj_callback/1, remove_obj_callback/1, update_obj_callback/1, update_watcher_callback/1]).

start() ->
	application:ensure_all_started(aoi).

test() ->
	aoi:aoi(1000, 1000, 100, 100, 3),
	Types = [player, monster, npc],
	Count = 6,
	random:seed(),
	F = fun(Id) ->
		Random = random:uniform(9),
		Type = random:uniform(3),
		Ranom2 = random:uniform(100),
		AoiObj = #aoi_obj{id = Id, pos = #aoi_pos{x = Random * Ranom2, y = Random * Ranom2}, type = lists:nth(Type, Types)},
		io:format("~w~n",[AoiObj]),
		aoi:add_obj(AoiObj)
	end,
	[F(Id) || Id <- lists:seq(1, Count)],
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_ADD_OBJECT, {aoi_test, add_obj_callback, []}),
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_REMOVE_OBJECT, {aoi_test, remove_obj_callback, []}),
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_OBJECT, {aoi_test, update_obj_callback, []}),
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_WATCHER, {aoi_test, update_watcher_callback, []}).

get_ids_by_pos(X, Y) ->
	aoi:get_ids_by_pos(#aoi_pos{x = X, y = Y}, 3).

get_ids_by_type(X, Y, Type) ->
	aoi:get_ids_by_type(#aoi_pos{x = X, y = Y}, 3, [Type]).

add_obj_callback({Obj, Watcher}) ->
	io:format("add_obj_callback obj: ~w watcher: ~w ~n",[Obj, Watcher]).

remove_obj_callback({Obj, Watcher}) ->
	io:format("remove_obj_callback obj: ~w watcher: ~w ~n",[Obj, Watcher]).

update_obj_callback({Obj, NewObj, OldWatchers, NewWatchers}) ->
	io:format("update obj callback obj: ~w newObj: ~w OldWatcher: ~w NewWatchers: ~w ~n",[Obj, NewObj, OldWatchers, NewWatchers]);

update_obj_callback({Obj, OldWatchers}) ->
	io:format("update obj callback obj: ~w OldWatcher: ~w ~n",[Obj, OldWatchers]).

update_watcher_callback({Watcher, AddIds, DelIds}) ->
	io:format("udpate watcher callback ~w ~w ~w",[Watcher, AddIds, DelIds]).