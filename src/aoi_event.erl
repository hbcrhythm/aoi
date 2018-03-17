-module(aoi_event).
-author('labihbc@gmail.com').
-include("aoi.hrl").

-export([add_obj/3, remove_obj/3, update_obj/3, update_watcher/3]).

add_obj(M, F, A) ->
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_ADD_OBJECT, {M, F, A}).

remove_obj(M, F, A) ->
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_REMOVE_OBJECT, {M, F, A}).

update_obj(M, F, A) ->
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_OBJECT, {M, F, A}).

update_watcher(M, F, A) ->
	cluster_event_stdlib:event2_add(?AOI_EVENT_DICT, ?AOI_EVENT_UPDATE_WATCHER, {M, F, A}).