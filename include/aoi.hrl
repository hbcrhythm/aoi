
-define(DEFAULT_CALLBACK_GET, erlang:get('@aoi') ).
-define(DEFAULT_CALLBACK_PUT, fun(Aoi) -> erlang:put('@aoi', Aoi) end).

-define(AOI_EVENT_DICT, '@aoi_event_dict').
-define(AOI_EVENT_ADD_OBJECT, add_object).
-define(AOI_EVENT_REMOVE_OBJECT, remove_object).
-define(AOI_EVENT_UPDATE_OBJECT, update_object).

-record(aoi,{
		width
		,height
		,tower_width
		,tower_height
		,range_limit = 5
		,max_x
		,max_y
		,towers
	}).

-record(aoi_tower, {
		x
		,y
		,ids = []
		,watchers = [
			%%{type, TypeList}
		]
		,type_map = [
			%%{Type, TypeList}
		]
		,size = 0
	}).

-record(aoi_obj, {
		id
		,pos
		,type
	}).

-record(aoi_pos, {
		x
		,y
		,dir
	}).
