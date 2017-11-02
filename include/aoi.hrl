
-define(DEFAULT_CALLBACK_GET, erlang:get('@aoi') ).
-define(DEFAULT_CALLBACK_PUT, fun(Aoi) -> erlang:put('@aoi', Aoi) end).

-record(aoi,{
		width
		,height
		,tower_width
		,tower_height
		,range_limit
		,max_x
		,max_y
		,towers
	}).

-record(aoi_tower, {
		x
		,y
		,ids = []
		,watchers = []
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
	}).

-define(AOI_OBJ_MODE_WATCHER, 1).
-define(AOI_OBJ_MODE_MARKER, 2).
-define(AOI_OBJ_MODE_MOVE, 4).
-define(AOI_OBJ_MODE_DROP, 8).