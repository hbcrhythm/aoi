-module(aoi_util).
-author('labihbc@gmail.com').
-export([ceil/1, floor/1]).

%% @spec ceil(I) -> int()
%% @doc  向上取整
ceil(I) ->
    I2 = trunc(I),
    case I =:= I2 of
        true    -> I;
        false   -> I2 + 1
    end.

%% @spec floor(I) -> int()
%% @doc  向下取整
floor(I) ->
    I2 = trunc(I),
    case I =:= I2 of
        true    -> I;
        false   -> I2 - 1
    end.