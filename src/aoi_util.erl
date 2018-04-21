-module(aoi_util).
-author('labihbc@gmail.com').
-export([ceil/1, floor/1]).

%% @spec ceil(I) -> int()
%% @doc  向上取整
ceil(X) ->
    T = trunc(X),
    if  X == T -> T;
        true ->
            if  X > 0 -> T + 1;
                true  -> T
            end         
    end.
%% @spec floor(I) -> int()
%% @doc  向下取整
floor(X) ->
     T = trunc(X),
     if X == T -> T;
        true ->
            if X > 0 -> T;
               true  ->  T - 1
            end
     end.
