-module(luerl_ship).

%% The basic entry point to set up the function table.
-export([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]). %Shorten these

%% This works if luerl/ebin has been added to the path
-include_lib("luerl/src/luerl.hrl").

install(St) ->
    luerl_emul:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"self">>,#erl_func{code=fun self/2}},
     {<<"set_tick">>,#erl_func{code=fun set_tick/2}},
     {<<"get_pos">>,#erl_func{code=fun get_pos/2}},
     {<<"set_pos">>,#erl_func{code=fun set_pos/2}},
     {<<"get_speed">>,#erl_func{code=fun get_speed/2}},
     {<<"set_speed">>,#erl_func{code=fun set_speed/2}},
     {<<"zap">>,#erl_func{code=fun zap/2}},
     {<<"set_ship">>,#erl_func{code=fun set_ship/2}},
     {<<"do">>,#erl_func{code=fun do/2}},
     {<<"gc">>,#erl_func{code=fun gc/2}}
    ].

self([], St) ->
    {[#userdata{d=self()}],St}.

set_tick([#userdata{d=S},Tick], St) when is_number(Tick) ->
    ship:set_tick(S, trunc(Tick)),
    {[],St}.

get_pos([#userdata{d=S}], St) ->
    {X,Y} = ship:get_pos(S),
    {[X,Y],St};
get_pos(As, St) -> badarg_error(get_pos, As, St).

set_pos([#userdata{d=S},X,Y], St) when is_number(X), is_number(Y) ->
    ship:set_pos(S, X, Y),
    {[],St};
set_pos(As, St) -> badarg_error(set_pos, As, St).

get_speed([#userdata{d=S}], St) ->
    {X,Y} = ship:get_speed(S),
    {[X,Y],St};
get_speed(As, St) -> badarg_error(get_speed, As, St).

set_speed([#userdata{d=S},X,Y], St) when is_number(X), is_number(Y) ->
    ship:set_speed(S, X, Y),
    {[],St};
set_speed(As, St) -> badarg_error(set_speed, As, St).

zap([#userdata{d=S}], St) ->
    ship:zap(S),
    {[],St};
zap(As, St) -> badarg_error(zap, As, St).

set_ship([#userdata{d=S},Name], St) ->
    ship:set_ship(S, Name),
    {[],St};
set_ship(As, St) -> badarg_error(set_ship, As, St).

do([#userdata{d=S},Cmd], St) ->
    {ok,Rs} = ship:lua_do(S, binary_to_list(Cmd)),
    {Rs,St};
do(As, St) -> badarg_error(do, As, St).

gc([#userdata{d=S}], St) ->
    ship:gc(S),
    {[],St};
gc(As, St) -> badarg_error(gc, As, St).

    
