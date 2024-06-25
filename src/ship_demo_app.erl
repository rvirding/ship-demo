%%%-------------------------------------------------------------------
%% @doc ship_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(ship_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ship_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
