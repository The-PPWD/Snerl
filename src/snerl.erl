-module(snerl).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sup:start_link().

stop(_State) ->
    ok.
