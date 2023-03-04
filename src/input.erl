-module(input).

-behaviour(gen_server).

-include_lib("cecho/include/cecho.hrl").

-export([start_link/0, start_input_loop/0]).

-export([init/1, handle_call/3, handle_cast/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


start_input_loop() ->
    gen_server:cast(?MODULE, input_loop).


init([]) ->
    {ok, {}}.


handle_call(_Request, _From, {}) ->
    {reply, ok, {}}.


handle_cast(input_loop, {}) ->
    input_tick(),
    {noreply, {}};

handle_cast(_Request, {}) ->
    {noreply, {}}.


input_tick() ->
    Char = cecho:getch(),
    Change = character_to_change(Char),
    server:send_change(Change),
    gen_server:cast(?MODULE, input_loop),
    ok.


character_to_change(Char) when Char =:= $q; Char =:= ?ceKEY_ESC -> shutdown;

character_to_change(Char) when Char =:= $w; Char =:= ?ceKEY_UP -> up;

character_to_change(Char) when Char =:= $s; Char =:= ?ceKEY_DOWN -> down;

character_to_change(Char) when Char =:= $a; Char =:= ?ceKEY_LEFT -> left;

character_to_change(Char) when Char =:= $d; Char =:= ?ceKEY_RIGHT -> right;

character_to_change(_Char) -> other.