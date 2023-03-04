-module(server).

-behaviour(gen_server).

-include_lib("cecho/include/cecho.hrl").
-include("../include/state.hrl").

-export([start_link/0, play/0, get_state/0, send_change/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).


start_link() ->
    OkPid = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    play(),
    OkPid.


play() ->
    gen_server:call(?MODULE, play).


get_state() ->
    gen_server:call(?MODULE, get_state).


send_change(Change) ->
    gen_server:cast(?MODULE, {send_change, Change}).


init([]) ->
    process_flag(trap_exit, true),
    setup_cecho(),
    {ok, {}}.


handle_call(play, _From, {}) ->
    render:start_render_loop(),
    input:start_input_loop(),
    {reply, ok, {}};

handle_call(get_state, _From, {}) ->
    State = state:get_state(),
    {reply, State, {}};

handle_call(_Request, _From, {}) ->
    {reply, ok, {}}.


handle_cast({send_change, Change}, {}) ->
    case Change of
        shutdown    -> gen_server:stop(?MODULE);
        _           -> state:update_state(Change)
    end,
    {noreply, {}};

handle_cast(_Request, {}) ->
    {noreply, {}}.


terminate(_Reason, {}) ->
    cecho:mvaddstr(0, 0, "Shutting down..."),
    teardown_cecho(),
    ok.


setup_cecho() ->
    cecho:noecho(),
    cecho:cbreak(),
    cecho:keypad(?ceSTDSCR, true),
    cecho:curs_set(?ceCURS_INVISIBLE),
    ok.


teardown_cecho() ->
    cecho:curs_set(?ceCURS_NORMAL),
    cecho:keypad(?ceSTDSCR, false),
    cecho:nocbreak(),
    cecho:echo(),
    ok.
