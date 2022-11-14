-module(snerl).

-export([main/1]).

-include_lib("cecho/include/cecho.hrl").

%% exported
%% main/1
main(_Args) ->
    cecho_setup(),
    game_loop(),
    cecho_teardown(),
    erlang:halt(0).


%% internal
%% game_loop/0
game_loop() ->
    cecho:mvaddstr(0, 0, "Hello, World!"),
    cecho:refresh(),
    cecho:getch().


%% cecho_setup/0
cecho_setup() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho().


%% cecho_teardown/0
cecho_teardown() ->
    cecho:nocbreak(),
    cecho:echo(),
    application:stop(cecho).

