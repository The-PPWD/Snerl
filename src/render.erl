-module(render).

-behaviour(gen_server).

-include("../include/state.hrl").

-export([start_link/0, start_render_loop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


start_render_loop() ->
    ?MODULE ! render_loop,
    ok.


init([]) ->
    {ok, {}}.


handle_call(_Request, _From, {}) ->
    {reply, ok, {}}.


handle_cast(_Request, {}) ->
    {noreply, {}}.


handle_info(render_loop, {}) ->
    render_tick(),
    {noreply, {}};

handle_info(_Info, {}) ->
    {noreply, {}}.


render_tick() ->
    #state{speed = Speed} = State = server:get_state(),
    update_screen(State),
    erlang:send_after(Speed, ?MODULE, render_loop),
    ok.


update_screen(#state{status = lost}) ->
    cecho:erase(),

    {Y, X} = cecho:getmaxyx(),
    GameOver = "Game Over!",
    Prompt = "Press 'q' or ESCAPE to quit or any other key to continue...",

    HalfY = round(Y / 2),
    HalfX = round(X / 2),

    cecho:mvaddstr(HalfY,     HalfX - round(length(GameOver) / 2), GameOver),
    cecho:mvaddstr(HalfY + 1, HalfX - round(length(Prompt) / 2), Prompt),

    cecho:refresh();

update_screen(#state{snake = Snake, food = Food}) ->
    cecho:erase(),

    add_snake(Snake),
    add_food(Food),

    cecho:refresh().


add_snake(Snake) ->
    lists:foreach(fun add_snake_part/1, Snake).


add_snake_part(Part = #snake_part{y = Y, x = X}) ->
    cecho:mvaddch(Y, X, get_snake_part_character(Part)).


get_snake_part_character(#snake_part{type = head, direction = Direction}) ->
    case Direction of
        up      -> $^;
        down    -> $v;
        left    -> $<;
        right   -> $>
    end;

get_snake_part_character(#snake_part{type = body, direction = Direction}) when Direction =:= up; Direction =:= down -> $";

get_snake_part_character(#snake_part{type = body, direction = Direction}) when Direction =:= left; Direction =:= right -> $=;

get_snake_part_character(#snake_part{type = tail, direction = Direction}) when Direction =:= up; Direction =:= down -> $';

get_snake_part_character(#snake_part{type = tail, direction = Direction}) when Direction =:= left; Direction =:= right -> $-.


add_food(Food) ->
    lists:foreach(fun add_food_instance/1, Food).


add_food_instance(#food_instance{y = Y, x = X}) ->
    cecho:mvaddch(Y, X, $*).
