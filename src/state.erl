-module(state).

-behaviour(gen_server).

-include_lib("cecho/include/cecho.hrl").
-include("../include/state.hrl").

-export([start_link/0, get_state/0, reset_state/0, update_state/1]).

-export([init/1, handle_call/3, handle_cast/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_state() ->
    gen_server:call(?MODULE, get_state).


reset_state() ->
    gen_server:call(?MODULE, reset_state).


update_state(Change) ->
    gen_server:cast(?MODULE, {update_state, Change}).


init([]) ->
    {ok, initialize_state()}.


handle_call(get_state, _From, State) ->
    NewState = state_tick(State),
    {reply, NewState, NewState};

handle_call(reset_state, _From, _State) ->
    NewState = initialize_state(),
    {reply, NewState, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({update_state, Change}, State) ->
    {noreply, update_state(Change, State)};

handle_cast(_Request, State) ->
    {noreply, State}.


state_tick(State = #state{status = Status}) when Status =:= shutdown; Status =:= lost ->
    State;

state_tick(State) ->
    State1 = update_snake_part_positions(State),
    State2 = handle_consume_food(State1),
    State3 = handle_collision(State2),
    State4 = update_speed(State3),
    State5 = update_food(State4),
    State6 = update_snake_part_directions(State5),
    State6.


initialize_state() ->
    Head = #snake_part{type = head, y = 0, x = 2, last_direction = right, direction = right},
    Body = #snake_part{type = body, y = 0, x = 1, last_direction = right, direction = right},
    Tail = #snake_part{type = tail, y = 0, x = 0, last_direction = right, direction = right},

    {Y, X} = cecho:getmaxyx(),
    Food = [#food_instance{y = rand:uniform(Y) - 1, x = rand:uniform(X) - 1}],

    #state{status = normal, speed = ?INITIAL_X_SPEED_MS, snake = [Head, Body, Tail], food = Food}.


update_state(_Change, #state{status = lost}) ->
    initialize_state();

update_state(up, State = #state{snake = [Head = #snake_part{direction = Direction} | Tail]}) ->
    case Direction of
        down    -> State;
        _       -> State#state{snake = [Head#snake_part{direction = up} | Tail]}
    end;

update_state(down, State = #state{snake = [Head = #snake_part{direction = Direction} | Tail]}) ->
    case Direction of
        up  -> State;
        _   -> State#state{snake = [Head#snake_part{direction = down} | Tail]}
    end;

update_state(left, State = #state{snake = [Head = #snake_part{direction = Direction} | Tail]}) ->
    case Direction of
        right   -> State;
        _       -> State#state{snake = [Head#snake_part{direction = left} | Tail]}
    end;

update_state(right, State = #state{snake = [Head = #snake_part{direction = Direction} | Tail]}) ->
    case Direction of
        left    -> State;
        _       -> State#state{snake = [Head#snake_part{direction = right} | Tail]}
    end.


update_snake_part_positions(State = #state{snake = Snake}) ->
    State#state{snake = lists:map(fun update_snake_part_position/1, Snake)}.


update_snake_part_position(SnakePart = #snake_part{y = Y, x = X, direction = Direction}) ->
    case Direction of
        up      -> SnakePart#snake_part{y = Y - 1};
        down    -> SnakePart#snake_part{y = Y + 1};
        left    -> SnakePart#snake_part{x = X - 1};
        right   -> SnakePart#snake_part{x = X + 1}
    end.


handle_consume_food(State = #state{snake = [#snake_part{y = Y, x = X} | _] = Snake, food = Food}) ->
    case encountered_food(Y, X, Food) of
        false   -> State;
        true    -> State#state{snake = add_snake_part(Snake)}
    end.


encountered_food(SnakeY, SnakeX, Food) ->
    lists:any(fun(#food_instance{y = FoodY, x = FoodX}) when FoodY =:= SnakeY, FoodX =:= SnakeX -> true; (_) -> false end, Food).


add_snake_part(Snake) ->
    [#snake_part{y = Y, x = X, last_direction = LastDirection} = Head | Tail] = lists:reverse(Snake),
    NewHead = case LastDirection of
                  up    -> Head#snake_part{y = Y + 1};
                  down  -> Head#snake_part{y = Y - 1};
                  left  -> Head#snake_part{x = X + 1};
                  right -> Head#snake_part{x = X - 1}
              end,
    lists:reverse([NewHead#snake_part{direction = LastDirection} | [Head#snake_part{type = body} | Tail]]).


handle_collision(State = #state{snake = [#snake_part{y = SnakeY, x = SnakeX} | _] = Snake}) ->
    {MaxY, MaxX} = cecho:getmaxyx(),
    WithinBounds = 0 =< SnakeY andalso SnakeY < MaxY andalso 0 =< SnakeX andalso SnakeX < MaxX,
    SnakeCollision = collided_with_self(Snake),
    case WithinBounds andalso not SnakeCollision of
        false   -> State#state{status = lost};
        true    -> State
    end.


collided_with_self([]) ->
    false;

collided_with_self([#snake_part{y = Y, x = X} | Tail]) ->
    case lists:member({Y, X}, lists:map(fun(#snake_part{y = NewY, x = NewX}) -> {NewY, NewX} end, Tail)) of
        false   -> collided_with_self(Tail);
        true    -> true
    end.


update_snake_part_directions(State = #state{snake = Snake}) ->
    State#state{snake = update_snake_part_directions(lists:reverse(Snake), [])}.


update_snake_part_directions([Head = #snake_part{direction = OldDirection} | [Body = #snake_part{direction = NewDirection} | []]], NewSnake) ->
    NewHead = Head#snake_part{last_direction = OldDirection, direction = NewDirection},
    update_snake_part_directions(Body, [NewHead | NewSnake]);

update_snake_part_directions([Head = #snake_part{direction = OldDirection} | [Body = #snake_part{direction = NewDirection} | Tail]], NewSnake) ->
    NewHead = Head#snake_part{last_direction = OldDirection, direction = NewDirection},
    update_snake_part_directions([Body | Tail], [NewHead | NewSnake]);

update_snake_part_directions(Head = #snake_part{direction = Direction}, NewSnake) ->
    NewHead = Head#snake_part{last_direction = Direction},
    [NewHead | NewSnake].


update_speed(State = #state{speed = Speed, snake = [#snake_part{y = Y, x = X, last_direction = LastDirection, direction = Direction} | _], food = Food})
    when (LastDirection =:= up orelse LastDirection =:= down), (Direction =:= up orelse Direction =:= down) ->
    case encountered_food(Y, X, Food) of
        false   -> State;
        true    -> State#state{speed = lists:max([?MIN_Y_SPEED_MS, Speed - ?Y_SPEED_STEP_MS])}
    end;

update_speed(State = #state{speed = Speed, snake = [#snake_part{y = Y, x = X, last_direction = LastDirection, direction = Direction} | _], food = Food})
    when (LastDirection =:= left orelse LastDirection =:= right), (Direction =:= left orelse Direction =:= right) ->
    case encountered_food(Y, X, Food) of
        false   -> State;
        true    -> State#state{speed = lists:max([?MIN_X_SPEED_MS, Speed - ?X_SPEED_STEP_MS])}
    end;

update_speed(State = #state{speed = Speed, snake = [#snake_part{y = Y, x = X, direction = Direction} | _], food = Food}) when Direction =:= up; Direction =:= down ->
    case encountered_food(Y, X, Food) of
        false   -> State#state{speed = lists:max([?MIN_Y_SPEED_MS, round(Speed * ?FONT_YX_ASPECT_RATIO)])};
        true    -> State#state{speed = lists:max([?MIN_Y_SPEED_MS, round(Speed * ?FONT_YX_ASPECT_RATIO) - ?Y_SPEED_STEP_MS])}
    end;

update_speed(State = #state{speed = ?MIN_Y_SPEED_MS}) ->
    State#state{speed = ?MIN_X_SPEED_MS};

update_speed(State = #state{speed = Speed, snake = [#snake_part{y = Y, x = X} | _], food = Food}) ->
    case encountered_food(Y, X, Food) of
        false   -> State#state{speed = lists:max([?MIN_X_SPEED_MS, round(Speed * ?FONT_XY_ASPECT_RATIO)])};
        true    -> State#state{speed = lists:max([?MIN_X_SPEED_MS, round(Speed * ?FONT_XY_ASPECT_RATIO) - ?X_SPEED_STEP_MS])}
    end.


update_food(State = #state{snake = [#snake_part{y = Y, x = X} | _], food = Food}) ->
    case encountered_food(Y, X, Food) of
        false   -> State;
        true    -> replace_food(State)
    end.


replace_food(State = #state{snake = [#snake_part{y = Y, x = X} | _], food = Food}) ->
    UneatenFood = remove_food(Y, X, Food),
    NewFood = add_food(UneatenFood),
    State#state{food = NewFood}.


remove_food(SnakeY, SnakeX, Food) ->
    lists:filter(fun(#food_instance{y = FoodY, x = FoodX}) when FoodY =:= SnakeY, FoodX =:= SnakeX -> false; (_) -> true end, Food).


add_food(Food) ->
    {Y, X} = cecho:getmaxyx(),
    NewFood = #food_instance{y = rand:uniform(Y) - 1, x = rand:uniform(X) - 1},
    [NewFood | Food].