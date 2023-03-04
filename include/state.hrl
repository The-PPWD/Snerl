-record(snake_part, {type, y, x, last_direction, direction}).
-record(food_instance, {y, x}).

-record(state, {status, speed, snake, food}).

-define(FONT_YX_ASPECT_RATIO, 2).
-define(FONT_XY_ASPECT_RATIO, 0.5).

-define(MIN_X_SPEED_MS, 25).
%% MIN_Y_SPEED_MS = round(?MIN_X_SPEED_MS * ?FONT_YX_ASPECT_RATIO)
-define(MIN_Y_SPEED_MS, 50).

-define(INITIAL_X_SPEED_MS, 250).
%% INITIAL_Y_SPEED_MS = round(?INITIAL_X_SPEED_MS * ?FONT_YX_ASPECT_RATIO)
-define(INITIAL_Y_SPEED_MS, 500).

-define(X_SPEED_STEP_MS, 25).
%% Y_SPEED_STEP_MX = round(?X_SPEED_STEP_MS * ?FONT_YX_ASPECT_RATIO)
-define(Y_SPEED_STEP_MS, 50).