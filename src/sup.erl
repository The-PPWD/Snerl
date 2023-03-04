-module(sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupFlags = #{intensity => 0,
                 auto_shutdown => any_significant},

    ChildSpecs = [#{id => state,
                    start => {state, start_link, []}},

                  #{id => input,
                    start => {input, start_link, []}},

                  #{id => render,
                    start => {render, start_link, []}},

                  #{id => server,
                    start => {server, start_link, []},
                    restart => transient,
                    significant => true}],

    {ok, {SupFlags, ChildSpecs}}.
