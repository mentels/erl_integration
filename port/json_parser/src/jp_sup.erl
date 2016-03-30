-module(jp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 10},
    ChildSPec = #{id => jp_server,
                  start => {jp_server, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [jp_server]},
    {ok, {SupFlags, [ChildSPec]}}.
