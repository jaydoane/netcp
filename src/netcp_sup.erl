-module(netcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Listen) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Listen]).

start_child() ->
    supervisor:start_child(?SERVER, []).

init([Listen]) ->
    Server = {
        netcp_server, {netcp_server, start_link, [Listen]},
        temporary, brutal_kill, worker, [netcp_server]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1}, % FIX
    {ok, {RestartStrategy, Children}}.
