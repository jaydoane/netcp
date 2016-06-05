-module(netcp_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("netcp.hrl").

start(_StartType, _StartArgs) ->
    Port = app_env(port, ?DEFAULT_PORT),
    Transport = app_env(transport, ?DEFAULT_TRANSPORT),
    {ok, Listen} = Transport:listen(Port, listen_opts(Transport)),
    io:format("netcp listening on port ~p~n", [Port]),
    case netcp_sup:start_link(Listen) of
        {ok, Pid} ->
            netcp_sup:start_child(),
            {ok, Pid, Listen};
        Other ->
            {error, Other}
    end.

stop(Listen) ->
    ok = (netcp:transport(Listen)):close(Listen).

listen_opts(ssl) ->
    ok = ssl:start(), % FIX?
    [prop(certfile, ?DEFAULT_CERTFILE),
     prop(keyfile, ?DEFAULT_KEYFILE),
     prop(ciphers, ?DEFAULT_CIPHERS)] ++ ?BASE_TCP_OPTS;
listen_opts(_) ->
    ?BASE_TCP_OPTS.

app_env(Key, Default) ->
    application:get_env(netcp, Key, Default).

prop(Key, Defaults) ->
    {Key, app_env(Key, Defaults)}.
