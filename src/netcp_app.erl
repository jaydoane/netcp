-module(netcp_app).

-behaviour(application).

-export([start/2, stop/1, env/2]).

-include("netcp.hrl").

start(_StartType, _StartArgs) ->
    Port = env(port, ?DEFAULT_PORT),
    Transport = env(transport, ?DEFAULT_TRANSPORT),
    ListenOpts = listen_opts(Transport),
    ok = netcp_ssl:maybe_start_ssl(ListenOpts),
    ok = netcp_ssl:maybe_generate_default_cert(ListenOpts),
    {ok, Listen} = Transport:listen(Port, ListenOpts),
    io:format("netcp listening on port ~p transport ~p ~nopts ~p~n",
        [Port, Transport, ListenOpts]),
    case netcp_sup:start_link(Listen) of
        {ok, Pid} ->
            netcp_sup:start_child(),
            {ok, Pid, Listen};
        Other ->
            {error, Other}
    end.

stop(Listen) ->
    ok = (netcp:transport(Listen)):close(Listen).

tcp_opts() ->
    ?BASE_TCP_OPTS ++ [prop(recbuf, ?DEFAULT_RECBUF_SIZE)].

listen_opts(ssl) -> [
    prop(certfile, ?DEFAULT_CERTFILE),
    prop(keyfile, ?DEFAULT_KEYFILE),
    prop(ciphers, ?DEFAULT_CIPHERS)] ++ tcp_opts();
listen_opts(_) ->
    tcp_opts().

env(Key, Default) ->
    application:get_env(netcp, Key, Default).

prop(Key, Defaults) ->
    {Key, env(Key, Defaults)}.
