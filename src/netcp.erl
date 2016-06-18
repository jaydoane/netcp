-module(netcp).

%% external api
%% app internal
-export([accept/1, recv/5, transport/1]).

%% -compile(export_all).

-include("netcp.hrl").



accept(Listen) when is_port(Listen) ->
    gen_tcp:accept(Listen);
accept(Listen) when is_tuple(Listen) -> % is_tuple since ssl_api.hrl is not exposed
    {ok, Socket} = ssl:transport_accept(Listen),
    ok = ssl:ssl_accept(Socket),
    io:format("SSL connection info ~p~n", [ssl:connection_information(Socket)]),
    {ok, Socket}.

recv(Transport, Socket, Device, ByteCount, CheckSum) ->
    case Transport:recv(Socket, 0) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv(Transport, Socket, Device,
                ByteCount + iolist_size(Data),
                erlang:adler32(CheckSum, Data));
        {error, closed} ->
            {ok, ByteCount, CheckSum}
    end.


sendfile(Transport, Host, Path, Opts) ->
    sendfile(Transport, Host, ?DEFAULT_PORT, Path, Opts).

sendfile(tcp, Host, Port, Path, Opts) ->
    sendfile(gen_tcp, Host, Port, Path, Opts);
sendfile(Transport, Host, Port, Path, Opts) ->
    Start = erlang:system_time(micro_seconds),
    BufSize = proplists:get_value(readbuf, Opts, ?DEFAULT_FILE_READ_BUF_SIZE),
    {ok, Socket} = Transport:connect(Host, Port, connect_opts(Transport, Opts)),
    {ok, Device} = file:open(Path, [read, raw, binary]),

    {ok, ByteCount, CheckSum} = send(
        Transport, Socket, Device, BufSize, 0, erlang:adler32(<<>>)),

    ok = file:close(Device),
    ok = Transport:close(Socket),
    ElapsedMicroSeconds = erlang:system_time(micro_seconds) - Start,
    {ok, ByteCount, CheckSum, ByteCount/ElapsedMicroSeconds}.

connect_opts(Transport, Opts) ->
    case Transport of
        ssl ->
            ok = ssl:start(), % FIX
            [prop(ciphers, Opts, ?DEFAULT_CIPHERS)];
        _ ->
            []
    end ++ ?BASE_TCP_OPTS.

send(Transport, Socket, Device, BufSize, ByteCount, CheckSum) ->
    case file:read(Device, BufSize) of
        {ok, Data} ->
            case Transport:send(Socket, Data) of
                ok ->
                    send(Transport, Socket, Device, BufSize,
                        ByteCount + size(Data),
                        erlang:adler32(CheckSum, Data));
                Else ->
                    Else
            end;
        eof ->
            {ok, ByteCount, CheckSum}
    end.

transport(Socket) when is_port(Socket) ->
    gen_tcp;
transport(_) ->
    ssl.

prop(Key, Opts, Default) ->
    {Key, proplists:get_value(Key, Opts, Default)}.    
