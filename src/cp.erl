-module(cp).

-compile(export_all).

-define(FILE_READ_BUF_SIZE, 1000000).

-define(CIPHERS, [
    {rsa,aes_128_gcm,sha},
    {rsa,aes_128_cbc,sha},
    {rsa,rc4_128,md5}]).

rate(M, F, A) ->
    case timer:tc(M, F, A) of
        {ElapsedMicroSec, {ok, Bytes, CheckSum}} ->
            {Bytes, ElapsedMicroSec, Bytes/ElapsedMicroSec, CheckSum}; % MB/s
        {ElapsedMicroSec, Bytes} ->
            {Bytes, ElapsedMicroSec, Bytes/ElapsedMicroSec} % MB/s
    end.

wait_tcp(Port, File, RecBufSize) ->
    BaseOpts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    TcpOpts = case RecBufSize of
        undefined -> BaseOpts;
        _ -> BaseOpts ++ [{recbuf, RecBufSize}]
    end,
    {ok, Listen} = gen_tcp:listen(Port, TcpOpts),
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("Socket ~p~n", [Socket]),
    {ok, Device} = file:open(File, [write, raw]),

    %% {ok, ByteCount} = recv(Socket, Device, 0),
    {ok, ByteCount, CheckSum} = recv_checksum(
        Socket, Device, 0, erlang:adler32(<<>>)),

    ok = file:close(Device),
    ok = gen_tcp:close(Socket),
    {ok, ByteCount, CheckSum}.

recv_checksum(Socket, Device, AccByteCount, AccCheckSum) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv_checksum(Socket, Device,
                AccByteCount + erlang:iolist_size(Data),
                erlang:adler32(AccCheckSum, Data));
        {error, closed} ->
            {ok, AccByteCount, AccCheckSum}
    end.

recv(Socket, Device, AccByteCount) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv(Socket, Device, AccByteCount + erlang:iolist_size(Data));
        {error, closed} ->
            {ok, AccByteCount}
    end.

wait_ssl(Port, File, RecBufSize) ->
    wait_ssl(Port, File, RecBufSize, ?CIPHERS).

wait_ssl(Port, File, RecBufSize, Ciphers) ->
    BaseOpts = [binary, {packet, 0}, {active, false}, {reuseaddr, true}],
    TcpOpts = case RecBufSize of
        undefined -> BaseOpts;
        _ -> BaseOpts ++ [{recbuf, RecBufSize}]
    end,
    SslOpts = [
        {certfile,"/tmp/certificate.pem"},
        {keyfile, "/tmp/key.pem"},
        {ciphers, Ciphers}],
    ok = ssl:start(),
    {ok, Listen} = ssl:listen(Port, TcpOpts ++ SslOpts),
    {ok, Socket} = ssl:transport_accept(Listen),
    io:format("Socket ~p~n", [Socket]),
    ok = ssl:ssl_accept(Socket),
    io:format("SSL info ~p~n", [ssl:connection_info(Socket)]),
    {ok, Device} = file:open(File, [write, raw]),

    {ok, ByteCount} = recv_ssl(Socket, Device, 0),

    ok = file:close(Device),
    ok = ssl:close(Socket),
    {ok, ByteCount}.

recv_ssl(Socket, Device, ByteCount) ->
    case ssl:recv(Socket, 0) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv_ssl(Socket, Device, ByteCount + erlang:iolist_size(Data));
        {error, closed} ->
            {ok, ByteCount}
    end.

sendfile(Host, Port, Path) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    {ok, ByteCount} = file:sendfile(Path, Socket),
    ok = gen_tcp:close(Socket),
    ByteCount.

sendfile_tcp(Host, Port, Path) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    {ok, Device} = file:open(Path, [read, raw, binary]),
    %% {ok, ByteCount} = send_tcp(Socket, Device, 0),
    {ok, ByteCount, CheckSum} = send_tcp_checksum(
        Socket, Device, 0, erlang:adler32(<<>>)),
    ok = file:close(Device),
    ok = gen_tcp:close(Socket),
    {ok, ByteCount, CheckSum}.

send_tcp(Socket, Device, ByteCount) ->
    case file:read(Device, ?FILE_READ_BUF_SIZE) of
        {ok, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    send_tcp(Socket, Device, ByteCount + size(Data));
                Else ->
                    Else
            end;
        eof ->
            {ok, ByteCount}
    end.

send_tcp_checksum(Socket, Device, AccByteCount, AccCheckSum) ->
    case file:read(Device, ?FILE_READ_BUF_SIZE) of
        {ok, Data} ->
            case gen_tcp:send(Socket, Data) of
                ok ->
                    send_tcp_checksum(
                        Socket, Device, AccByteCount + size(Data),
                        erlang:adler32(AccCheckSum, Data));
                Else ->
                    Else
            end;
        eof ->
            {ok, AccByteCount, AccCheckSum}
    end.

sendfile_ssl(Host, Port, Path) ->
    sendfile_ssl(Host, Port, Path, ?CIPHERS).

sendfile_ssl(Host, Port, Path, Ciphers) ->
    SslOpts = [binary, {packet, 0}, {active, false}, {ciphers, Ciphers}],
    ok = ssl:start(),
    {ok, Socket} = ssl:connect(Host, Port, SslOpts),
    io:format("SSL info ~p~n", [ssl:connection_info(Socket)]),
    %% {ok, Bytes} = file:sendfile(Path, Socket),
    {ok, Device} = file:open(Path, [read, raw, binary]),
    {ok, ByteCount} = send_ssl(Socket, Device, 0),
    ok = file:close(Device),
    ok = ssl:close(Socket),
    ByteCount.

send_ssl(Socket, Device, ByteCount) ->
    case file:read(Device, ?FILE_READ_BUF_SIZE) of
        {ok, Data} ->
            case ssl:send(Socket, Data) of
                ok ->
                    send_ssl(Socket, Device, ByteCount + size(Data));
                Else ->
                    Else
            end;
        eof ->
            {ok, ByteCount}
    end.
