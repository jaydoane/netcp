-module(netcopy).

-export([listen/4, sendfile/5]).
-export([generate_cert/0, generate_cert/2, test/1]).

-define(BASE_TCP_OPTS, [
    binary, {reuseaddr, true}, {active, false}, {exit_on_close, false}]).

-define(DEFAULT_RECBUF_SIZE, 16*1024*1024).
-define(DEFAULT_FILE_READ_BUF_SIZE, 16*1024*1024).

-define(DEFAULT_CERTFILE, "/tmp/netcopy/cert.pem").
-define(DEFAULT_KEYFILE, "/tmp/netcopy/key.pem").
-define(DEFAULT_CIPHERS, [{rsa,aes_128_cbc,sha},
                          {rsa,rc4_128,md5}]).

-define(RETRY_DELAY_MS, 100).
-define(CONNECT_ATTEMPTS, 5).


listen(tcp, Port, Path, Opts) ->
    listen(gen_tcp, Port, Path, Opts);
listen(Transport, Port, Path, Opts)
    when Transport =:= gen_tcp; Transport =:= ssl ->
    ListenOpts = listen_opts(Transport, Opts),
    ok = maybe_start_ssl(ListenOpts),
    ok = maybe_generate_default_cert(ListenOpts),
    {ok, Listen} = Transport:listen(Port, ListenOpts),
    {ok, Socket} = accept(Listen),
    {ok, Device} = file:open(Path, [write, raw]),
    {ok, ByteCount, CheckSum} =
        recv(Transport, Socket, Device, 0, erlang:adler32(<<>>)),
    ok = file:close(Device),
    Response = term_to_binary(#{
        path => Path,
        bytecount => ByteCount,
        checksum => CheckSum}),
    ok = Transport:send(Socket, Response),
    ok = Transport:close(Socket),
    ok = Transport:close(Listen),
    {ok, ByteCount, CheckSum}.

listen_opts(Transport, Opts) ->
    TcpOpts = ?BASE_TCP_OPTS ++ [prop(recbuf, Opts, ?DEFAULT_RECBUF_SIZE)],
    case Transport of
        ssl ->
            [prop(certfile, Opts, ?DEFAULT_CERTFILE),
             prop(keyfile, Opts, ?DEFAULT_KEYFILE),
             prop(ciphers, Opts, ?DEFAULT_CIPHERS)];
        _ ->
            []
    end ++ TcpOpts.

accept(Listen) when is_port(Listen) ->
    gen_tcp:accept(Listen);
accept(Listen) when is_tuple(Listen) -> % is_tuple since ssl_api.hrl is not exposed
    {ok, Socket} = ssl:transport_accept(Listen),
    ok = ssl:ssl_accept(Socket),
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


sendfile(tcp, Host, Port, Path, Opts) ->
    sendfile(gen_tcp, Host, Port, Path, Opts);
sendfile(Transport, Host, Port, Path, Opts)
    when Transport =:= gen_tcp; Transport =:= ssl ->
    Start = erlang:system_time(micro_seconds),
    BufSize = proplists:get_value(readbuf, Opts, ?DEFAULT_FILE_READ_BUF_SIZE),
    ConnectOpts = connect_opts(Transport, Opts),
    ok = maybe_start_ssl(ConnectOpts),
    {ok, Socket} = connect_retry(Transport, Host, Port, ConnectOpts,
                                ?RETRY_DELAY_MS, ?CONNECT_ATTEMPTS),
    {ok, Device} = file:open(Path, [read, raw, binary]),
    {ok, ByteCount, CheckSum} = send(
        Transport, Socket, Device, BufSize, 0, erlang:adler32(<<>>)),
    ok = file:close(Device),
    ok = Transport:shutdown(Socket, write),
    {ok, Response} = Transport:recv(Socket, 0, 5000),
    io:format("Response ~p~n", [binary_to_term(Response)]),
    ok = Transport:close(Socket),
    ElapsedMicroSeconds = erlang:system_time(micro_seconds) - Start,
    {ok, ByteCount, CheckSum, ByteCount/ElapsedMicroSeconds}.

connect_retry(Transport, Host, Port, ConnectOpts, RetryDelayMS, AttemptCount) ->
    case Transport:connect(Host, Port, ConnectOpts) of
        {ok, Socket} ->
            {ok, Socket};
        {error, _} when AttemptCount > 0 ->
            ok = timer:sleep(RetryDelayMS),
            connect_retry(Transport, Host, Port, ConnectOpts, 
                          RetryDelayMS, AttemptCount - 1);
        Else ->
            Else
    end.

connect_opts(Transport, Opts) ->
    case Transport of
        ssl ->
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

prop(Key, Opts, Default) ->
    {Key, proplists:get_value(Key, Opts, Default)}.    


maybe_start_ssl(Opts) ->
    case proplists:get_value(ciphers, Opts) of
        false ->
            ok;
        _ ->
            ok = ssl:start()
    end.

maybe_generate_default_cert(Opts) ->
    case proplists:get_value(certfile, Opts) of
        false ->
            ok;
        Path when Path =:= ?DEFAULT_CERTFILE ->
            case filelib:is_file(Path) of
                false ->
                    ok = generate_cert();
                true ->
                    ok
            end;
        _ ->
            ok
    end.
                    
generate_cert() ->
    generate_cert(?DEFAULT_KEYFILE, ?DEFAULT_CERTFILE).

generate_cert(Keyfile, Certfile) ->
    [ok = filelib:ensure_dir(Path) || Path <- [Keyfile, Certfile]],
    Cmd = io_lib:format(
        "openssl req -x509 -nodes -days 3650 "
        "-subj '/C=US/ST=CA/O=Company Name/CN=server.name.com' "
        "-newkey rsa:2048 -keyout ~s -out ~s", [Keyfile, Certfile]),
    Output = os:cmd(Cmd),
    ok = io:format(Output).

test(Transport) ->
    Port = 10101,
    Path = "/tmp/netcopy-test.out",
    spawn(?MODULE, listen, [Transport, Port, Path, []]),
    {ok, _, _, _} = sendfile(Transport, "localhost", Port, "/etc/hosts", []),
    ok = file:delete(Path).
