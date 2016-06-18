-module(netcp).

%% external api
-export([
    start/0, stop/0,
    sendfile/4, sendfile/5, recv_file/1,
    transformer/1]).
%% app internal
-export([accept/1, transport/1]).

-include("netcp.hrl").

start() ->
    application:start(netcp).

stop() ->
    application:stop(netcp).

accept(Listen) when is_port(Listen) ->
    gen_tcp:accept(Listen);
accept(Listen) when is_tuple(Listen) -> % is_tuple since ssl_api.hrl is not exposed
    {ok, Socket} = ssl:transport_accept(Listen),
    ok = ssl:ssl_accept(Socket),
    io:format("SSL connection info ~p~n", [ssl:connection_information(Socket)]),
    {ok, Socket}.

unique_path() ->
    netcp_app:env(dir, "/tmp/") ++ "1".
        %% ++ integer_to_list(erlang:unique_integer([positive])).
    
recv_file(Socket) ->
    Transport = transport(Socket),
    UniquePath = unique_path(),
    {Path, Header, Data, ExpectedSize} = case maybe_recv_header(Socket) of
        {header, H} ->
            io:format("header ~p~n", [H]),
            P = maps:get(path, H, UniquePath),
            E = maps:get(size, H, 0),
            {P, H, <<>>, E};
        {data, D} ->
            {UniquePath, undefined, D, 0}
    end,
    {ok, Device} = file:open(Path, [write, raw]),
    ByteCount = case Data of
        <<>> -> 0;
        _ ->
            ok = file:write(Device, Data),
            size(Data)
    end,
    {ok, Size, CheckSum} = recv(
        Transport, Socket, Device, ExpectedSize, ByteCount, erlang:adler32(<<>>)),
    Response = #{path => Path, size => Size, checksum => CheckSum},
    ok = maybe_respond(Socket, Response, Header),
    ok = file:close(Device),
    ok = Transport:close(Socket),
    io:format("Wrote ~p~n", [Response]),
    ok.

maybe_recv_header(Socket) ->
    Transport = transport(Socket),
    case Transport:recv(Socket, size(?MAGIC)) of
        {ok, ?MAGIC} ->
            {ok, BinHeaderSize} = Transport:recv(Socket, ?ENCODED_SIZE),
            HeaderSize = binary:decode_unsigned(BinHeaderSize),
            {ok, BinHeader} = Transport:recv(Socket, HeaderSize),
            {header, binary_to_term(BinHeader)};
        {ok, Data} ->
            {data, Data}
    end.

maybe_respond(_, _, undefined) ->
    ok; % no header -> no response
maybe_respond(Socket, Response, _Header) ->
    BinResponse = term_to_binary(Response),
    Transport = netcp:transport(Socket),
    io:format("respond ~p~n", [Response]),
    ok = Transport:send(Socket, BinResponse).

recv(_Transport, _Socket, _Device, ByteCount, ByteCount, CheckSum) 
    when ByteCount > 0 ->
    {ok, ByteCount, CheckSum};
recv(Transport, Socket, Device, ExpectedSize, ByteCount, CheckSum) ->
    case Transport:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv(Transport, Socket, Device, ExpectedSize,
                ByteCount + iolist_size(Data),
                erlang:adler32(CheckSum, Data));
        {error, timeout} ->
            {ok, ByteCount, CheckSum};
        {error, closed} ->
            {ok, ByteCount, CheckSum}
    end.

send_header(Socket, Header) ->
    io:format("send_header ~p~n", [Header]),
    Transport = transport(Socket),
    ok = Transport:send(Socket, ?MAGIC),
    {BinSize, BinHeader} = encode(?ENCODED_SIZE, Header),
    ok = Transport:send(Socket, BinSize),
    ok = Transport:send(Socket, BinHeader).

encode(ByteWidth, Term) ->
    Bin = term_to_binary(Term),
    EncodedSize = binary:encode_unsigned(size(Bin)),
    PaddedSize = left_pad(EncodedSize, ByteWidth),
    {PaddedSize, Bin}.

left_pad(Bin, ByteWidth) when size(Bin) =< ByteWidth ->
    Bits = ?BITS_PER_BYTE * (ByteWidth - size(Bin)),
    <<0:Bits, Bin/binary>>.

sendfile(Transport, Host, Path, Opts) ->
    sendfile(Transport, Host, ?DEFAULT_PORT, Path, Opts).

sendfile(tcp, Host, Port, Path, Opts) ->
    sendfile(gen_tcp, Host, Port, Path, Opts);
sendfile(Transport, Host, Port, Path, Opts) ->
    Start = erlang:system_time(micro_seconds),
    BufSize = proplists:get_value(readbuf, Opts, ?DEFAULT_FILE_READ_BUF_SIZE),
    {ok, Socket} = Transport:connect(Host, Port, connect_opts(Transport, Opts)),
    
    {ok, Device} = file:open(Path, [read, raw, binary]),
    Mod = proplists:get_value(transform, Opts, ?MODULE),
    Transform = Mod:transformer(Device),

    case proplists:get_value(header, Opts) of
        undefined -> ok;
        _ -> 
            Props = case proplists:get_value(path, Opts) of
                undefined ->
                    [];
                RemotePath ->
                    [{path, RemotePath}]
            end ++ [{size, filelib:file_size(Path)}],
            ok = send_header(Socket, maps:from_list(Props))
    end,
    {ok, ByteCount, CheckSum} = send(
        Socket, Device, BufSize, 0, erlang:adler32(<<>>), Transform),

    ok = file:close(Device),

    Response = case proplists:get_value(header, Opts) of
        undefined -> undefined;
        _ ->
            Resp = recv_response(Transport, Socket),
            ok = check_response(ByteCount, CheckSum, Resp),
            Resp
    end,
    ok = Transport:close(Socket),
    ElapsedMicroSeconds = erlang:system_time(micro_seconds) - Start,
    {ok, ByteCount, CheckSum, ByteCount/ElapsedMicroSeconds, Response}.

recv_response(Transport, Socket) ->
    {ok, BinResponse} = Transport:recv(Socket, 0, ?RESPONSE_TIMEOUT), % FIXME
    Response = binary_to_term(BinResponse),
    io:format("recv_response ~p~n", [Response]),
    Response.

check_response(ByteCount, CheckSum, Resp) ->
    ByteCount = maps:get(size, Resp),
    CheckSum = maps:get(checksum, Resp),
    ok.

connect_opts(Transport, Opts) ->
    case Transport of
        ssl ->
            ok = ssl:start(), % FIX
            [prop(ciphers, Opts, ?DEFAULT_CIPHERS)];
        _ ->
            []
    end ++ ?BASE_TCP_OPTS.

send(Socket, Device, BufSize, Pos, CheckSum, Transform) ->
    case file:pread(Device, Pos, BufSize) of
        {ok, Data0} ->
            Data = Transform(Data0),
            Transport = transport(Socket),
            case Transport:send(Socket, Data) of
                ok ->
                    send(Socket, Device, BufSize, Pos + size(Data),
                        erlang:adler32(CheckSum, Data), Transform);
                Else ->
                    Else
            end;
        eof ->
            {ok, Pos, CheckSum}
    end.

%% default is no-op
transformer(_Device) ->
    fun(Data) -> Data end.

transport(Socket) when is_port(Socket) ->
    gen_tcp;
transport(_) ->
    ssl.

%% getopts(Socket, OptionNames) when is_port(Socket) ->
%%     inet:getopts(Socket, OptionNames);
%% getopts(Socket, OptionNames) ->
%%     ssl:getopts(Socket, OptionNames).

prop(Key, Opts, Default) ->
    {Key, proplists:get_value(Key, Opts, Default)}.    
