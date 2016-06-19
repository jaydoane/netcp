-module(netcp).

%% external api
-export([
    start/0, stop/0,
    sendfile/4, sendfile/5,
    transformer/1]).
%% app internal
-export([accept/1, recv_file/1]).

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
    {Device, Path, ExpectedSize, Header, ByteCount, CheckSum0} = 
        prepare_device(Socket),
    {ok, Size, CheckSum} = recv(
        Socket, Device, ExpectedSize, ByteCount, CheckSum0),
    Response = #{path => Path, size => Size, checksum => CheckSum},
    ok = maybe_respond(Socket, Response, Header),
    ok = file:close(Device),
    ok = Transport:close(Socket),
    io:format("Wrote ~p~n", [Response]),
    ok.

prepare_device(Socket) ->
    {Header, Data, ExpectedSize, Path} = maybe_recv_header(Socket),
    {ok, Device} = file:open(Path, [write, raw]),
    {ByteCount, CheckSum} = begin
        ok = file:write(Device, Data),
        {size(Data), erlang:adler32(Data)}
    end,
    {Device, Path, ExpectedSize, Header, ByteCount, CheckSum}.

maybe_recv_header(Socket) ->
    UniquePath = unique_path(),
    Transport = transport(Socket),
    case Transport:recv(Socket, size(?MAGIC)) of
        {ok, ?MAGIC} ->
            {ok, BinHeaderSize} = Transport:recv(Socket, ?ENCODED_SIZE),
            HeaderSize = binary:decode_unsigned(BinHeaderSize),
            {ok, BinHeader} = Transport:recv(Socket, HeaderSize),
            Header = binary_to_term(BinHeader),
            ExpectedSize = maps:get(size, Header),
            Path = maps:get(path, Header, UniquePath),
            {Header, <<>>, ExpectedSize, Path};
        {ok, Data} ->
            {undefined, Data, 0, UniquePath}
    end.

maybe_respond(_, _, undefined) ->
    ok; % no header -> no response
maybe_respond(Socket, Response, _Header) ->
    BinResponse = term_to_binary(Response),
    Transport = transport(Socket),
    io:format("respond ~p~n", [Response]),
    ok = Transport:send(Socket, BinResponse).

recv(_Socket, _Device, ByteCount, ByteCount, CheckSum) when ByteCount > 0 ->
    {ok, ByteCount, CheckSum};
recv(Socket, Device, ExpectedSize, ByteCount, CheckSum) ->
    Transport = transport(Socket),
    case Transport:recv(Socket, 0, ?RECV_TIMEOUT) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv(Socket, Device, ExpectedSize,
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
    ok = maybe_send_header(Socket, Path, Opts),
    {ok, ByteCount, CheckSum} = send(
        Socket, Device, BufSize, 0, erlang:adler32(<<>>), Transform),
    ok = file:close(Device),
    Response = maybe_recv_response(Socket, Opts, ByteCount, CheckSum),
    ok = Transport:close(Socket),
    ElapsedMicroSeconds = erlang:system_time(micro_seconds) - Start,
    {ok, ByteCount, CheckSum, ByteCount/ElapsedMicroSeconds, Response}.

maybe_send_header(Socket, Path, Opts) ->
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
    end.

maybe_recv_response(Socket, Opts, ByteCount, CheckSum) ->
    case proplists:get_value(header, Opts) of
        undefined -> undefined;
        _ ->
            Response = recv_response(Socket),
            ok = check_response(ByteCount, CheckSum, Response),
            Response
    end.

recv_response(Socket) ->
    Transport = transport(Socket),
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
