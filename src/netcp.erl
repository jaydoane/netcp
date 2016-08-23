-module(netcp).

%% external api
-export([
    start/0, stop/0,
    sendfile/3, sendfile/5,
    block_size/0, transformer/0]).
%% app internal
-export([accept/1, recv_file/1, left_pad/2]).

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
    netcp_log:info("netcp ssl accept ~p", [ssl:connection_info(Socket)]),
    {ok, Socket}.

unique_path() ->
    netcp_app:env(dir, "/tmp/netcp-") ++ hex_str(crypto:rand_bytes(4)).
    
hex_str(Bin) ->
    [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= Bin].

recv_file(Socket) ->
    Transport = transport(Socket),
    {Device, Path, ExpectedSize, Header, ByteCount, CheckSum0} = 
        prepare_device(Socket),
    {ok, Size, CheckSum} =
        recv(Socket, Device, ExpectedSize, ByteCount, CheckSum0),
    Response = #{path => Path, size => Size, checksum => CheckSum},
    ok = maybe_respond(Socket, Response, Header),
    ok = file:close(Device),
    ok = Transport:close(Socket),
    netcp_log:info("netcp recv_file ~p", [Response]),
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
    ok; % only respond if sent header
maybe_respond(Socket, Response, _Header) ->
    BinResponse = term_to_binary(Response),
    Transport = transport(Socket),
    netcp_log:debug("netcp respond ~p", [Response]),
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
    netcp_log:debug("netcp send_header ~p", [Header]),
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

sendfile(Host, Path, Opts) ->
    Transport = netcp_config:get(transport, ?DEFAULT_TRANSPORT),
    Port = netcp_config:get(port, ?DEFAULT_PORT),
    sendfile(Transport, Host, Port, Path, Opts).

sendfile(tcp, Host, Port, Path, Opts) ->
    sendfile(gen_tcp, Host, Port, Path, Opts);
sendfile(Transport, Host, Port, Path, Opts) ->
    Start = epoch_micro_seconds(),
    ConnectOpts = connect_opts(Transport, Opts),
    ok = netcp_ssl:maybe_start_ssl(ConnectOpts),
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
    {ok, Socket} = Transport:connect(Host, Port, ConnectOpts, Timeout),
    {ok, Device} = file:open(Path, [read, raw, binary]),
    Mod = proplists:get_value(transform, Opts, ?MODULE),
    BufBlockCount = proplists:get_value(
        readbuf_blocks, Opts, ?DEFAULT_READ_BUF_BLOCK_COUNT),
    BufSize = Mod:block_size() * BufBlockCount,
    ok = maybe_send_header(Socket, Path, Opts),
    {ok, ByteCount, CheckSum} = 
        send(Socket, Device, BufSize, 0, erlang:adler32(<<>>), Mod:transformer()),
    ok = file:close(Device),
    Response = maybe_recv_response(Socket, Opts, ByteCount, CheckSum),
    ok = Transport:close(Socket),
    ElapsedMicroSeconds = epoch_micro_seconds() - Start,
    #{size := ByteCount, checksum := CheckSum} = Response,
    {ok, ByteCount, ByteCount/ElapsedMicroSeconds}.

maybe_send_header(Socket, Path, Opts) ->
    case proplists:get_value(raw, Opts) of
        undefined ->
            Props = case proplists:get_value(path, Opts) of
                undefined ->
                    [];
                RemotePath ->
                    [{path, RemotePath}]
            end ++ [{size, filelib:file_size(Path)}],
            ok = send_header(Socket, maps:from_list(Props));
        _ -> 
            ok
    end.

maybe_recv_response(Socket, Opts, ByteCount, CheckSum) ->
    case proplists:get_value(raw, Opts) of
        undefined ->
            Response = recv_response(Socket),
            ok = check_response(ByteCount, CheckSum, Response),
            Response;
        _ ->
            undefined
    end.

recv_response(Socket) ->
    Transport = transport(Socket),
    {ok, BinResponse} = Transport:recv(Socket, 0, ?RESPONSE_TIMEOUT), % FIXME
    Response = binary_to_term(BinResponse),
    netcp_log:debug("netcp recv_response ~p", [Response]),
    Response.

check_response(ByteCount, CheckSum, Resp) ->
    ByteCount = maps:get(size, Resp),
    CheckSum = maps:get(checksum, Resp),
    ok.

connect_opts(ssl, Opts) ->
    ?BASE_TCP_OPTS ++ [prop(ciphers, Opts, ?DEFAULT_CIPHERS)];
connect_opts(_, _) ->
    ?BASE_TCP_OPTS.

send(Socket, Device, BufSize, Pos, CheckSum, Transform) ->
    case file:pread(Device, Pos, BufSize) of
        {ok, Data0} ->
            Data = Transform(Data0),
            Transport = transport(Socket),
            case Transport:send(Socket, Data) of
                ok ->
                    send(Socket, Device, BufSize, Pos + iolist_size(Data),
                        erlang:adler32(CheckSum, Data), Transform);
                Else ->
                    Else
            end;
        eof ->
            {ok, Pos, CheckSum}
    end.

%% default is no-op
transformer() ->
    fun(Data) -> Data end.

block_size() ->
    ?DEFAULT_BLOCK_SIZE.

transport(Socket) when is_port(Socket) ->
    gen_tcp;
transport(_) ->
    ssl.

prop(Key, Opts, Default) ->
    {Key, proplists:get_value(Key, Opts, Default)}.    

%% support OTP 17
epoch_micro_seconds() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
