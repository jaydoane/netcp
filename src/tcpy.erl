-module(tcpy).

-compile(export_all).

-define(TRANSPORT, gen_tcp).

-define(BITS_PER_BYTE, 8).

%% send(Host, Port, IOData) ->
%%     {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
%%     ok = gen_tcp:send(Socket, IOData),
%%     ok = gen_tcp:close(Socket).

%% sendfile() ->
%%     sendfile("db2.testy013.cloudant.net", 12378,
%%         "/srv/rebal2_test_assets/shard_maps.tar.gz").

sendfile(Host, Port, Path) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    EncodedPath = encode(Path),
    ok = gen_tcp:send(Socket, EncodedPath),
    {ok, Bytes} = file:sendfile(Path, Socket),
    %% Stat = inet:getstat(Socket),
    ok = inet:setopts(Socket, [{active, false}]),
    Opts = inet:getopts(Socket, [active]),
    %% io:format("Stat ~p~n", [Stat]),
    ok = gen_tcp:shutdown(Socket, write),
    io:format("Opts ~p~n", [Opts]),
    {ok, Response} = gen_tcp:recv(Socket, 0, 5000),
    io:format("Response ~p~n", [Response]),
    io:format("Decoded Response ~p~n", [binary:decode_unsigned(Response)]),
    ok = gen_tcp:close(Socket),
    Bytes.

get(Port, File) ->
    get(Port, File, undefined).

get(Port, File, RecBuf) ->
    BaseOpts = [
        binary, {packet, 0}, {active, false}, {reuseaddr, true}, 
        {exit_on_close, false}],
    TcpOpts = case RecBuf of
        undefined -> BaseOpts;
        _ -> BaseOpts ++ [{recbuf, RecBuf}]
    end,
    {ok, Listen} = gen_tcp:listen(Port, TcpOpts),
    {ok, ListenOpts} = inet:getopts(Listen, [buffer, recbuf]),
    io:format("ListenOpts ~p~n", [ListenOpts]),
    
    {ok, Socket} = gen_tcp:accept(Listen),
    {ok, SocketOpts} = inet:getopts(Socket, [buffer, recbuf]),
    io:format("SocketOpts ~p~n", [SocketOpts]),

    {ok, EncodedSize} = gen_tcp:recv(Socket, 2),
    Size = binary:decode_unsigned(EncodedSize),
    io:format("Size ~p~n", [Size]),
    {ok, BinPath} = gen_tcp:recv(Socket, Size),
    io:format("BinPath ~p~n", [BinPath]),
    
    {ok, Device} = file:open(File, [write, raw]),
    %% {ok, Device} = file:open(BinPath, [write, raw]),

    {ok, ByteCount} = recv(Socket, Device, 0),
    EncodedByteCount = binary:encode_unsigned(ByteCount),
    io:format("Returning ByteCount ~p~n", [EncodedByteCount]),
    ok = gen_tcp:send(Socket, EncodedByteCount),

    ok = file:close(Device),
    ok = gen_tcp:close(Socket),
    ok = gen_tcp:close(Listen),
    {ok, ByteCount}.

recv(Socket, Device, ByteCount) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = file:write(Device, Data),
            recv(Socket, Device, ByteCount + erlang:iolist_size(Data));
        {error, closed} ->
            {ok, ByteCount}
    end.

pad_2(<<_>> = Bin) ->
    <<0, Bin/binary>>;
pad_2(<<_,_>> = Bin) ->
    Bin.

encode(Path) when is_list(Path) ->
    BinPath = list_to_binary(Path),
    Size = size(BinPath),
    EncodedSize = binary:encode_unsigned(Size),
    PaddedSize = pad_2(EncodedSize),
    [PaddedSize, BinPath];
encode(Term) ->
    Bin = term_to_binary(Term),
    Size = size(Bin),
    EncodedSize = binary:encode_unsigned(Size),
    PaddedSize = pad4(EncodedSize),
    [PaddedSize, Bin].

encode(Term, ByteWidth) ->
    Bin = term_to_binary(Term),
    EncodedSize = binary:encode_unsigned(size(Bin)),
    PaddedSize = left_pad(EncodedSize, ByteWidth),
    [PaddedSize, Bin].


left_pad(Bin, ByteWidth) when size(Bin) =< ByteWidth ->
    Bits = ?BITS_PER_BYTE * (ByteWidth - size(Bin)),
    <<0:Bits, Bin/binary>>.


pad4(<<_>> = Bin) ->
    <<0, 0, 0, Bin/binary>>;
pad4(<<_, _>> = Bin) ->
    <<0, 0, Bin/binary>>;
pad4(<<_, _, _>> = Bin) ->
    <<0, Bin/binary>>;
pad4(<<_, _, _, _>> = Bin) ->
    <<Bin/binary>>.


