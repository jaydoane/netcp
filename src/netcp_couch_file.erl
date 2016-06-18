-module(netcp_couch_file).

-export([block_size/0, transformer/1]).
-export([uuid/0]).

-define(BLOCK_SIZE, 16#1000). % 4 KiB
-define(UUID_BYTE_SIZE, 16).
-define(HEADER_OFFSET, -27).

-record(db_header, {
    disk_version = 6,
    update_seq = 0,
    unused = 0,
    id_tree_state = nil,
    seq_tree_state = nil,
    local_tree_state = nil,
    purge_seq = 0,
    purged_docs = nil,
    security_ptr = nil,
    revs_limit = 1000,
    uuid,
    epochs,
    compacted_seq
}).

block_size() ->
    ?BLOCK_SIZE.

transformer(Device) ->
    Header = first_header(Device, ?BLOCK_SIZE),
    io:format("transformer Header ~p~n", [Header]),
    Uuid = uuid(Header),
    NewUuid = uuid(),
    io:format("transformer UUIDs ~p -> ~p~n", [Uuid, NewUuid]),
    fun(Data) ->
        binary:replace(Data, Uuid, NewUuid, [global])
    end.


hexify(Bin) ->
    << << if N >= 10 -> N - 10 + $a;
        true         -> N      + $0 end >>
    || <<N:4>> <= Bin >>.

uuid() ->
    hexify(crypto:rand_bytes(?UUID_BYTE_SIZE)).

uuid(#db_header{uuid = Uuid}) ->
    Uuid.

first_header(Device, BlockSize) ->
    Pattern = binary:compile_pattern([<<"db_header">>]),
    load_header(Device, hd(offsets(Device, Pattern, BlockSize))).

offsets(Device, Pattern, BlockSize) ->
    {ok, Data} = file:pread(Device, 0, BlockSize),
    [Offset || {Offset, _} <- binary:matches(Data, Pattern)].

load_header(Device, Pos) ->
    HeaderPos = Pos + ?HEADER_OFFSET,
    {ok, <<1>>} = file:pread(Device, HeaderPos, 1),
    {ok, <<HeaderLen:32/integer>>} = file:pread(Device, HeaderPos + 1, 4),
    TotalBytes = total_read_len(1, HeaderLen),
    {ok, <<RawBin:TotalBytes/binary>>} = file:pread(
        Device, HeaderPos + 5, TotalBytes),
    <<Md5Sig:16/binary, HeaderBin/binary>> =
	iolist_to_binary(remove_block_prefixes(5, RawBin)),
    Md5Sig = crypto:hash(md5, HeaderBin),
    binary_to_term(HeaderBin).

total_read_len(0, FinalLen) ->
    total_read_len(1, FinalLen) + 1;
total_read_len(BlockOffset, FinalLen) ->
    case ?BLOCK_SIZE - BlockOffset of
        BlockLeft when BlockLeft >= FinalLen ->
            FinalLen;
        BlockLeft ->
            FinalLen + ((FinalLen - BlockLeft) div (?BLOCK_SIZE - 1)) +
            if ((FinalLen - BlockLeft) rem (?BLOCK_SIZE - 1)) =:= 0 -> 0;
                true -> 1 end
    end.

remove_block_prefixes(_BlockOffset, <<>>) ->
    [];
remove_block_prefixes(0, <<_BlockPrefix,Rest/binary>>) ->
    remove_block_prefixes(1, Rest);
remove_block_prefixes(BlockOffset, Bin) ->
    BlockBytesAvailable = ?BLOCK_SIZE - BlockOffset,
    case size(Bin) of
        Size when Size > BlockBytesAvailable ->
            <<DataBlock:BlockBytesAvailable/binary,Rest/binary>> = Bin,
            [DataBlock | remove_block_prefixes(0, Rest)];
        _Size ->
            [Bin]
    end.
