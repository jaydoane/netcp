-module(netcp_couch_file).

-export([block_size/0, transformer/0]).

-define(BLOCK_SIZE, 16#1000). % 4 KiB
-define(UUID_BYTE_SIZE, 16).

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

-record(comp_header, {
    db_header,
    meta_state
}).

block_size() ->
    ?BLOCK_SIZE.

transformer() ->
    NewUuid = uuid(),
    io:format("transformer uuid ~p~n", [NewUuid]),
    fun(Data) ->
        [maybe_replace_header(Block, NewUuid) || Block <- blocks(Data)]
    end.
    
maybe_replace_header(Block, NewUuid) ->
    try
        replace_header(Block, NewUuid)
    catch
        error:{badmatch, _} -> Block
    end.

replace_header(Block, NewUuid) ->
    {Header, NewHeader} = case load_header(Block) of
        #db_header{} = DbHeader ->
            {DbHeader, DbHeader#db_header{uuid = NewUuid}};
        #comp_header{db_header = DbHeader} = CompHeader ->
            {CompHeader, CompHeader#comp_header{
                db_header = DbHeader#db_header{uuid = NewUuid}}}
    end,
    binary:replace(Block, as_stored(Header), as_stored(NewHeader)).

hexify(Bin) ->
    << << if N >= 10 -> N - 10 + $a;
        true         -> N      + $0 end >>
    || <<N:4>> <= Bin >>.

uuid() ->
    hexify(crypto:rand_bytes(?UUID_BYTE_SIZE)).

header_offsets(Data) ->
    [?BLOCK_SIZE * Index || Index <- lists:seq(0, size(Data) div ?BLOCK_SIZE)].

blocks(Data) ->
    [part(Data, Offset) || Offset <- header_offsets(Data)].

part(Data, Offset) ->
    binary:part(Data, Offset, read_size(Data, Offset)).

read_size(Data, Offset) ->
    min(size(Data), Offset + ?BLOCK_SIZE) - Offset.

as_stored(Header) ->
    HeaderBin = term_to_binary(Header),
    Hash = crypto:hash(md5, HeaderBin),
    HashHeaderBin = <<Hash/binary, HeaderBin/binary>>,
    EncodedSize = binary:encode_unsigned(size(HashHeaderBin)),
    SizeBin = netcp:left_pad(EncodedSize, 4),
    <<1, SizeBin/binary, HashHeaderBin/binary>>.

load_header(Data) ->
    load_header(Data, 0).
load_header(Data, Pos) ->
    PrefixSize = 5, % 1 (version) + 4 (size) bytes
    <<1, HeaderSize:32/integer-unsigned, RestBlock/binary>> =
        binary:part(Data, Pos, read_size(Data, Pos)),
    TotalSize = total_read_size(PrefixSize, HeaderSize),
    RawBin = raw_bin(TotalSize, RestBlock, Data, Pos + PrefixSize),
    <<Hash:16/binary, HeaderBin/binary>> =
        iolist_to_binary(remove_block_prefixes(PrefixSize, RawBin)),
    Hash = crypto:hash(md5, HeaderBin),
    binary_to_term(HeaderBin).

raw_bin(TotalSize, RestBlock, Data, Pos) when TotalSize =< size(RestBlock) ->
    <<RawBin:TotalSize/binary>> = binary:part(Data, Pos, TotalSize),
    RawBin;
raw_bin(_TotalSize, _RestBlock, _Data, _Pos) ->
    erlang:error(large_header). % FIXME?

total_read_size(0, FinalSize) ->
    total_read_size(1, FinalSize) + 1;
total_read_size(BlockOffset, FinalSize) ->
    case ?BLOCK_SIZE - BlockOffset of
        BlockLeft when BlockLeft >= FinalSize ->
            FinalSize;
        BlockLeft ->
            FinalSize + ((FinalSize - BlockLeft) div (?BLOCK_SIZE - 1)) +
            if ((FinalSize - BlockLeft) rem (?BLOCK_SIZE - 1)) =:= 0 -> 0;
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
