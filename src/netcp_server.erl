-module(netcp_server).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen}).

start_link(Listen) ->
    gen_server:start_link(?MODULE, [Listen], []).

init([Listen]) ->
    {ok, #state{listen = Listen}, _Timeout=0}. % immediately handle_info(timeout, ...

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_cast(stop, State) ->
%%     {stop, normal, State}.

%% handle_info({tcp_closed, Socket}, State) ->
%%     log:info("tcp_closed ~p, stopping", [Socket]),
%%     {stop, normal, State};
handle_info(timeout, #state{listen = Listen} = State) ->
    {ok, Socket} = netcp:accept(Listen),
    netcp_sup:start_child(),
    Transport = netcp:transport(Socket),
    %% {ok, SocketOpts} = netcp:getopts(Socket, [exit_on_close, active]),
    %% io:format("Socket Opts ~p~n", [SocketOpts]),
    Path = unique_path(),
    {ok, Device} = file:open(Path, [write, raw]),

    {Header, ByteCount} = netcp:maybe_recv_header(Socket, Device),
    io:format("Header ~p~n", [Header]),
    ExpectedSize = maps:get(filesize, Header, 0),
    {ok, Size, CheckSum} = netcp:recv(
        Transport, Socket, Device, ExpectedSize, ByteCount, erlang:adler32(<<>>)),

    Response = #{path => Path, size => Size, checksum => CheckSum},
    ok = maybe_respond(Socket, Response, Header),
    ok = file:close(Device),
    ok = Transport:close(Socket),
    io:format("Wrote ~p ~p bytes ~p checksum~n", [Path, Size, CheckSum]),
    {stop, normal, State}.

maybe_respond(_, _, Header) when map_size(Header) == 0 ->
    ok; % no header -> no response
maybe_respond(Socket, Response, _Header) ->
    BinResponse = term_to_binary(Response),
    Transport = netcp:transport(Socket),
    io:format("respond ~p~n", [Response]),
    ok = Transport:send(Socket, BinResponse).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

unique_path() ->
    netcp_app:env(dir, "/tmp/") ++ "1".
        %% ++ integer_to_list(erlang:unique_integer([positive])).
    
