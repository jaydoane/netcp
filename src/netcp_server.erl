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
    File = unique_name(),
    {ok, Device} = file:open(File, [write, raw]),
    Transport = netcp:transport(Socket),

    {ok, ByteCount, CheckSum} = 
        netcp:recv(Transport, Socket, Device, 0, erlang:adler32(<<>>)),

    ok = file:close(Device),
    ok = Transport:close(Socket),
    io:format("Wrote ~p with ~p bytes ~p checksum~n", [File, ByteCount, CheckSum]),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

unique_name() ->
    "/tmp/" ++ integer_to_list(erlang:unique_integer([positive])).
    
