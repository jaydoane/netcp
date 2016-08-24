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

handle_info(timeout, #state{listen = Listen} = State) ->
    {ok, Socket} = netcp:accept(Listen),
    {ok, _} = netcp_sup:start_child(),
    ok = netcp:recv_file(Socket),
    {stop, normal, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
