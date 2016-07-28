-module(netcp_log).

-export([
    debug/2, 
    info/2]).

debug(Format, Data) ->
    maybe_couch_log(debug, Format, Data).

info(Format, Data) ->
    maybe_couch_log(info, Format, Data).

maybe_couch_log(Function, Format, Data) ->
    case erlang:function_exported(couch_log, Function, 2) of
        true ->
            couch_log:Function(Format, Data);
        false ->
            io:format(Format ++ "~n", Data)
    end.
