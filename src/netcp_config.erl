-module(netcp_config).

-export([get/2]).

get(Key, Default) ->
    case erlang:function_exported(config, get, 3) of
        true ->
            Val = config:get("netcp", atom_to_list(Key), Default),
            case Val of
                Default ->
                    Default;
                _ ->
                    {ok, Term} = couch_util:parse_term(Val),
                    Term
            end;
        false ->
            application:get_env(netcp, Key, Default)
    end.
