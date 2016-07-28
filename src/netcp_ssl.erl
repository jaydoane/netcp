-module(netcp_ssl).

-export([maybe_start_ssl/1, maybe_generate_default_cert/1]).

-include("netcp.hrl").

maybe_start_ssl(Opts) ->
    case proplists:get_value(ciphers, Opts) of
        undefined ->
            ok;
        _ ->
            case ssl:start() of
                ok -> ok;
                {error,{already_started,ssl}} -> ok
            end
    end.

maybe_generate_default_cert(Opts) ->
    case proplists:get_value(certfile, Opts) of
        undefined ->
            ok;
        Path ->
            case {Path =:= ?DEFAULT_CERTFILE, filelib:is_file(Path)} of
                {true, false} ->
                    ok = generate_cert();
                _ ->
                    ok
            end
    end.
                    
generate_cert() ->
    generate_cert(?DEFAULT_KEYFILE, ?DEFAULT_CERTFILE).

generate_cert(Keyfile, Certfile) ->
    [ok = filelib:ensure_dir(Path) || Path <- [Keyfile, Certfile]],
    Cmd = io_lib:format(
        "openssl req -x509 -nodes -days 3650 "
        "-subj '/C=US/ST=CA/O=NetCP Inc/CN=netcp.example.com' "
        "-newkey rsa:2048 -keyout ~s -out ~s", [Keyfile, Certfile]),
    Output = os:cmd(Cmd),
    ok = io:format(Output).

