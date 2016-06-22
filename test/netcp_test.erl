-module(netcp_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_PORT, 12121).

setup() ->
    ok = application:set_env(netcp, port, ?TEST_PORT),
    ok = netcp:start().

teardown() ->
    ok = netcp:stop().

golden_path_test() ->
    Source = "test/fixtures/sample.couch",
    Target = "/tmp/sample.couch",
    true = filelib:is_file(Source),
    false = filelib:is_file(Target),
    ok = setup(),
    {ok, _, _, _, _} = netcp:sendfile(
        ssl, "localhost", ?TEST_PORT, Source,
        [{transform,netcp_couch_file}, {path, Target}]),
    true = filelib:is_file(Target),
    ok = file:delete(Target),
    ok = teardown().
