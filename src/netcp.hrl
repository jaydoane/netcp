-define(DEFAULT_TRANSPORT, ssl).
-define(DEFAULT_PORT, 11011).

-define(MAGIC, <<"«º«º«º«º">>).

-define(BASE_TCP_OPTS, [
    binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(RECV_TIMEOUT, 1000). % ms
-define(RESPONSE_TIMEOUT, 5000). % ms
-define(DEFAULT_CONNECT_TIMEOUT, 5000). % ms

-define(DEFAULT_RECBUF_SIZE, 4096*4096).
-define(DEFAULT_READ_BUF_BLOCK_COUNT, 4096).
-define(DEFAULT_BLOCK_SIZE, 4096).
-define(BITS_PER_BYTE, 8).
-define(ENCODED_SIZE, 4).

-define(DEFAULT_CERTFILE,
    filename:join([os:getenv("HOME"), ".netcp", "cert.pem"])).
-define(DEFAULT_KEYFILE,
    filename:join([os:getenv("HOME"), ".netcp", "key.pem"])).
-define(DEFAULT_CIPHERS, [
    {rsa,aes_128_gcm,null,sha256},
    {rsa,aes_128_cbc,sha}]).

