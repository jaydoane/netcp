-define(DEFAULT_TRANSPORT, ssl).
-define(DEFAULT_PORT, 11011).

-define(MAGIC, <<"«º«º«º«º">>).

-define(BASE_TCP_OPTS, [
    binary, {packet, 0}, {active, false}, {reuseaddr, true}, {exit_on_close, false}]).
-define(RECV_TIMEOUT, 1000). % ms
-define(RESPONSE_TIMEOUT, 5000). % ms

-define(DEFAULT_RECBUF_SIZE, 16*1024*1024).
-define(DEFAULT_FILE_READ_BUF_SIZE, 16*1024*1024).
-define(BITS_PER_BYTE, 8).
-define(ENCODED_SIZE, 4).

-define(DEFAULT_CERTFILE,
        filename:join([os:getenv("HOME"), ".certs", "certificate.pem"])).
-define(DEFAULT_KEYFILE,
        filename:join([os:getenv("HOME"), ".certs", "key.pem"])).
-define(DEFAULT_CIPHERS, [
    {rsa,aes_128_cbc,sha},
    {rsa,rc4_128,md5}]).

