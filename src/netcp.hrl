-define(DEFAULT_TRANSPORT, gen_tcp).
-define(DEFAULT_PORT, 11011).

-define(BASE_TCP_OPTS, [
    binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(DEFAULT_RECBUF_SIZE, 16*1024*1024).
-define(DEFAULT_FILE_READ_BUF_SIZE, 16*1024*1024).

-define(DEFAULT_CERTFILE, "/tmp/certificate.pem").
-define(DEFAULT_KEYFILE, "/tmp/key.pem").
-define(DEFAULT_CIPHERS, [
    {rsa,aes_128_cbc,sha},
    {rsa,rc4_128,md5}]).

