# netcp

Copy files between Erlang nodes using either plain TCP or TLS
sockets.

## build

Netcp is a pure OTP application built with rebar3 (but should also
build under rebar2). It currently has no dependencies, and has been
tested under OTP 18.3 and 19.0-rc2.

```bash
git clone https://github.com/jaydoane/netcp.git
cd netcp
rebar3 compile
```

## run

Start an Erlang shell on both hosts:
```bash
erl -pa _build/default/lib/netcp/ebin
```

On the receiving host:
```erlang
> netcp:start().
```

On the sending host:
```erlang
> netcp:sendfile(ssl, "db2.testy012.cloudant.net", 11011, "/srv/db/shards/60000000-7fffffff/testy012-user1/tdb100m1k_4.1461917292.couch", [{transform,netcp_couch_file}, {path, <<"/srv/jdoane/tdb100m1k_4.1461917292.couch">>},{ciphers, [{rsa,aes_128_gcm,null,sha256}]}]).
{ok,11291828508,4037410518,58.73471022859291,
    #{checksum => 4037410518,
      path => <<"/srv/jdoane/tdb100m1k_4.1461917292.couch">>,
      size => 11291828508}}
```
The output is in the form `{ok, ByteCount, CheckSum, Rate(MB/sec),
Response}`, where the Response is from the receiving node.
