erlang-statd
============

Distributed statistics client/server in erlang.

Usage
-----

### Server

```
rebar -get-deps compile; ./start.sh
```

### Client

```
erl -sname bar
```

```
c(commons), c(stat_client).
gen_server:start_link(stat_client, server@hostname, []).
```

