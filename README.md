erlang-statd
============

Distributed statistics client/server in erlang.

Usage
-----

### Server

```
erl -sname foo
```

```
c(stat_server).
stat_server:start_server().
```

### Client

```
erl -sname bar
```

```
c(commons), c(stat_client).
gen_server:start_link(stat_client, foo@hostname, []).
```

