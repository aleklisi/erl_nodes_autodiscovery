erl_nodes_autodiscovery
=====

is an application which is demonstrating a simple mechanism of Erlang nodes autodiscovery.
This will be hopefully useful to connect nodes in a local network.

Run 2 (or more, up to 10) consoles side by side and start the project:

```
rebar3 shell --setcookie secret_cookie --name a@127.0.0.1
```

and then setup log level to debug to see what nodes are connected:

```
logger:set_primary_config(level, debug).
```