%%%-------------------------------------------------------------------
%% @doc erl_nodes_autodiscovery public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_nodes_autodiscovery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    PortsList = lists:seq(12345, 12355),
    set_random_node_name(),
    erl_nodes_autodiscovery_sup:start_link(PortsList).

stop(_State) ->
    ok.

%% internal functions

set_random_node_name() ->
    net_kernel:stop(),
    NodeName = build_randomized_node_name(),
    net_kernel:start([NodeName, longnames]),
    NodeName.

build_randomized_node_name() ->
    Name = random_name(10),
    IP = {A, B, C, D} = local_ip_v4(),
    logger:info("My IP address is ~p", [IP]),
    StrNodeName = lists:flatten(io_lib:format("~s@~p.~p.~p.~p", [Name, A, B, C, D])),
    list_to_atom(StrNodeName).

local_ip_v4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([
         Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
         size(Addr) == 4, Addr =/= {127,0,0,1}
    ]).

random_name(Len) ->
    [rand_char() || _ <- lists:seq(1, Len)].

rand_char() ->
    X = "abcdefghijklmnopqrstuvwxyz",
    lists:nth(rand:uniform(length(X)), X).
