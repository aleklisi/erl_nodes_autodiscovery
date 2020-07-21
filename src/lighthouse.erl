-module(lighthouse).

-define(REPEAT_MSG, resend_the_search_broadcast).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts = #{ports := Ports, repeat_after := RepeatAfter}) ->
    erlang:send_after(RepeatAfter, self(), ?REPEAT_MSG),
    {socket, Port, Socket} = maybe_start_udp(Ports),
    {ok, Opts#{socket => Socket, port => Port}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?REPEAT_MSG, State = #{repeat_after := RepeatAfter, socket := Socket, ports := Ports}) ->
    {ok, NetConfigs} = inet:getifaddrs(),
    IPAddresses = lists:foldl(fun get_broadcast_addresses/2, [], NetConfigs),
    lists:map(fun(IP) -> broadcast_my_node_name(Socket, IP, Ports) end, IPAddresses),
    erlang:send_after(RepeatAfter, self(), ?REPEAT_MSG),
    {noreply, State};
handle_info({udp,_,_,_,BinNodeName}, State) ->
    NodeName = binary_to_atom(BinNodeName, utf8),
    net_adm:ping(NodeName),
    logger:info("Current node ~p is connected to ~p nodes",[node(), nodes()]),
    {noreply, State};
handle_info(Msg, State) ->
    logger:error("Unexpected message ~p ~p",[?MODULE, Msg]),
    {noreply, State}.

get_broadcast_addresses(NetConfig, AlreadyFoundAddresses) ->
    case get_broadcast_address(NetConfig) of
        none -> AlreadyFoundAddresses;
        Address -> [Address | AlreadyFoundAddresses]
    end.

get_broadcast_address({_NetName, Opts}) ->
    proplists:get_value(broadaddr, Opts, none).

broadcast_my_node_name(Socket, BroadcastIPAddress, Ports) ->
    BinaryNodeName = atom_to_binary(node(), utf8),
    lists:map(fun(Port) ->
        gen_udp:send(Socket, {BroadcastIPAddress, Port}, BinaryNodeName)
    end, Ports).


maybe_start_udp(Ports) ->
    lists:foldl( fun maybe_open_udp_port/2, empty, Ports).

maybe_open_udp_port(_, Acc = {socket, _Port, _Socket}) -> Acc;
maybe_open_udp_port(Port, empty) ->
    SocketConfig = [
        binary
        , {active,true}
        , {broadcast, true}
    ],
    case gen_udp:open(Port, SocketConfig) of
        {ok, Socket} ->
            logger:info("Opening the socket on a port ~p", [Port]),
            {socket, Port, Socket};
        _ -> empty
    end.