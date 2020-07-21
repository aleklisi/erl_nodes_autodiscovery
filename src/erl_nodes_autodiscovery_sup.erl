%%%-------------------------------------------------------------------
%% @doc erl_nodes_autodiscovery top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_nodes_autodiscovery_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(PortsList) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PortsList]).

init([PortsList]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        #{
            id => lighthouse,
            start => {
                lighthouse,
                start_link,
                [#{ports => PortsList, repeat_after => 5000}]
            }
        }
        ],
    {ok, {SupFlags, ChildSpecs}}.
