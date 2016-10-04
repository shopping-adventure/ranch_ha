%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha_cluster).

-include("ranch_ha.hrl").

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).


-spec start_link(#cluster{}, [node()], term()) -> {ok, pid()} | {error, term()}.
start_link(Cluster, Nodes, Opts) ->
    supervisor:start_link(?MODULE, [Cluster, Nodes, Opts]).

%%%
%%% Private
%%%
init([Cluster, Nodes, Opts]) ->
    SupFlags = #{ strategy => one_for_one,
		  intensity => 1,
		  period => 5 },

    Monitor = #{ id => Cluster#cluster.monitor,
		 start => {ranch_ha_monitor, start_link, [Cluster, Nodes, Opts]},
		 type => worker },

    {PolicyName, PolicyOpts} = proplists:get_value(policy, Opts, {round_robin, []}),
    Policy = #{ id => Cluster#cluster.policy,
		start => {ranch_ha_policy, start_link, [Cluster, PolicyName, PolicyOpts]},
		type => worker },
    {ok, {SupFlags, [Monitor, Policy]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
