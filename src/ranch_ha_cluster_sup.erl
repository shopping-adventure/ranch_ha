%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha_cluster_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

start_link(Ref, Nodes, Opts) ->
    supervisor:start_link({local, Ref}, ?MODULE, [Ref, Nodes, Opts]).


init([Ref, Nodes, Opts]) ->
    SupFlags = #{ strategy => one_for_one,
		  intensity => 1,
		  period => 5 },
    Monitor = #{ id => {monitor, Ref},
		 start => {ranch_ha_monitor, start_link, [Ref, Nodes, Opts]},
		 type => worker },

    {PolicyName, PolicyOpts} = proplists:get_value(policy, Opts, {round_robin, []}),
    Policy = #{ id => {policy, Ref},
		start => {ranch_ha_policy, start_link, [Ref, PolicyName, PolicyOpts]},
		type => worker },
    {ok, {SupFlags, [Monitor, Policy]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
