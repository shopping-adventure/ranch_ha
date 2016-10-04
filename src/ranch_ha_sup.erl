%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha_sup).

-include("ranch_ha.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_monitor/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


-spec start_monitor(#cluster{}, [atom()], term()) -> {ok, pid()} | {error, term()}.
start_monitor(Cluster, Nodes, Opts) ->
    ClusterSup = #{ id => Cluster#cluster.id,
		    start => {ranch_ha_cluster, start_link, [Cluster, Nodes, Opts]},
		    type => supervisor },
    supervisor:start_child(?SERVER, ClusterSup).

%%%
%%% Private
%%%
init([]) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},
    {ok, {SupFlags, []}}.
