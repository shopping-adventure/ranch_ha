%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha).

-include("ranch_ha.hrl").

-behaviour(application).

-export([new/1,
	 start_cluster/3]).

%% Application callbacks
-export([start/2, stop/1]).

-type opt() :: {refresh, integer()}
	     | {policy, ranch_ha_policy:t()}.

-export_type([opt/0]).

-spec new(Ref :: atom()) -> #cluster{}.
new(Ref) ->
    #cluster{
       id=Ref,
       monitor=?monitor_id(Ref), 
       policy=?policy_id(Ref)}.
    

-spec start_cluster(Cluster :: #cluster{},
		    Nodes :: [atom()], 
		    Opts :: [opt()]) ->
			   {ok, #cluster{}} | {error, term()}.
start_cluster(Cluster, Nodes, Opts) ->
    case ranch_ha_sup:start_monitor(Cluster, Nodes, Opts) of
	{error, {already_started, Pid}} -> {ok, Pid};
	{error, Err} -> {error, Err};
	{ok, Pid} -> {ok, Pid}
    end.

%% Application callbacks
start(_StartType, _StartArgs) ->
    ranch_ha_sup:start_link().

stop(_State) ->
    ok.
