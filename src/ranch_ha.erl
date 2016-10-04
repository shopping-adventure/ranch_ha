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

-export([start_cluster/3]).

%% Application callbacks
-export([start/2, stop/1]).

-type opt() :: {refresh, integer()}.    % interval between dead node pinging

-export_type([opt/0]).

-spec start_cluster(Ref :: atom(),
		    Nodes :: [atom()], 
		    Opts :: [opt()]) ->
			   {ok, #cluster{}} | {error, term()}.
start_cluster(Ref, Nodes, Opts) ->
    {ok, _} = application:ensure_all_started(ranch_ha),
    Cluster = #cluster{
		 id=Ref,
		 monitor=?monitor_id(Ref), 
		 policy=?policy_id(Ref)},
    case ranch_ha_sup:start_monitor(Cluster, Nodes, Opts) of
	{error, {already_started, _Pid}} -> {ok, Cluster};
	{error, Err} -> {error, Err};
	{ok, _Pid} -> {ok, Cluster}
    end.

%% Application callbacks
start(_StartType, _StartArgs) ->
    ranch_ha_sup:start_link().

stop(_State) ->
    ok.
