%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha).

-behaviour(application).

-export([start_cluster/3]).

%% Application callbacks
-export([start/2, stop/1]).

-type cluster() :: atom().
-type opt() :: {refresh, integer()}.    % interval between dead node pinging

-export_type([cluster/0,
	      opt/0]).

-spec start_cluster(Ref :: cluster(),
		    Nodes :: [atom()], 
		    Opts :: [opt()]) ->
			   ok | {error, term()}.
start_cluster(Ref, Nodes, Opts) ->
    {ok, _} = application:ensure_all_started(ranch_ha),
    case ranch_ha_sup:start_monitor(Ref, Nodes, Opts) of
	{error, {already_started, _Pid}} -> ok;
	{error, Err} -> {error, Err};
	{ok, _Pid} -> ok
    end.

%% Application callbacks
start(_StartType, _StartArgs) ->
    ranch_ha_sup:start_link().

stop(_State) ->
    ok.
