%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(ranch_ha_protocol).

-behaviour(ranch_protocol).

-export([start_link/4]).

-type opt() :: undefined.

-export_type([opt/0]).

-spec start_link(Ref :: ranch:ref(),
		 Socket :: any(),
		 Transport :: module(),
		 ProtocolOptions :: any()) ->
			{ok, ConnectionPid :: pid()} | {ok, SupPid :: pid(), ConnectionPid :: pid()}.
start_link(Ref, Socket, Transport, ProtocolOptions) ->
    {Cluster, Protocol, _Opts} = proplists:get_value(ha, ProtocolOptions),
    Node = ranch_ha_policy:next(Cluster),
    rpc:call(Node, Protocol, start_link, [Ref, Socket, Transport, ProtocolOptions]).
