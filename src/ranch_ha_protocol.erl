%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(ranch_ha_protocol).

-include("ranch_ha.hrl").

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
    Local = node(),
    case ranch_ha_policy:next(Cluster) of
	Local -> 
	    Protocol:start_link(Ref, Socket, Transport, ProtocolOptions);
	Node ->
	    {ok, Proxy} = ranch_ha_proxy_transport:start_link(Socket, Transport),
	    rpc:call(Node, Protocol, start_link, 
		     [Ref, Proxy, ranch_ha_proxy_transport, ProtocolOptions])
    end.
