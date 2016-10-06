%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2016 Jean Parpaillon.
-module(ranch_ha_ip).

-include("ranch_ha.hrl").

%% API
-export([addifaddr/3,
	 delifaddr/3]).

-spec addifaddr(Netif :: string(), Ip :: inet:ip_address(), Netmask :: inet:ip_address()) ->
		       ok | {error, term()}.
addifaddr(Netif, Ip, Netmask) when is_list(Netif) ->
    Cmd = io_lib:format("LANG=C /sbin/ip address add ~s/~s dev ~s", [Netif, 
								     inet:ntoa(Ip),
								     inet:ntoa(Netmask)]),
    case os:cmd(Cmd) of
	[] -> ok;
	["RTNETLINK answers: Operation not permitted" | _] ->
	    ?debug("Check you have capability CAP_NET_ADMIN", []),
	    {error, eperm};
	["RTNETLINK answers: Operation not supported" | _] ->
	    {error, eafnosupport};
	["RTNETLINK answers: File exists" | _] ->
	    {error, eexist};
	["Cannot find device" | _] ->
	    {error, enodev};
	Other ->
	    {error, Other}
    end.


-spec delifaddr(Netif :: string(), Ip :: inet:ntoa(), Netmask :: inet:ntoa()) ->
		       ok | {error, term()}.
delifaddr(Netif, Ip, Netmask) ->
    Cmd = io_lib:format("LANG=C /sbin/ip address del ~s/~s dev ~s", [Netif, 
								     inet:ntoa(Ip),
								     inet:ntoa(Netmask)]),
    case os:cmd(Cmd) of
	[] -> ok;
	["RTNETLINK answers: Cannot assign requested address" | _] ->
	    {error, enoent};
	["RTNETLINK answers: Operation not permitted" | _] ->
	    ?debug("Check you have capability CAP_NET_ADMIN", []),
	    {error, eperm};
	["RTNETLINK answers: Operation not supported" | _] ->
	    {error, eafnosupport};
	["RTNETLINK answers: File exists" | _] ->
	    {error, eexist};
	["Cannot find device" | _] ->
	    {error, enodev};
	Other ->
	    {error, Other}
    end.
