%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(ranch_ha_policy_random).

-include("ranch_ha.hrl").

-behaviour(ranch_ha_policy).

-export([init/2,
	 next/1,
	 node_change/1]).

-record(state, {
	  cluster :: ranch_ha:cluster(),
	  alive   :: tuple()
	 }).


init(Cluster, _Opts) ->
    Alive = ranch_ha_monitor:alive(Cluster),
    {ok, #state{cluster=Cluster, alive=list_to_tuple(Alive)}}.


next(#state{alive=Alive}=S) ->
    {element(rand:uniform(size(Alive)), Alive), S}.


node_change(#state{cluster=Cluster}=S) ->
    ?debug("Reload alive nodes"),
    {ok, S#state{alive=list_to_tuple(ranch_ha_monitor:alive(Cluster))}}.
