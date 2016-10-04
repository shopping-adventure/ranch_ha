%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  3 Oct 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(ranch_ha_policy_round_robin).

-include("ranch_ha.hrl").

-behaviour(ranch_ha_policy).

-export([init/2,
	 next/1,
	 node_change/1]).

-record(state, {
	  cluster :: ranch_ha:cluster(),
	  alive   :: [atom()],
	  next    :: [atom()]
	 }).


init(Cluster, _Opts) ->
    Alive = ranch_ha_monitor:alive(Cluster),
    {ok, #state{cluster=Cluster, alive=Alive, next=Alive}}.


next(#state{next=[], alive=Alive}=S) ->
    {hd(Alive), S#state{next=tl(Alive)}};

next(#state{next=Next}=S) ->
    {hd(Next), S#state{next=tl(Next)}}.


node_change(#state{cluster=Cluster}=S) ->
    ?debug("Reload alive nodes"),
    Alive = ranch_ha_monitor:alive(Cluster),
    {ok, S#state{alive=Alive, next=Alive}}.
