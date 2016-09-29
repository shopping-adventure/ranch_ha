%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_monitor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_monitor(Nodes) ->
    supervisor:start_child(?SERVER, [Nodes]).

%%%
%%% Private
%%%
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
		 intensity => 1,
		 period => 5},
    Monitor = #{ id => ranch_ha_monitor,
		 start => {ranch_ha_monitor, start_link, []},
		 type => worker },
    {ok, {SupFlags, [Monitor]}}.
