%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha_monitor).

-include("ranch_ha.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1,
	 status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


start_link(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #{ nodes => Nodes }, []).


status() ->
    gen_server:call(?SERVER, status).

%%%
%%% Private
%%%
init(#{ nodes := Nodes }) ->
    ?debug("Start monitoring nodes: ~p", [Nodes]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    State = lists:foldl(fun (Node, Acc) ->
				Connect = net_kernel:connect_node(Node),
				maps:put(Node, Connect, Acc) 
			end, #{}, Nodes),
    {ok, State}.


handle_call(status, _From, State) ->
    {reply, State, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({nodeup, Node, Infos}, Nodes) ->
    ?debug("Node ~s up: ~p", [Node, Infos]),
    {noreply, Nodes#{ Node => true }};

handle_info({nodeup, Node}, Nodes) ->
    ?debug("Node ~s up", [Node]),
    {noreply, Nodes#{ Node => true }};

handle_info({nodedown, Node, Infos}, Nodes) ->
    ?debug("Node ~s down: ~p", [Node, Infos]),
    {noreply, Nodes#{ Node => false }};

handle_info({nodedown, Node}, Nodes) ->
    ?debug("Node ~s down", [Node]),
    {noreply, Nodes#{ Node => false }}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
