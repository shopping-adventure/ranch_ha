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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_statem).

%% API
-export([start_link/3,
	 'master?'/1,
	 alive/1,
	 all/1]).

%% gen_statem callbacks
-export([init/1, 
	 callback_mode/0,
	 code_change/4,
	 terminate/3]).

%% State functions
-export([master/3,
	 slave/3]).

-define(DELAY, 2000).
-define(NODES_TID, ranch_ha_monitor_nodes).

%% Nodes: cluster nodes, first is higher priority
start_link(Ref, Nodes, Opts) ->
    Args = [Ref, Nodes, Opts],
    Refresh = proplists:get_value(refresh, Opts, ?DELAY),
    gen_statem:start_link({local, Ref}, ?MODULE, #{ nodes => Nodes, refresh => Refresh, args => Args }, []).


-spec 'master?'(Ref :: ranch_ha:cluster()) -> master | slave.
'master?'(Ref) ->
    gen_statem:call({monitor, Ref}, 'master?').


-spec alive(Ref :: ranch_ha:cluster()) -> [atom()].
alive(Ref) ->
    gen_statem:call({monitor, Ref}, alive).


-spec all(Ref :: ranch_ha:cluster()) -> [{atom(), boolean()}].
all(Ref) ->
    gen_statem:call({monitor, Ref}, all).

%%%
%%% Private
%%%
callback_mode() ->
    state_functions.


init(#{ nodes := [ Master | _ ]=NodesList, refresh := Refresh, args := Args }) ->
    ?debug("Start monitoring nodes: ~p", [NodesList]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    T_Nodes = init_table(NodesList),
    _ = connect(T_Nodes),
    State0 = case node() of 
		 Master -> ?info("Set MASTER mode", []), master; 
		 _ -> ?info("Set SLAVE mode", []), slave
	     end,
    {ok, _} = timer:send_after(Refresh, refresh),
    {ok, State0, #{ nodes => T_Nodes, refresh => Refresh, args => Args }}.


terminate(_Reason, _Data, _State) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


master(info, {nodeup, Node, _Infos}, #{ nodes := T_Nodes, args := Args }=Data) ->
    Priority = priority(Node, T_Nodes),
    ok = nodeup(Node, T_Nodes, Args),
    case priority(node(), T_Nodes) of
	MyPriority when Priority < MyPriority ->
	    ?debug("Set SLAVE mode (MASTER: ~s)", [Node]),
	    {next_state, slave, Data};
	_ ->
	    ?debug("New node up: ~s", [Node]),
	    {keep_state, Data}
    end;

master(info, {nodedown, Node, _Infos}, #{ nodes := T_Nodes }=Data) ->
    ok = nodedown(Node, T_Nodes),
    {keep_state, Data};

master({call, From}, 'master?', Data) ->
    {keep_state, Data, [{reply, From, master}]};

master(Type, Other, Data) ->
    handle_event(Type, Other, Data).


slave(info, {nodeup, Node, _Infos}, #{ nodes := T_Nodes, args := Args }=Data) ->
    ok = nodeup(Node, T_Nodes, Args),
    {next_state, slave, Data};

slave(info, {nodedown, Node, _Infos}, #{ nodes := T_Nodes }=Data) ->
    Priority = priority(Node, T_Nodes),
    ok = nodedown(Node, T_Nodes),
    case priority(node(), T_Nodes) of
	MyPriority when Priority > MyPriority ->
	    ?debug("SLAVE node down ~s", [Node]),
	    {next_state, slave, Data};
	MyPriority when Priority < MyPriority ->
	    case 'status?'(node(), T_Nodes) of
		master ->
		    ?debug("Changing to MASTER mode", []),
		    {next_state, master, Data};
		slave ->
		    ?debug("Salve node going down: ~s", [Node]),
		    {next_state, slave, Data}
	    end
    end;

slave({call, From}, 'master?', Data) ->
    {keep_state, Data, [{reply, From, slave}]};

slave(Type, Other, Data) ->
    handle_event(Type, Other, Data).


%%%===================================================================
%%% Internal functions
%%%===================================================================
init_table(NodesList) ->
    T_Nodes = ets:new(?NODES_TID, [{read_concurrency, true}, ordered_set]),
    _ = lists:foldl(fun (Node, Prio) ->
			    true = ets:insert(T_Nodes, {Prio, Node, node() == Node}),
			    Prio+1
		    end, 0, NodesList),
    T_Nodes.


priority(Node, Tid) -> 
    [ [Prio] ] = ets:match(Tid, {'$1', Node, '_'}),
    Prio.


nodeup(Node, T_Nodes, Args) -> 
    case rpc:call(Node, ranch_ha, start_cluster, Args) of
	{error, Reason} ->
	    ?error("start_cluster failed on node ~s: ~p", [Node, Reason]),
	    [[Prio]] = ets:match(T_Nodes, {'$1', Node, '_'}),
	    true = ets:insert(T_Nodes, {Prio, Node, false});
	ok ->
	    [[Prio]] = ets:match(T_Nodes, {'$1', Node, '_'}),
	    true = ets:insert(T_Nodes, {Prio, Node, true})
    end,
    ok.
	

nodedown(Node, T_Nodes) -> 
    [[Prio]] = ets:match(T_Nodes, {'$1', Node, '_'}),
    true = ets:update_element(T_Nodes, {Prio, Node, false}),
    ok.


'status?'(Node, Nodes) -> 
    'status?'(Node, ets:first(Nodes), Nodes).


'status?'(_Node, '$end_of_table', _) -> slave;
%% No node of higher priority is alive -> master
'status?'(Node, Prio, Nodes) -> 
    case ets:lookup(Nodes, Prio) of
	[{_, Node, true}] -> master;
	[{_, Node, false}] -> slave;
	[{_, _OtherNode, true}] -> slave;
	[{_, _OtherNode, false}] -> 'status?'(Node, ets:next(Nodes, Prio))
    end.


alive_(T_Nodes) ->
    lists:map(fun ([Node]) -> Node end, ets:match(T_Nodes, {'_', '$1', true})).


connect(T_Nodes) -> 
    ets:insert(T_Nodes, lists:map(fun ({Prio, Node, false}) ->
					  {Prio, Node, net_kernel:connect_node(Node)}
				  end, ets:match_object(T_Nodes, {'_', '_', false}))).


handle_event({call, From}, alive, #{ nodes := T_Nodes }=Data) ->
    {keep_state, Data, [{reply, From, alive_(T_Nodes)}]};

handle_event(info, refresh, #{ nodes := T_Nodes, refresh := Delay }=Data) ->
    ok = connect(T_Nodes),
    {ok, _} = timer:send_after(Delay, refresh),
    {keep_state, Data}.
