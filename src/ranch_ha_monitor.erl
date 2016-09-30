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
-export([start_link/2,
	 status/0]).

%% gen_statem callbacks
-export([init/1, 
	 callback_mode/0,
	 code_change/4,
	 terminate/3]).

%% State functions
-export([master/3,
	 slave/3]).

-define(SERVER, ?MODULE).

%% Nodes: cluster nodes, first is higher priority
%% Fun: called when node becomes slave or master
start_link(Nodes, Fun) ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, #{ nodes => Nodes, f => Fun }, []).


-spec status() -> master | slave.
status() ->
    gen_statem:call(?SERVER, status).

%%%
%%% Private
%%%
callback_mode() ->
    state_functions.


init(#{ nodes := [ Master | _ ]=Nodes, f := Fun }) ->
    ?debug("Start monitoring nodes: ~p", [Nodes]),
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
    Data = lists:map(fun (Node) ->
			     Connect = net_kernel:connect_node(Node),
			     {Node, Connect}
		     end, Nodes),
    State0 = case node() of Master -> master; _ -> slave end,
    ok = Fun(State0),
    {ok, State0, #{ nodes => Data, f => Fun }}.


terminate(_Reason, _Data, _State) ->
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


master(info, {nodeup, Node, _Infos}, #{ nodes := Nodes0, f := Fun }=Data0) ->
    Priority = priority(Node, Nodes0),
    Data = Data0#{ nodes => status(Node, true, Priority) },
    case priority(node(), Nodes0) of
	MyPriority when Priority > MyPriority ->
	    ?debug("Changing to SLAVE mode (MASTER: ~s)", [Node]),
	    ok = Fun(slave),
	    {next_state, slave, Data};
	_ ->
	    ?debug("New node up, with lower priority: ~s", [Node]),
	    {keep_state, Data}
    end;

master(info, {nodedown, Node, _Infos}, #{ nodes := Nodes }=Data) ->
    {keep_state, Data#{ nodes := status(Node, false, Nodes)}};

master({call, From}, status, Data) ->
    {keep_state, Data, [{reply, From, master}]}.


slave(info, {nodeup, Node, _Infos}, #{ nodes := Nodes }=Data) ->
    {next_state, slave, Data#{ nodes := status(Node, true, Nodes)}};

slave(info, {nodedown, Node, _Infos}, #{ nodes := Nodes0, f := Fun }=Data0) ->
    Priority = priority(Node, Nodes0),
    Nodes = status(Node, false, Nodes0),
    Data = Data0#{ nodes := Nodes},
    case priority(node(), Nodes0) of
	MyPriority when Priority < MyPriority ->
	    ?debug("SLAVE node down ~s", [Node]),
	    {next_state, slave, Data};
	MyPriority when Priority > MyPriority ->
	    case 'status?'(node(), Nodes) of
		master ->
		    ?debug("Changing to MASTER mode", []),
		    ok = Fun(master),
		    {next_state, master, Data};
		slave ->
		    ?debug("Salve node going down: ~s", [Node]),
		    {next_state, slave, Data}
	    end
    end;

slave({call, From}, status, Data) ->
    {keep_state, Data, [{reply, From, slave}]}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
priority(Node, Nodes) -> priority(Node, 0, Nodes).

priority(_, _, []) -> throw(not_found);
priority(Node, I, [ {Node, _} | _Nodes ]) -> I;
priority(Node, I, [ {_Node2, _} | Nodes]) -> priority(Node, I-1, Nodes).


status(Node, Status, Nodes) -> lists:keyreplace(Node, 1, Nodes, {Node, Status}).


'status?'(_Node, []) -> slave;
%% No node of higher priority is alive -> master
'status?'(Node, [{Node, true} | _Nodes]) -> master;
%% Looked up node is down
'status?'(Node, [{Node, false} | _Nodes]) -> slave;
%% Node2 is alive and of higher priority -> slave
'status?'(_Node1, [{_Node2, true} | _Nodes]) -> slave;
%% Node2 is not alive and of higher priority -> recurse
'status?'(Node, [{_Node2, false} | Nodes]) -> 'status?'(Node, Nodes).


%%%
%%% eunit
%%%
-ifdef(TEST).
'status?_test_'() ->
    [
     ?_assertMatch(slave, 
		  'status?'(node1, [{nodeA, false}, {nodeB, false}])),
     ?_assertMatch(slave, 
		  'status?'(node1, [{nodeA, false}, {nodeB, true}])),
     ?_assertMatch(master,
		   'status?'(node1, [{node1, true}, {node2, true}, {node3, true}])),
     ?_assertMatch(slave,
		   'status?'(node2, [{node1, true}, {node2, true}, {node3, true}])),
     ?_assertMatch(master,
		   'status?'(node2, [{node1, false}, {node2, true}, {node3, true}])),
     ?_assertMatch(slave,
		   'status?'(node2, [{node1, false}, {node2, false}, {node3, true}]))
    ].
-endif.
