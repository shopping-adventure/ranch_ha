-module(cluster_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("ranch_ha_tests.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    [ 
      cluster
    ].

init_per_suite(Config) ->
    {ok, {Nodes, Slaves}} = start_slaves(4),
    [ {nodes, Nodes}, {slaves, Slaves} | Config ].

end_per_suite(Config) ->
    ok = stop_slaves(?config(slaves, Config)),
    ok.

init_per_group(_group, Config) ->
    Config.

end_per_group(_group, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%
%% Tests
%%
cluster(Config) ->
    Nodes = ?config(nodes, Config),
    ct_rpc:app_node(ranch_ha, Nodes),
    F = fun (master) -> ?log("MASTER mode", []); (slave) -> ?log("SLAVE mode", []) end,
    lists:foreach(fun (Node) ->
			  ct_rpc:call(Node, ranch_ha_sup, start_monitor, [Nodes, F])
		  end, Nodes),
    timer:sleep(1),

    Master0 = hd(Nodes),
    Slaves0 = tl(Nodes),
    ?assertMatch(master, ct_rpc:call(Master0, ranch_ha_monitor, status, [])),
    lists:foreach(fun (Node) ->
			  ?assertMatch(slave, ct_rpc:call(Node, ranch_ha_monitor, status, []))
		  end, Slaves0),
    
    %% Kill MASTER
    ct_rpc:call(Master0, erlang, halt, [0]),
    timer:sleep(1),

    Master1 = hd(Slaves0),
    Slaves1 = tl(Slaves0),
    ?assertMatch(master, ct_rpc:call(Master1, ranch_ha_monitor, status, [])),
    lists:foreach(fun (Node) ->
			  ?assertMatch(slave, ct_rpc:call(Node, ranch_ha_monitor, status, []))
		  end, Slaves1),
    
    %% Restart MASTER
    start_slave(1),
    init_slave(Master0, Nodes),
    timer:sleep(1),

    ?assertMatch(master, ct_rpc:call(Master0, ranch_ha_monitor, status, [])),
    lists:foreach(fun (Node) ->
			  ?assertMatch(slave, ct_rpc:call(Node, ranch_ha_monitor, status, []))
		  end, Slaves0).


%%%
%%% Private
%%%
start_slaves(I) ->
    start_slaves(lists:seq(1, I), [], []).

start_slaves([], Nodes0, Slaves0) ->
    Nodes = lists:reverse(Nodes0), 
    Slaves = lists:reverse(Slaves0),
    lists:foreach(fun (Node) ->
			  init_slave(Node, Nodes)
		  end, Nodes),
    {ok, {Nodes, Slaves}};

start_slaves([ I | Tail ], Nodes, Slaves) ->
    case start_slave(I) of
	{ok, Node, Slave} -> start_slaves(Tail, [ Node | Nodes ], [ Slave | Slaves]);
	{error, Reason} -> {error, Reason}
    end.

start_slave(I) ->	    
    Slave = list_to_atom("ct" ++ integer_to_list(I)),
    ?log("Starting node: ~s", [Slave]),
    Funs = [
	    {code, set_path, [code:get_path()]},
	    {application, ensure_all_started, [ranch_ha]},
	    {application, ensure_all_started, [common_test]}
	   ],
    Opts = [{monitor_master, true}, {startup_functions, Funs}],
    case ct_slave:start(Slave, Opts) of
	{ok, Node} ->  {ok, Node, Slave};
	{error, Reason, Node} -> 
	    ?error("Error starting ~s: ~p", [Node, Reason]),
	    {error, Reason}
    end.


init_slave(Node, Nodes) ->
    F = fun (master) -> ?log("MASTER mode", []); (slave) -> ?log("SLAVE mode", []) end,
    ct_rpc:call(Node, ranch_ha_sup, start_monitor, [Nodes, F]).


stop_slaves([]) ->
    ok;
stop_slaves([ Slave | Slaves ]) ->
    ?log("Stopping node ~s", [Slave]),
    case ct_slave:stop(Slave) of
	{ok, _} ->
	    stop_slaves(Slaves);
	{error, Reason, _} ->
	    ?log("(ignore) ~p", [Reason]),
	    stop_slaves(Slaves)
    end.

