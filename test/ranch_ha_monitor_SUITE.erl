-module(ranch_ha_monitor_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

groups() -> 
    [
     {monitor, [parallel, {repeat, 10}], [single]}
    ].

all() ->
    [ {group, monitor} ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_group, Config) ->
    _ = application:ensure_all_started(ranch_ha),
    Config.

end_per_group(_group, Config) ->
    ok = application:stop(ranch_ha),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%
%% Tests
%%
single(_Config) ->
    {ok, _} = ranch_ha_sup:start_monitor([node()], fun (master) ->
							   io:format("MASTER mode");
						       (slave) -> 
							   io:format("SLAVE mode")
						   end),
    ?assertMatch(master, ranch_ha_monitor:status()).
