%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created : 28 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(ranch_ha).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    ranch_ha_sup:start_link().

stop(_State) ->
    ok.
