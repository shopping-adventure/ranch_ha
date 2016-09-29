%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2016, Jean Parpaillon
%%% @doc High-Availability TCP ranch protocol
%%%
%%% @end
%%% Created : 27 Sep 2016 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(ranch_ha_tcp).

-behaviour(ranch_transport).

%%% ranch_transport callbacks
-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([accept/2]).
-export([accept_ack/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).

-type opts() :: [ {ha_if, string()}
                  | ranch_tcp:opts() ].
-export_type([opts/0]).

name() -> ha_tcp.


secure() -> 
    ranch_tcp:secure().


messages() -> 
    ranch_tcp:messages().


listen(Opts) ->
    HaOpts = ranch:filter_options(Opts, [ha_nodes], [{ha_nodes, []}]),
    {ok, _} = ranch_ha_sup:start_monitor(proplists:get_value(ha_nodes, HaOpts, [])),
    ranch_tcp:listen(Opts).


accept(LSocket, Timeout) ->
    ranch_tcp:accept(LSocket, Timeout).


accept_ack(S, T) ->
    ranch_tcp:accept_ack(S, T).


connect(Host, Port, Opts) ->
    ranch_tcp:connect(Host, Port, Opts).


connect(Host, Port, Opts, Timeout) ->
    ranch_tcp:connect(Host, Port, Opts, Timeout).


recv(Socket, Length, Timeout) ->
    ranch_tcp:recv(Socket, Length, Timeout).


send(Socket, Packet) ->
    ranch_tcp:send(Socket, Packet).


sendfile(Socket, Filename) ->
    ranch_tcp:sendfile(Socket, Filename).


sendfile(Socket, File, Offset, Bytes) ->
    ranch_tcp:sendfile(Socket, File, Offset, Bytes).


sendfile(Socket, Filename, Offset, Bytes, Opts) ->
    ranch_tcp:sendfile(Socket, Filename, Offset, Bytes, Opts).


setopts(Socket, Opts) ->
    ranch_tcp:setopts(Socket, Opts).


controlling_process(Socket, Pid) ->
    ranch_tcp:controlling_process(Socket, Pid).


peername(Socket) ->
    ranch_tcp:peername(Socket).


sockname(Socket) ->
    ranch_tcp:sockname(Socket).


shutdown(Socket, How) ->
    ranch_tcp:shutdown(Socket, How).


close(Socket) ->
    ranch_tcp:close(Socket).
