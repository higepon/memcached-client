%%%-------------------------------------------------------------------
%%% File    : memcached_SUITE.erl
%%% Author  : higepon <higepon@users.sourceforge.jp>
%%% Description : Tests for memachached client
%%%
%%% Created :  7 Dec 2009 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------
-module(memcached_SUITE).

-compile(export_all).

-include("ct.hrl").
-define(MEMCACHED_PORT, 11411).

suite() ->
    [{timetrap,{seconds,30}}].


init_per_suite(Config) ->
    process_flag(trap_exit,true),
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    io:format("con0=~p ~n", [Conn]),
    [{connection, Conn}].


end_per_suite(Config) ->
    Conn1 = proplists:get_value(connection, Config),
    io:format("con1=~p  registered~p~n", [Conn1, registered()]),
    {ok, Conn2} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    io:format("con1=~p ~p registered~p~n", [Conn1, Conn2, registered()]),

    ok = memcached:disconnect(Conn2),
    Config.



%% Tests start.
test_set_get(_Config) ->
    ok.


test_connect_error(_Config) ->
    process_flag(trap_exit,true),
    {error, _} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT + 1).


%% Tests end.
all() ->
    [test_set_get
%     connect_error

    ].

