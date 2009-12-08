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

%% N.B. We can't use init_per_suite to share the connection,
%% since init_per_suite and end_per_suite run on different processes.


%% Tests start.
test_connect_disconnect(_Config) ->
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    ok = memcached:disconnect(Conn).


test_connect_error(_Config) ->
    process_flag(trap_exit,true),
    {error, _} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT + 1).


test_set_get(_Config) ->
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_get_not_exist(_Config) ->
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    {ok, not_exist} = memcached:get(Conn, "not-exist"),
    ok = memcached:disconnect(Conn).


test_set_expiry(_Config) ->
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue", 0, 1),
    receive after 1000 -> [] end,
    {ok, not_exist} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).

%% Tests end.
all() ->
    [test_connect_disconnect,
     test_connect_error,
     test_set_get,
     test_get_not_exist,
     test_set_expiry

    ].
