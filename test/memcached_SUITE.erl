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
-define(MEMCACHED_HOST, "127.0.0.1").

suite() ->
    [{timetrap,{seconds,30}}].

%% N.B. We can't use init_per_suite to share the connection,
%% since init_per_suite and end_per_suite run on different processes.

%% Tests start.
test_connect_disconnect(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:disconnect(Conn).


test_connect_error(_Config) ->
    process_flag(trap_exit,true),
    {error, _} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT + 1).


test_set_get(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_get_not_exist(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    {error, not_found} = memcached:get(Conn, "not-exist"),
    ok = memcached:disconnect(Conn).


test_set_expiry(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue", 0, 1),
    receive after 1000 -> [] end,
    {error, not_found} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_delete(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:delete(Conn, "mykey"),
    {error, not_found} = memcached:delete(Conn, "not_found"),
    {error, not_found} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_delete_with_time(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:delete(Conn, "mykey", 10),
    {error, not_found} = memcached:get(Conn, "mykey"),

    %% Even with time option, set command will succeed.
    ok = memcached:set(Conn, "mykey", "myvalue"),

    ok = memcached:disconnect(Conn).


test_replace(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    ok = memcached:replace(Conn, "mykey", "mynewvalue"),
    {ok, "mynewvalue"} = memcached:get(Conn, "mykey"),

    %% key not_found doesn't exist, so replace never happen.
    {error, not_stored} = memcached:replace(Conn, "not_found", "newvalue", 0, 0),
    ok = memcached:disconnect(Conn).


test_get_multi(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:set(Conn, "mykey1", "myvalue1"),
    ok = memcached:set(Conn, "mykey3", "myvalue3"),
    {ok, [{"mykey1", "myvalue1"},
          {"mykey3", "myvalue3"}]}
        = memcached:get_multi(Conn, ["mykey1", "mykey2", "mykey3"]),
    ok = memcached:disconnect(Conn).


%% Tests end.
all() ->
    [test_connect_disconnect,
     test_connect_error,
     test_set_get,
     test_get_not_exist,
     test_set_expiry,
     test_delete,
     test_delete_with_time,
     test_replace,
     test_get_multi
    ].
