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


test_setb_getb(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:setb(Conn, "bin_key", <<10:64/little>>),
    io:format("aval=~p~n", [<<10:64/little>>]),
    {ok,  <<10:64/little>>} = memcached:getb(Conn, "bin_key"),
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


test_add(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    ok = memcached:add(Conn, "myaddkey", "myvalue1"),
    {error, not_stored} = memcached:add(Conn, "myaddkey", "newvalue"),
    ok = memcached:disconnect(Conn).


test_append(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    {error, not_stored} = memcached:append(Conn, "appendkey", "a"),
    ok = memcached:set(Conn, "appendkey", "a"),
    ok = memcached:append(Conn, "appendkey", "a"),
    {ok, _} = memcached:get(Conn, "appendkey"),
    ok = memcached:disconnect(Conn).


test_prepend(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
    {error, not_stored} = memcached:prepend(Conn, "prependkey", "a"),
    ok = memcached:set(Conn, "prependkey", "a"),
    ok = memcached:prepend(Conn, "prependkey", "a"),
    {ok, _} = memcached:get(Conn, "prependkey"),
    ok = memcached:disconnect(Conn).

test_split(_Config) ->
    case memcached:split("abc\r\n\ndef\r\nxy") of
    {Head, Tail} ->
            Head = "abc",
            Tail = "\ndef\r\nxy"
    end.

%% test_decr(_Config) ->
%%     {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT),
%%     ok = memcached:setb(Conn, "decr_key", <<10:64/little>>),
%%     {ok, 3} = memcached:decr(Conn, "decr_key", <<1:64/little>>),
%%     ok = memcached:disconnect(Conn).

%% todo decr, bad value -> 0

%% Tests end.
all() ->
    [
test_connect_disconnect,
     test_connect_error,
     test_set_get,
     test_setb_getb,
     test_get_not_exist,
     test_get_multi,
     test_set_expiry,
     test_delete,
     test_replace,
     test_add,
     test_split,
     test_append,
     test_prepend
%%     test_decr
].
