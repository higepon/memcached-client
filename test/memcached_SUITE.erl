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
-define(MEMCACHED_PORT1, 11411).
-define(MEMCACHED_PORT2, 11511).
-define(MEMCACHED_HOST, "127.0.0.1").

suite() ->
    [{timetrap,{seconds,30}}].

%% N.B. We can't use init_per_suite to share the connection,
%% since init_per_suite and end_per_suite run on different processes.

%% Tests start.
test_connect_disconnect(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:disconnect(Conn).


test_connect_error(_Config) ->
    process_flag(trap_exit,true),
    {error, _} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1 + 1).


test_set_get(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_setb_getb(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "bin_key", <<10:64/little>>),
    io:format("aval=~p~n", [<<10:64/little>>]),
    {ok,  <<10:64/little>>} = memcached:getb(Conn, "bin_key"),
    ok = memcached:disconnect(Conn).


test_get_not_exist(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    {error, not_found} = memcached:get(Conn, "not-exist"),
    ok = memcached:disconnect(Conn).


test_set_expiry(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "mykey", "myvalue", 0, 1),
    receive after 1000 -> [] end,
    {error, not_found} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_delete(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    {ok, "myvalue"} = memcached:get(Conn, "mykey"),
    ok = memcached:delete(Conn, "mykey"),
    {error, not_found} = memcached:delete(Conn, "not_found"),
    {error, not_found} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_replace(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "mykey", "myvalue"),
    ok = memcached:replace(Conn, "mykey", "mynewvalue"),
    {ok, "mynewvalue"} = memcached:get(Conn, "mykey"),

    %% key not_found doesn't exist, so replace never happen.
    {error, not_stored} = memcached:replace(Conn, "not_found", "newvalue", 0, 0),
    ok = memcached:disconnect(Conn).


test_replaceb(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "mykey", <<10:64/little>>),
    ok = memcached:replaceb(Conn, "mykey", <<9:64/little>>),
    {ok, <<9:64/little>>} = memcached:getb(Conn, "mykey"),

    %% key not_found doesn't exist, so replaceb never happen.
    {error, not_stored} = memcached:replaceb(Conn, "not_found", <<8:64/little>>, 0, 0),
    ok = memcached:disconnect(Conn).


test_get_multi(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "mykey1", "myvalue1"),
    ok = memcached:set(Conn, "mykey3", "myvalue3"),
    {ok, [{"mykey1", "myvalue1"},
          {"mykey3", "myvalue3"}]}
        = memcached:get_multi(Conn, ["mykey1", "mykey2", "mykey3"]),
    ok = memcached:disconnect(Conn).


test_get_multib(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "mykey1b", <<10:64/little>>),
    ok = memcached:setb(Conn, "mykey3b", <<13:64/little>>),
    {ok, [{"mykey1b", <<10:64/little>>},
          {"mykey3b", <<13:64/little>>}]}
        = memcached:get_multib(Conn, ["mykey1b", "mykey2b", "mykey3b"]),
    ok = memcached:disconnect(Conn).


test_add(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:add(Conn, "myaddkey", "myvalue1"),
    {error, not_stored} = memcached:add(Conn, "myaddkey", "newvalue"),
    ok = memcached:disconnect(Conn).


test_addb(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:addb(Conn, "myaddbkey", <<10:64/little>>),
    {error, not_stored} = memcached:addb(Conn, "myaddbkey", <<9:64/little>>),
    {ok, <<10:64/little>>} = memcached:getb(Conn, "myaddbkey"),
    ok = memcached:disconnect(Conn).


test_append(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    {error, not_stored} = memcached:append(Conn, "appendkey", "a"),
    ok = memcached:set(Conn, "appendkey", "a"),
    ok = memcached:append(Conn, "appendkey", "a"),
    {ok, _} = memcached:get(Conn, "appendkey"),
    ok = memcached:disconnect(Conn).


test_prepend(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
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


test_incr(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "incr_key", list_to_binary("10")),
    {ok, 11} = memcached:incr(Conn, "incr_key", 1),
    {error, not_found} = memcached:incr(Conn, "incr_key2", 1),
    ok = memcached:disconnect(Conn).


test_decr(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "decr_key", list_to_binary("10")),
    {ok, 9} = memcached:decr(Conn, "decr_key", 1),
    {error, not_found} = memcached:decr(Conn, "decr_key2", 1),
    ok = memcached:disconnect(Conn).


test_version(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    Version = memcached:version(Conn),
    true = is_list(Version),
    ok = memcached:disconnect(Conn).


test_quit(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:quit(Conn),
    {error, closed} = memcached:get(Conn, "mykey"),
    ok = memcached:disconnect(Conn).


test_stats(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    case memcached:stats(Conn) of
        {ok, [{Key, _BinaryValue} | _More]} ->
            true = is_list(Key)
    end,
    ok = memcached:disconnect(Conn).


test_flush_all(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:flush_all(Conn),
    ok = memcached:flush_all(Conn, 10),
    ok = memcached:disconnect(Conn).


test_cas(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    Cas64 = <<1>>,
    ok = memcached:cas(Conn, "casKey", "casValue", 0, 0, Cas64),
    ok = memcached:disconnect(Conn).


test_gets(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "higeKey", "higeValue"),
    {ok, "higeValue", CasUnique64} = memcached:gets(Conn, "higeKey"),
    ok = memcached:cas(Conn, "higeKey", "higeNewValue", 0, 0, CasUnique64),
    {ok, "higeNewValue", CasUnique642} = memcached:gets(Conn, "higeKey"),
    true = CasUnique64 =/= CasUnique642,
    ok = memcached:disconnect(Conn).


test_gets_multi(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:set(Conn, "hageKey1", "hageValue1"),
    ok = memcached:set(Conn, "hageKey2", "hageValue2"),
    {ok, [{"hageKey1", "hageValue1", CasUnique1},
          {"hageKey2", "hageValue2", CasUnique2}]}
        = memcached:gets_multi(Conn, ["hageKey1", "hageKey2"]),
    ok = memcached:cas(Conn, "hageKey1", "hageNewValue1", 0, 0, CasUnique1),
    ok = memcached:cas(Conn, "hageKey2", "hageNewValue2", 0, 0, CasUnique2),
    {ok, [{"hageKey1", "hageNewValue1", NewCasUnique1},
          {"hageKey2", "hageNewValue2", NewCasUnique2}]} = memcached:gets_multi(Conn, ["hageKey1", "hageKey2"]),
    true = CasUnique1 =/= NewCasUnique1,
    true = CasUnique1 =/= NewCasUnique2,
     ok = memcached:disconnect(Conn).


test_gets_multib(_Config) ->
    {ok, Conn} = memcached:connect(?MEMCACHED_HOST, ?MEMCACHED_PORT1),
    ok = memcached:setb(Conn, "hageKey1", <<"hageValue1">>),
    ok = memcached:setb(Conn, "hageKey2", <<"hageValue2">>),
    {ok, [{"hageKey1", <<"hageValue1">>, CasUnique1},
          {"hageKey2", <<"hageValue2">>, CasUnique2}]}
        = memcached:gets_multib(Conn, ["hageKey1", "hageKey2"]),
    ok = memcached:casb(Conn, "hageKey1", <<"hageNewValue1">>, 0, 0, CasUnique1),
    ok = memcached:casb(Conn, "hageKey2", <<"hageNewValue2">>, 0, 0, CasUnique2),
    {ok, [{"hageKey1", <<"hageNewValue1">>, NewCasUnique1},
          {"hageKey2", <<"hageNewValue2">>, NewCasUnique2}]} = memcached:gets_multib(Conn, ["hageKey1", "hageKey2"]),
    true = CasUnique1 =/= NewCasUnique1,
    true = CasUnique1 =/= NewCasUnique2,
    ok = memcached:disconnect(Conn).


test_multiple_server(_Config) ->
    {ok, Conn} = memcached:connect([{?MEMCACHED_HOST, ?MEMCACHED_PORT1},
                                    {?MEMCACHED_HOST, ?MEMCACHED_PORT2}]),
    ok = memcached:set(Conn, "1", "1"),
    ok = memcached:set(Conn, "2", "2"),
    ok = memcached:set(Conn, "3", "3"),
    ok = memcached:set(Conn, "4", "4"),
    {ok, "1"} = memcached:get(Conn, "1"),
    {ok, "2"} = memcached:get(Conn, "2"),
    {ok, "3"} = memcached:get(Conn, "3"),
    {ok, "4"} = memcached:get(Conn, "4"),
    ok = memcached:disconnect(Conn).

%% Tests end.
all() ->
    [
     test_connect_disconnect,
     test_connect_error,
     test_set_get,
     test_setb_getb,
     test_get_not_exist,
     test_get_multi,
     test_get_multib,
     test_set_expiry,
     test_delete,
     test_replace,
     test_replaceb,
     test_add,
     test_addb,
     test_split,
     test_append,
     test_prepend,
     test_incr,
     test_decr,
     test_version,
     test_stats,
     test_quit,
     test_flush_all,
     test_cas,
     test_gets,
     test_gets_multi,
     test_gets_multib,
     test_multiple_server
].
