%%%-------------------------------------------------------------------
%%% File    : chash_SUITE.erl
%%% Author  : higepon <higepon@users.sourceforge.jp>
%%% Description : Tests for consistent hashing.
%%%
%%% Created :  4 Jan 2010 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------
-module(chash_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

%% Tests start.
test_simple(_Config) ->
    CHash = chash:create(),
    ok = chash:add_node(CHash, "127.0.0.1:8000", server_a),
    ok = chash:add_node(CHash, "127.0.0.1:8001", server_b),
    ok = chash:add_node(CHash, "127.0.0.1:8002", server_c),
    Server = chash:get_node(CHash, "Hello"),
    lists:any(fun(E) -> E =:= Server end, [server_a, server_b, server_c]),
    ok = chash:remove_node(CHash, "127.0.0.1:8001"),

    Server2 = chash:get_node(CHash, "Hello"),
    lists:any(fun(E) -> E =:= Server2 end, [server_a, server_c]),
    ok.

%% Tests end.
all() ->
    [
     test_simple
    ].

