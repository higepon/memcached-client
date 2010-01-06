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
    CHash = chash:new(chash1),
    ok = chash:add_node(CHash, "127.0.0.1:8000", server_a),
    ok = chash:add_node(CHash, "127.0.0.1:8001", server_b),
    ok = chash:add_node(CHash, "127.0.0.1:8002", server_c),
    Server = chash:get_node(CHash, "Hello"),
    io:format("Server=~p", [Server]),
    true = lists:any(fun(E) -> E =:= Server end, [server_a, server_b, server_c]),

    ok = chash:remove_node(CHash, "127.0.0.1:8001"),
    Server2 = chash:get_node(CHash, "Hello"),
    true = lists:any(fun(E) -> E =:= Server2 end, [server_a, server_c]),

    ok = chash:remove_node(CHash, "127.0.0.1:8002"),
    Server2 = chash:get_node(CHash, "Hello"),
    true = lists:any(fun(E) -> E =:= Server2 end, [server_a]),
    true = chash:delete(CHash),
    ok.


test_practical(_Config) ->
    Keys = gen_random_keys(10000, 10000 * 10000),
    CHash = chash:new(chash1),
    ok = chash:add_node(CHash, "127.0.0.1:8000", server_a),
    ok = chash:add_node(CHash, "127.0.0.1:8001", server_b),
    ok = chash:add_node(CHash, "127.0.0.1:8002", server_c),

    Servers = lists:map(fun(Key) -> chash:get_node(CHash, Key) end, Keys),
    NumberOfA = length(lists:filter(fun(Server) -> Server =:= server_a end, Servers)),
    NumberOfB = length(lists:filter(fun(Server) -> Server =:= server_b end, Servers)),
    NumberOfC = length(lists:filter(fun(Server) -> Server =:= server_c end, Servers)),
    io:format("~p ~p ~p ~p~n", [NumberOfA, NumberOfB, NumberOfC, Servers]),
    true = (3000 =< NumberOfA andalso NumberOfA =< 3600),
    ok.

%% Tests end.
all() ->
    [
     test_simple,
     test_practical
    ].

%%====================================================================
%% Internal functions
%%====================================================================
gen_random_keys(N, MaxKey) ->
    crypto:start(),
    lists:map(fun(Key) -> integer_to_list(Key) end, gen_random_keys(N, MaxKey, dict:new(), [])).


gen_random_keys(0, _MaxKey, _Dict, Keys) ->
    Keys;
gen_random_keys(N, MaxKey, Dict, Keys) ->
    RandomKey = random:uniform(MaxKey),
    case Dict:find(RandomKey) of
        {ok, _} ->
            gen_random_keys(N, MaxKey, Dict, Keys);
        error ->
            D1 = Dict:store(RandomKey, 1),
            gen_random_keys(N - 1, MaxKey, D1, [RandomKey | Keys])
    end.
