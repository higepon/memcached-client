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

    ok = chash:remove_node(CHash, "127.0.0.1:8000"),
    Server2 = chash:get_node(CHash, "Hello"),
    true = lists:any(fun(E) -> E =:= Server2 end, [server_b, server_c]),

    ok = chash:remove_node(CHash, "127.0.0.1:8002"),
    Server3 = chash:get_node(CHash, "Hello"),
    true = lists:any(fun(E) -> E =:= Server3 end, [server_b]),
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
    true = (3000 =< NumberOfA andalso NumberOfA =< 3600),
    true = (3000 =< NumberOfB andalso NumberOfB =< 3600),
    true = (3000 =< NumberOfC andalso NumberOfC =< 3600),

    %% remove node
    ok = chash:remove_node(CHash, "127.0.0.1:8001"),
    Servers2 = lists:map(fun(Key) -> chash:get_node(CHash, Key) end, Keys),
    NumberOfA2 = length(lists:filter(fun(Server) -> Server =:= server_a end, Servers2)),
    NumberOfB2 = length(lists:filter(fun(Server) -> Server =:= server_b end, Servers2)),
    NumberOfC2 = length(lists:filter(fun(Server) -> Server =:= server_c end, Servers2)),
    true = (4000 =< NumberOfA2 andalso NumberOfA2 =< 6000),
    true = (4000 =< NumberOfC2 andalso NumberOfC2 =< 6000),
    true = NumberOfB2 =:= 0,

    %% remove node
    ok = chash:remove_node(CHash, "127.0.0.1:8000"),
    Servers3 = lists:map(fun(Key) -> chash:get_node(CHash, Key) end, Keys),
    NumberOfA3 = length(lists:filter(fun(Server) -> Server =:= server_a end, Servers3)),
    NumberOfB3 = length(lists:filter(fun(Server) -> Server =:= server_b end, Servers3)),
    NumberOfC3 = length(lists:filter(fun(Server) -> Server =:= server_c end, Servers3)),
    true = NumberOfA3 =:= 0,
    true = NumberOfB3 =:= 0,
    true = NumberOfC3 =:= 10000,

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
