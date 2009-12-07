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
    {ok, Conn} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT),
    Config.

end_per_suite(_Config) ->
    ok.

%% Tests start.
set_get(_Config) ->
    ok.

connect_error(_Config) ->
    process_flag(trap_exit,true),
    {error, _} = memcached:connect("127.0.0.1", ?MEMCACHED_PORT + 1).

%% Tests end.
all() ->
    [set_get,
     connect_error

    ].

