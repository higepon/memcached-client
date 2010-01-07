%%%-------------------------------------------------------------------
%%% File    : chash.erl
%%% Author  : higepon <higepon@users.sourceforge.jp>
%%% Description : Consistent Hashing library.
%%%
%%% Created :  5 Jan 2010 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------
-module(chash).

%% API
-export([new/1, delete/1, add_node/3, remove_node/2, get_node/2]).


%%====================================================================
%% Definitions
%%====================================================================
-define(NUMBER_OF_REPLICAS, 100).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: new
%% Description:
%%--------------------------------------------------------------------
new(Name) ->
    ets:new(Name, [ordered_set, protected]).

%%--------------------------------------------------------------------
%% Function: add_node
%% Description:
%%--------------------------------------------------------------------
add_node(CHash, NodeKey, Node) ->
    add_node(0, CHash, NodeKey, Node).

add_node(N, _CHash, _NodeKey, _Node) when N =:= ?NUMBER_OF_REPLICAS ->
    ok;
add_node(N, CHash, NodeKey, Node) ->
    Hash = hash([N | NodeKey]),
    true = ets:insert(CHash, {Hash, Node}),
    add_node(N + 1, CHash, NodeKey, Node).

%%--------------------------------------------------------------------
%% Function: remove_node
%% Description:
%%--------------------------------------------------------------------
remove_node(CHash, NodeKey) ->
    remove_node(0, CHash, NodeKey).

remove_node(N, _CHash, _NodeKey) when N =:= ?NUMBER_OF_REPLICAS ->
    ok;
remove_node(N, CHash, NodeKey) ->
    Hash = hash([N | NodeKey]),
    true = ets:delete(CHash, Hash),
    remove_node(N + 1, CHash, NodeKey).

%%--------------------------------------------------------------------
%% Function: get_node
%% Description:
%%--------------------------------------------------------------------
get_node(CHash, Key) ->
    Hash = hash(Key),
    case ets_lookup(CHash, Hash) of
        [] ->
            case ets:next(CHash, Hash) of
                '$end_of_table' ->
                    case ets:first(CHash) of
                        '$end_of_table' ->
                            {error, no_entry};
                        FirstKey ->
                            ets_lookup(CHash, FirstKey)
                    end;
                NextKey ->
                    ets_lookup(CHash, NextKey)
            end;
        Value ->
            Value
    end.

%%--------------------------------------------------------------------
%% Function: delete
%% Description:
%%--------------------------------------------------------------------
delete(CHash) ->
    ets:delete(CHash).

%%====================================================================
%% Internal functions
%%====================================================================
hash(Key) ->
    erlang:md5(Key).

ets_lookup(CHash, Key) ->
    case ets:lookup(CHash, Key) of
        [{_Key, Value}] ->
             Value;
        Other -> Other
    end.
