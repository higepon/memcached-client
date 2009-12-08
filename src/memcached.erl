%%    Copyright (c) 2009  Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
%%
%%    Redistribution and use in source and binary forms, with or without
%%    modification, are permitted provided that the following conditions
%%    are met:
%%
%%    1. Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%
%%    2. Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%%    3. Neither the name of the authors nor the names of its contributors
%%       may be used to endorse or promote products derived from this
%%       software without specific prior written permission.
%%
%%    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%-------------------------------------------------------------------
%%% File    : memcached.erl
%%% Author  : Taro Minowa(Higepon) <higepon@users.sourceforge.jp>
%%% Description : A minimal memcached client library.
%%%
%%% Created :  7 Dec 2009 by higepon <higepon@users.sourceforge.jp>
%%%-------------------------------------------------------------------
-module(memcached).
-behaviour(gen_server).

%% API
-export([connect/2, disconnect/1,
        set/3, get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Definitions
%%====================================================================
-define(TCP_OPTIONS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false},
                      {sndbuf,16384},{recbuf,4096}]).
-define(TIMEOUT, 1000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: connect
%% Description: connect to a memcached
%% Returns: {ok, Conn} or {error, Reason}.
%%--------------------------------------------------------------------
connect(Host, Port) ->
    Name = random_id(),
    gen_server:start_link({local, Name}, ?MODULE, [Host, Port], []).


%%--------------------------------------------------------------------
%% Function: set
%% Description: set value
%% Returns: ok
%%--------------------------------------------------------------------
set(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {set, Key, Value}).


%%--------------------------------------------------------------------
%% Function: get
%% Description: get value
%% Returns: {ok, Value}, {ok, not_exist} or {error, Reason}
%%--------------------------------------------------------------------
get(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {get, Key}).


%%--------------------------------------------------------------------
%% Function: disconnect
%% Description: disconnect
%% Returns: ok
%%--------------------------------------------------------------------
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).

connect(Hosts, Ports, Fun) ->
    %% todo
    %% Fun is
    ok.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, Key}, _From, Socket) ->
    Command = iolist_to_binary([<<"get ">>, Key]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"END\r\n">>} ->
            {reply, {ok, not_exist}, Socket};
        {ok, Packet} ->
            %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
            Parsed = io_lib:fread("VALUE ~s ~u ~u\r\n", binary_to_list(Packet)),
            {ok, [_Key, _Flags, Bytes], Rest} = Parsed,
            Value = binary_to_term(list_to_binary(Rest)),
            {reply, {ok, Value}, Socket};
        {error, Reason} ->
            {reply, {error, Reason}, Socket}
    end;


handle_call({set, Key, Value}, _From, Socket) ->
    ValueAsBinary = term_to_binary(Value),
    Bytes = integer_to_list(size(ValueAsBinary)),
    Command = iolist_to_binary([<<"set ">>, Key, <<" ">>, "0", <<" ">>, "0", <<" ">>, Bytes]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    gen_tcp:send(Socket, <<ValueAsBinary/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case string:tokens(binary_to_list(Packet), "\r\n") of
                ["STORED"] ->
                    {reply, ok, Socket};
                Other ->
                    {reply, {error, Other}, Socket}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, Socket}
    end;


handle_call(disconnect, _From, Socket) ->
    gen_tcp:close(Socket),
    {reply, ok, Socket}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("cast=~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("info=~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, Socket) ->
    io:format("terminate ~p~n", [self()]),
    gen_tcp:close(Socket).

%%====================================================================
%% Internal functions
%%====================================================================
random_id() ->
    crypto:start(),
    list_to_atom("memcached_client" ++ integer_to_list(crypto:rand_uniform(1, 65536 * 65536))).


init([Host, Port]) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            {stop, Reason};
        Other ->
            {stop, Other}
    end.
