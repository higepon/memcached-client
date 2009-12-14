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
         set/3, set/5, setb/3,%% setb/5,
         get/2, get_multi/2, getb/2,
         replace/3, replace/5,
         add/3, add/5,
         append/3, prepend/3,
         delete/2,
         decr/3,
         split/1
        ]).

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


set(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {set, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: setb
%% Description: set binary value
%% Returns: ok
%%--------------------------------------------------------------------
setb(Conn, Key, Value) when is_list(Key) andalso is_binary(Value) ->
    gen_server:call(Conn, {setb, Key, Value}).


%%--------------------------------------------------------------------
%% Function: replace
%% Description: replace value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
replace(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {replace, Key, Value}).


replace(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {replace, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: add
%% Description: add value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
add(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {add, Key, Value}).


add(Conn, Key, Value, Flags, ExpTime) when is_list(Key) andalso is_integer(ExpTime) ->
    gen_server:call(Conn, {add, Key, Value, Flags, ExpTime}).


%%--------------------------------------------------------------------
%% Function: append
%% Description: append value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
append(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {append, Key, Value}).


%%--------------------------------------------------------------------
%% Function: prepend
%% Description: prepend value
%% Returns: ok, {error, not_stored} or {error, Reason}
%%--------------------------------------------------------------------
prepend(Conn, Key, Value) when is_list(Key) ->
    gen_server:call(Conn, {prepend, Key, Value}).


%%--------------------------------------------------------------------
%% Function: get
%% Description: get value
%% Returns: {ok, Value}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
get(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {get, Key}).


%%--------------------------------------------------------------------
%% Function: getb
%% Description: get value as binary
%% Returns: {ok, Value}, {error, not_found} or {error, Reason}
%%--------------------------------------------------------------------
getb(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {getb, Key}).

%%--------------------------------------------------------------------
%% Function: get_multi
%% Description: get multiple values
%% Returns: {ok, Values}, Values = list of {Key, Value}.
%%--------------------------------------------------------------------
get_multi(Conn, Keys) when is_list(Keys) ->
    gen_server:call(Conn, {get_multi, Keys}).


%%--------------------------------------------------------------------
%% Function: delete
%% Description: delete value
%% Returns: ok
%%--------------------------------------------------------------------
delete(Conn, Key) when is_list(Key) ->
    gen_server:call(Conn, {delete, Key}).


%%--------------------------------------------------------------------
%% Function: decr
%% Description: decr value
%% Returns: {ok, NewValue}
%%--------------------------------------------------------------------
decr(Conn, Key, Value) ->
    gen_server:call(Conn, {decr, Key, Value}).


%%--------------------------------------------------------------------
%% Function: disconnect
%% Description: disconnect
%% Returns: ok
%%--------------------------------------------------------------------
disconnect(Conn) ->
    gen_server:call(Conn, disconnect).

connect(Hosts, Ports, Fun) ->
    %% todo
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
            {reply, {error, not_found}, Socket};
        {ok, <<"ERROR\r\n">>} ->
            {reply, {error, unknown_command}, Socket};
        {ok, Packet} ->
            %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
            Parsed = io_lib:fread("VALUE ~s ~u ~u\r\n", binary_to_list(Packet)),
            {ok, [_Key, _Flags, _Bytes], Rest} = Parsed,
            Value = binary_to_term(list_to_binary(Rest)),
            {reply, {ok, Value}, Socket};
        {error, Reason} ->
            {reply, {error, Reason}, Socket}
    end;

handle_call({getb, Key}, _From, Socket) ->
    Command = iolist_to_binary([<<"get ">>, Key]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"END\r\n">>} ->
            {reply, {error, not_found}, Socket};
        {ok, <<"ERROR\r\n">>} ->
            {reply, {error, unknown_command}, Socket};
        {ok, Packet} ->
            io:format("Packet=~p~n", [Packet]),
            %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
            %% N.B. we can't use io_lib:fread, since it can't handle \r\n.
            case split(binary_to_list(Packet)) of
                {error, Reason} ->
                    {reply, {error, Reason}, Socket};
                {Head, Tail} ->
                    io:format("Head=~p Tail=~p~n", [Head, Tail]),
                    {ok, [_Key, _Flags, Bytes], []} = io_lib:fread("VALUE ~s ~u ~u", Head),
                    {ValueList, _}  = lists:split(Bytes, Tail),
                    Value = list_to_binary(ValueList),
                    {reply, {ok, Value}, Socket}
            end
    end;


handle_call({get_multi, Keys}, _From, Socket) ->
    Command = iolist_to_binary([<<"get ">>, string_join(" ", Keys)]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case parse_values(binary_to_list(Packet), []) of
                {ok, Values} ->
                    {reply, {ok, Values}, Socket};
                Other ->
                    {reply, Other, Socket}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, Socket}
    end;


handle_call({set, Key, Value}, _From, Socket) ->
    {reply, storage_command(Socket, "set", Key, Value, 0, 0), Socket};
handle_call({set, Key, Value, Flags, ExpTime}, _From, Socket) ->
    {reply, storage_command(Socket, "set", Key, Value, Flags, ExpTime), Socket};


handle_call({setb, Key, Value}, _From, Socket) ->
    {reply, storage_command2(Socket, "set", Key, Value, 0, 0), Socket};

handle_call({replace, Key, Value}, _From, Socket) ->
    {reply, storage_command(Socket, "replace", Key, Value, 0, 0), Socket};
handle_call({replace, Key, Value, Flags, ExpTime}, _From, Socket) ->
    {reply, storage_command(Socket, "replace", Key, Value, Flags, ExpTime), Socket};


handle_call({add, Key, Value}, _From, Socket) ->
    {reply, storage_command(Socket, "add", Key, Value, 0, 0), Socket};
handle_call({add, Key, Value, Flags, ExpTime}, _From, Socket) ->
    {reply, storage_command(Socket, "add", Key, Value, Flags, ExpTime), Socket};


handle_call({append, Key, Value}, _From, Socket) ->
    {reply, storage_command(Socket, "append", Key, Value, 0, 0), Socket};
handle_call({prepend, Key, Value}, _From, Socket) ->
    {reply, storage_command(Socket, "prepend", Key, Value, 0, 0), Socket};


handle_call({delete, Key}, _From, Socket) ->
    {reply, delete_command(Socket, Key), Socket};


handle_call({decr, Key, Value}, _From, Socket) ->
    {reply, decr_command(Socket, Key, Value), Socket};


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
parse_values(Data, Values) ->
    case Data of
        "END\r\n" ->
            {ok, lists:reverse(Values)};
        "ERROR\r\n" ->
            {error, unknown_command};
        _ ->
            %% Format: VALUE <key> <flags> <bytes> [<cas unique>]\r\n
            Parsed = io_lib:fread("VALUE ~s ~u ~u\r\n", Data),
            {ok, [Key, _Flags, Bytes], Rest} = Parsed,
            SepLen = length("\r\n"),
            {Head, Tail}  = lists:split(Bytes + SepLen, Rest),

            Value = binary_to_term(list_to_binary(Head)),
            case Tail of
                [] -> {ok, lists:reverse([{Key, Value} | Values])};
                _ ->
                    parse_values(Tail, [{Key, Value} | Values])
            end
    end.


storage_command(Socket, Command, Key, Value, Flags, ExpTime) when is_integer(Flags) andalso is_integer(ExpTime) ->
    ValueAsBinary = term_to_binary(Value),
    Bytes = integer_to_list(size(ValueAsBinary)),
    CommandAsBinary = iolist_to_binary([Command, <<" ">>, Key, <<" ">>, integer_to_list(Flags), <<" ">>, integer_to_list(ExpTime), <<" ">>, Bytes]),
    gen_tcp:send(Socket, <<CommandAsBinary/binary, "\r\n">>),
    gen_tcp:send(Socket, <<ValueAsBinary/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case string:tokens(binary_to_list(Packet), "\r\n") of
                ["STORED"] ->
                    ok;
                ["NOT_STORED"] ->
                    {error, not_stored};
                ["ERROR"] ->
                    {error, unknown_command};
                %% memcached returns this for append command.
                ["ERROR", "ERROR"] ->
                    {error, unknown_command};
                Other ->
                    io:format("Other=~p~n", [Other]),
                    {error, Other}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

storage_command2(Socket, Command, Key, Value, Flags, ExpTime) when is_integer(Flags) andalso is_integer(ExpTime) ->
    ValueAsBinary = Value,
    Bytes = integer_to_list(size(ValueAsBinary)),
    CommandAsBinary = iolist_to_binary([Command, <<" ">>, Key, <<" ">>, integer_to_list(Flags), <<" ">>, integer_to_list(ExpTime), <<" ">>, Bytes]),
    gen_tcp:send(Socket, <<CommandAsBinary/binary, "\r\n">>),
    gen_tcp:send(Socket, <<ValueAsBinary/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, Packet} ->
            case string:tokens(binary_to_list(Packet), "\r\n") of
                ["STORED"] ->
                    ok;
                ["NOT_STORED"] ->
                    {error, not_stored};
                ["ERROR"] ->
                    {error, unknown_command};
                %% memcached returns this for append command.
                ["ERROR", "ERROR"] ->
                    {error, unknown_command};
                Other ->
                    io:format("Other=~p~n", [Other]),
                    {error, Other}
            end;
        {error, Reason} ->
            {error, Reason}
    end.



%% memcached 1.4.0 or higher doesn't support time argument.
delete_command(Socket, Key) ->
    Command = iolist_to_binary([<<"delete ">>, Key]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"DELETED\r\n">>} ->
            ok;
        {ok, <<"NOT_FOUND\r\n">>} ->
            {error, not_found};
        {ok, Other} ->
            {error, binary_to_list(Other)};
        {error, Reason} ->
            {error, Reason}
    end.


decr_command(Socket, Key, Value) ->
    Command = iolist_to_binary([<<"decr ">>, Key, " ", Value]),
    gen_tcp:send(Socket, <<Command/binary, "\r\n">>),
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
        {ok, <<"NOT_FOUND\r\n">>} ->
            {error, not_found};
        {ok, Packet} ->
            Parsed = io_lib:fread("~d\r\n", binary_to_list(Packet)),
            {ok, [NewValue], _Rest} = Parsed,
            {error, NewValue};
        {error, Reason} ->
            {error, Reason}
    end.


random_id() ->
    crypto:start(),
    list_to_atom("memcached_client" ++ integer_to_list(crypto:rand_uniform(1, 65536 * 65536))).

%% Borrowed from http://www.trapexit.org/String_join_with
string_join(Join, L) ->
    string_join(Join, L, fun(E) -> E end).

string_join(_Join, L=[], _Conv) ->
    L;
string_join(Join, [H|Q], Conv) ->
    lists:flatten(lists:concat(
        [Conv(H)|lists:map(fun(E) -> [Join, Conv(E)] end, Q)]
    )).


init([Host, Port]) ->
    case gen_tcp:connect(Host, Port, ?TCP_OPTIONS) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            {stop, Reason};
        Other ->
            {stop, Other}
    end.

split(Head, S) ->
    case S of
        %% "\r\n"
        [13 | [10 | More]] ->
            {lists:reverse(Head), More};
        [] ->
            {error, not_found};
        [H | T] ->
            split([H | Head], T)
    end.

%% split string with "\r\n"
split(S) ->
    split([], S).


