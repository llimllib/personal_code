-module(fp_server).

-behaviour(gen_server).

-export([start/0, init/1, recv/2, send/1, handle_call/3, test/0]).

start() ->
    gen_server:start({local, fp_server}, fp_server, [], []).

%State is currently an empty list of strings
init(_Args) ->
    {ok, ""}.

%get data from the user
recv(Data, From) ->
    gen_server:call(fp_server, {recv, Data}).

%send the value of the current buffer to the user
send(Node) ->
    gen_server:call({fp_server, Node}, send).

test() ->
    io:format("testing", []).

%append new string to the buffer and return the result as the new state
handle_call({recv, Data}, _From, Buffer) ->
    {reply, ok, lists:append([Buffer, "\n", Data])};

%send the buffer to the caller
handle_call(send, _From, Buffer) ->
    %return buffer, and maintain it as the current state. To have the buffer
    %get cleared after every "recv" operation, this line would be
    %{reply, Buffer, []}.
    {reply, Buffer, Buffer}.
