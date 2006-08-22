-module(myserver).

-behaviour(gen_server).

-export([start/0, init/1, send/0, handle_call/3]).

start() ->
    gen_server:start({global, myserver}, myserver, [], []).

init(_Args) ->
    {ok, ""}.

send() ->
    gen_server:call(myserver, send).

handle_call(send, From, _State) ->
    %io:format("send called from ~p", [From]),
    {reply, "Thanks for calling", ""}.
