-module(ch3).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%Errors on compilation:
%./server1.erl:21: function alloc/1 undefined
%./server1.erl:18: function channels/0 undefined
%./server1.erl:25: function free/2 undefined
%./server1.erl:2: Warning: undefined call-back function code_change/3
%./server1.erl:2: Warning: undefined call-back function handle_info/2
%./server1.erl:2: Warning: undefined call-back function terminate/2

start_link() ->
    gen_server:start_link({local, ch3}, ch3, [], []).

alloc() ->
    gen_server:call(ch3, alloc).

free(Ch) ->
    gen_server:cast(ch3, {free, Ch}).

init(_Args) ->
    {ok, channels()}.

handle_call(alloc, _From, Chs) ->
    {Ch, Chs2} = alloc(Chs),
    {reply, Ch, Chs2}.

handle_cast({free, Ch}, Chs) ->
    Chs2 = free(Ch, Chs),
    {noreply, Chs2}.
