-module(fp_client).

-export([sender/0, recv/1, main_loop/0, wha/0]).

%send data to the fp_server
sender() ->
    S = io:get_line('send> '),
    {fp_server, fp_server@hello} ! recv(S).

%get data from the user
recv(Data) ->
    S = {fp_server, fp_server@hello} ! {recv, "test"},
    io:format("~s~n", [S]).

wha() ->
    {fp_server, fp_server@hello} ! send.

%strip the last character of the input string
strip_cr(S) ->
    string:substr(S, 1, length(S)-1).

main_loop() ->
    Cmd = strip_cr(httpd_util:to_lower(io:get_line('cmd > '))),
    case Cmd of
        "send" ->
            sender(),
            main_loop();
        "recv" ->
            recv(fp_server:send()),
            main_loop();
        "quit" ->
            Msg = "Thanks for using Fisher-Price (tm) My First Server",
            io:format("~s~n", [Msg]);
        _ ->
            io:format("~s~n", ["Please enter \"send\" or \"recv\""])
    end.
