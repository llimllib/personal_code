-module(tut).
-export([double/1]).

double({f, X}) -> 
    2 * X;
double(A) ->
    io:format('No Match').
