-module(worker).
-export([handle/1]).

handle(Update) ->
    io:format("~p~n", [Update]).
