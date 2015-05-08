-module(handler_utils).
-export([handle_exception/3]).

handle_exception(bad_key, Req, State) ->
    {ok, Req1} = cowboy_req:reply(400, Req),
    {halt, Req1, State};

handle_exception(badarg, Req, State) ->
    {ok, Req1} = cowboy_req:reply(400, Req),
    {halt, Req1, State}.
