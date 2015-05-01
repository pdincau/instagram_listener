-module(handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HUB_MODE, <<"subscribe">>).
-define(VERIFY_TOKEN, <<"token">>).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reply(Method, Req2),
    {ok, Req3, State}.

reply(<<"GET">>, Req) ->
    {SubscriptionParameters, Req1} = subscription_parameters(Req),
    case SubscriptionParameters of
        {_, _, undefined} ->
            cowboy_req:reply(400, Req1);
        {?HUB_MODE, ?VERIFY_TOKEN, Challenge} ->
            cowboy_req:reply(200, [], Challenge, Req1);
        {_, _, _} ->
            cowboy_req:reply(400, Req1)
    end;

reply(_, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

subscription_parameters(Req) ->
    {Mode, Req2} = cowboy_req:qs_val(<<"hub.mode">>, Req),
    {VerifyToken, Req3} = cowboy_req:qs_val(<<"hub.verify_token">>, Req2),
    {Challenge, Req4} = cowboy_req:qs_val(<<"hub.challenge">>, Req3),
    {{Mode, VerifyToken, Challenge}, Req4}.
