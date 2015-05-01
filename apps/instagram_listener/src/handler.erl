-module(handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(HUB_MODE, <<"subscribe">>).
-define(VERIFY_TOKEN, <<"token">>).
-define(APP_SECRET, <<"secret">>).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = reply(Method, Req2),
    {ok, Req3, State}.

reply(<<"GET">>, Req) ->
    SubscriptionParameters = subscription_parameters(Req),
    case SubscriptionParameters of
        {?HUB_MODE, ?VERIFY_TOKEN, Challenge} ->
            cowboy_req:reply(200, [], Challenge, Req);
        {_, _, _} ->
            cowboy_req:reply(400, Req)
    end;

reply(<<"POST">>, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            handle_post_with_body(Req);
        false ->
            cowboy_req:reply(400, Req)
    end;

reply(_, Req) ->
    cowboy_req:reply(405, Req).

handle_post_with_body(Req) ->
    case is_valid_update(Req) of
        true ->
            Req2 = handle_updates(Req),
            cowboy_req:reply(200, [], <<"">>, Req2);
        false ->
            cowboy_req:reply(400, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

subscription_parameters(Req) ->
    {Mode, Req2} = cowboy_req:qs_val(<<"hub.mode">>, Req),
    {VerifyToken, Req3} = cowboy_req:qs_val(<<"hub.verify_token">>, Req2),
    {Challenge, _Req4} = cowboy_req:qs_val(<<"hub.challenge">>, Req3),
    {Mode, VerifyToken, Challenge}.

is_valid_update(Req) ->
    {XHubSignature, Req2} = cowboy_req:header(<<"x-hub-signature">>, Req),
    {ok, [{Payload, true}], _Req3} = cowboy_req:body_qs(Req2),

    is_valid_pair(XHubSignature, Payload).

is_valid_pair(XHubSignature, Payload) ->
    <<Mac:160/integer>> = crypto:hmac(sha, ?APP_SECRET, Payload),
    Signature = lists:flatten(io_lib:format("sha1=~40.16.0b", [Mac])),
    binary_to_list(XHubSignature) =:= Signature.

handle_updates(Req) ->
    {ok, [{Payload, true}], Req2} = cowboy_req:body_qs(Req),
    Update = jsx:decode(Payload),
    spawn(fun() -> io:format("update is~p~n", [Update]) end),
    Req2.
