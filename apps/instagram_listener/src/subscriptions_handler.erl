-module(subscriptions_handler).

-export([init/3,
        rest_init/2,
        allowed_methods/2,
        content_types_accepted/2,
        content_types_provided/2,
        handle_get/2,
        handle_post/2,
        delete_resource/2]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, handle_post}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

handle_get(Req, State) ->
    Body = handle_get(),
    {Body, Req, State}.

handle_post(Req, State) ->
    try
        {ok, ReqBody, Req1} = cowboy_req:body(Req),
        {Type, Value} = subscription_params(ReqBody),
        RespBody = handle_create({Type, Value}),
        Req2 = cowboy_req:set_resp_body(RespBody, Req1),
        {true, Req2, State}
    catch
        _:Exception ->
            handler_utils:handle_exception(Exception, Req, State)
    end.

delete_resource(Req, State) ->
    {SubscriptionId, Req1} =  cowboy_req:binding(subscription_id, Req),
    Body = handle_delete(SubscriptionId),
    Req2 = cowboy_req:set_resp_body(Body, Req1),
    {true, Req2, State}.

handle_get() ->
    body_for(client:subscriptions()).

%% TODO: handle delete id subscription
handle_delete(undefined) ->
    body_for(client:unsubscribe());

handle_delete(Object) ->
    body_for(client:unsubscribe({object, Object})).

handle_create({<<"tag">>, Tag}) ->
    body_for(client:subscribe({tag, Tag}));

handle_create({<<"location">>, Location}) ->
    body_for(client:subscribe({location, Location}));

handle_create({<<"geography">>, Geography}) ->
    Lat = maps:get(<<"lat">>, Geography),
    Lng = maps:get(<<"lng">>, Geography),
    Radius = maps:get(<<"radius">>, Geography),
    body_for(client:subscribe({geography, {Lat, Lng, Radius}})).

body_for({ok, Body}) ->
    Body;

body_for({error, _Error}) ->
    <<"{}">>.

subscription_params(ReqBody) ->
        Body = jsx:decode(ReqBody, [return_maps]),
        {maps:get(<<"type">>, Body), maps:get(<<"value">>, Body)}.
