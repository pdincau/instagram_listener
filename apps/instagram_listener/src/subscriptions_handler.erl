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
    Body = handle_get_subscription(),
    {Body, Req, State}.

handle_post(Req, State) ->
    try
        {ok, ReqBody, Req1} = cowboy_req:body(Req),
        Body = jsx:decode(ReqBody, [return_maps]),
        Type = maps:get(<<"type">>, Body),
        Value = maps:get(<<"value">>, Body),
        RespBody = handle_create_subscription({Type, Value}),
        Req2 = cowboy_req:set_resp_body(RespBody, Req1),
        {true, Req2, State}
    catch
        _:Exception ->
            handle_exception(Exception, Req, State)
    end.

delete_resource(Req, State) ->
    {SubscriptionId, Req1} =  cowboy_req:binding(subscription_id, Req),
    handle_delete_subscription(SubscriptionId),
    {true, Req1, State}.

handle_get_subscription() ->
    body_for(client:subscriptions()).

%% TODO: handle delete id subscription
handle_delete_subscription(undefined) ->
    client:unsubscribe();

handle_delete_subscription(Object) ->
    client:unsubscribe({object, Object}).

handle_create_subscription({<<"tag">>, Tag}) ->
    body_for(client:subscribe({tag, Tag}));

handle_create_subscription({<<"location">>, Location}) ->
    body_for(client:subscribe({location, Location}));

handle_create_subscription({<<"geography">>, Geography}) ->
    Lat = maps:get(<<"lat">>, Geography),
    Lng = maps:get(<<"lng">>, Geography),
    Radius = maps:get(<<"radius">>, Geography),
    body_for(client:subscribe({geography, {Lat, Lng, Radius}})).

body_for({ok, Body}) ->
    Body;

body_for({error, _Error}) ->
    <<"{}">>.

handle_exception(bad_key, Req, State) ->
    {ok, Req1} = cowboy_req:reply(400, Req),
    {halt, Req1, State};

handle_exception(badarg, Req, State) ->
    {ok, Req1} = cowboy_req:reply(400, Req),
    {halt, Req1, State}.
