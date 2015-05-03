-module(subscriber).

-export([subscribe/1, subscriptions/0, unsubscribe/0, unsubscribe/1]).

-define(URL, <<"https://api.instagram.com/v1/subscriptions">>).
-define(BASE_BODY, <<"client_id={client-id}&client_secret={client-secret}&callback_url={callback_url}&{params}">>).

subscribe(Subscription) ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    {ok, CallbackUrl} = application:get_env(instagram_listener, callback_url),
    Body = body_for(ClientId, ClientSecret, CallbackUrl, custom_body(Subscription)),
    do_request(post, ?URL, Body).

subscriptions() ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    Url = url_for(ClientSecret, ClientId),
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            {error, Error}
    end.

unsubscribe() ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    Url = url_for(ClientSecret, ClientId, <<"object=all">>),
    do_request(delete, Url).

unsubscribe({id, Id}) when is_binary(Id) ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    Params = <<"id={unsubscribe_id}">>,
    BinaryParams = binary:replace(Params, <<"{unsubscribe_id}">>, Id),
    Url = url_for(ClientSecret, ClientId, BinaryParams),
    do_request(delete, Url);

unsubscribe({object, Object}) when is_binary(Object) ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    Params = <<"object={unsubscribe_object}">>,
    BinaryParams = binary:replace(Params, <<"{unsubscribe_object}">>, Object),
    Url = url_for(ClientSecret, ClientId, BinaryParams),
    do_request(delete, Url).

do_request(Verb, Url) ->
    do_request(Verb, Url, <<"">>).

do_request(Verb, Url, Body) ->
    case httpc:request(Verb, {Url, [], "", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            ResponseBody;
        Error ->
            {error, Error}
    end.

url_for(ClientSecret, ClientId) ->
    url_for(ClientSecret, ClientId, <<"">>).

url_for(ClientSecret, ClientId, Params) ->
    SubscriptionParams = <<"?client_secret={client-secret}&client_id={client-id}&{params}">>,
    SubscribeUrl = <<?URL/binary, SubscriptionParams/binary>>,
    Url = binary:replace(SubscribeUrl, <<"{client-secret}">>, ClientSecret),
    Url1 = binary:replace(Url, <<"{client-id}">>, ClientId),
    Url2 = binary:replace(Url1, <<"{params}">>, Params),
    binary_to_list(Url2).

body_for(ClientId, ClientSecret, CallbackUrl, Params) ->
    Body = binary:replace(?BASE_BODY, <<"{client-id}">>, ClientId),
    Body1 = binary:replace(Body, <<"{client-secret}">>, ClientSecret),
    Body2 = binary:replace(Body1, <<"{callback_url}">>, CallbackUrl),
    Body3 = binary:replace(Body2, <<"{params}">>, Params),
    binary_to_list(Body3).

custom_body(user) ->
    %% TODO: Not sure here
    <<"object=user&aspect=media">>;

custom_body({tag, ObjectId}) ->
    Body = <<"object=tag&aspect=media&object_id={object_id}">>,
    binary:replace(Body, <<"{object_id}">>, ObjectId);

custom_body({location, ObjectId}) ->
    Body = <<"object=location&aspect=media&object_id={object_id}">>,
    binary:replace(Body, <<"{object_id}">>, ObjectId);

custom_body({geography, {Lat, Lng, Radius}}) ->
    Body = <<"object=geography&lat={lat}&lng={lng}&radius={radius}&aspect=media">>,
    Body1 = binary:replace(Body, <<"{lat}">>, Lat),
    Body2 = binary:replace(Body1, <<"{lng}">>, Lng),
    binary:replace(Body2, <<"{radius}">>, Radius).
