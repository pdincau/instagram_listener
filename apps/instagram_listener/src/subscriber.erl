-module(subscriber).

-export([subscribe/1, subscriptions/0, unsubscribe/0, unsubscribe/1]).

-define(BASE_URL, <<"https://api.instagram.com/v1/subscriptions?client_secret={client-secret}&client_id={client-id}&{params}">>).

subscribe({tag, _ObjectId}) ->
    {error, not_implemented};

subscribe({location, _ObjectId}) ->
    {error, not_implemented};

subscribe({geography, {_Lat, _Lng, _Radius}}) ->
    {error, not_implemented};

subscribe({user, _User}) ->
    %% to do not sure how this works
    {error, not_implemented}.

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
    do_request(delete, <<"&object=all">>).

unsubscribe({id, Id}) when is_binary(Id) ->
    Params = <<"id={unsubscribe_id}">>,
    do_request(delete, binary:replace(Params, <<"{unsubscribe_id}">>, Id));

unsubscribe({object, Object}) when is_binary(Object) ->
    Params = <<"object={unsubscribe_object}">>,
    do_request(delete, binary:replace(Params, <<"{unsubscribe_object}">>, Object)).

do_request(Verb, Params) ->
    {ok, ClientId} = application:get_env(instagram_listener, client_id),
    {ok, ClientSecret} = application:get_env(instagram_listener, client_secret),
    Url = url_for(ClientSecret, ClientId, Params),
    case httpc:request(Verb, {Url, [], "", <<"">>}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Body;
        Error ->
            {error, Error}
    end.

url_for(ClientSecret, ClientId) ->
    url_for(ClientSecret, ClientId, <<"">>).

url_for(ClientSecret, ClientId, Params) ->
    Url = binary:replace(?BASE_URL, <<"{client-secret}">>, ClientSecret),
    Url1 = binary:replace(Url, <<"{client-id}">>, ClientId),
    Url2 = binary:replace(Url1, <<"{params}">>, Params),
    binary_to_list(Url2).
