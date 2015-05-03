-module(subscriber).

-export([subscriptions/0]).

-define(BASE_URL, <<"https://api.instagram.com/v1/subscriptions?client_secret={client-secret}&client_id={client-id}">>).

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

url_for(ClientSecret, ClientId) ->
    Url = binary:replace(?BASE_URL, <<"{client-secret}">>, ClientSecret),
    Url1 = binary:replace(Url, <<"{client-id}">>, ClientId),
    binary_to_list(Url1).
