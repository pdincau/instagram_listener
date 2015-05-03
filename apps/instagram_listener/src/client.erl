-module(client).
-behaviour(gen_server).

-export([start_link/0, stop/0, subscribe/1, subscriptions/0, unsubscribe/0, unsubscribe/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(URL, <<"https://api.instagram.com/v1/subscriptions">>).
-define(BASE_BODY, <<"client_id={client-id}&client_secret={client-secret}&callback_url={callback_url}&verify_token={verify_token}&{params}">>).

-record(state, {id, secret, callbackUrl, token}).

subscribe(Subscription) ->
    gen_server:call(?SERVER, {subscribe, Subscription}).

subscriptions() ->
    gen_server:call(?SERVER, subscriptions).

unsubscribe() ->
    unsubscribe(all).

unsubscribe(Subscription) ->
    gen_server:call(?SERVER, {unsubscribe, Subscription}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    {ok, Id} = application:get_env(instagram_listener, client_id),
    {ok, Secret} = application:get_env(instagram_listener, client_secret),
    {ok, CallbackUrl} = application:get_env(instagram_listener, callback_url),
    {ok, Token} = application:get_env(instagram_listener, verify_token),
    {ok, #state{id=Id, secret=Secret, callbackUrl=CallbackUrl, token=Token}}.

handle_call(subscriptions, _From, #state{id=Id, secret=Secret} = State) ->
    Reply = handle_list_subscriptions(Secret, Id),
    {reply, Reply, State};

handle_call({subscribe, Subscription}, _From, #state{id=Id, secret=Secret, callbackUrl=CallbackUrl, token=Token} = State) ->
    Reply = handle_subscribe(Subscription, Id, Secret, CallbackUrl, Token),
    {reply, Reply, State};

handle_call({unsubscribe, Subscription}, _From, #state{id=Id, secret=Secret} = State) ->
    Reply = handle_unsubscribe(Subscription, Secret, Id),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_list_subscriptions(Secret, Id) ->
    Url = url_for(Secret, Id),
    do_get(Url).

handle_subscribe(Subscription, Id, Secret, CallbackUrl, Token) ->
    Body = body_for(Id, Secret, CallbackUrl, Token, custom_body(Subscription)),
    post(binary_to_list(?URL), Body).

handle_unsubscribe(all, Secret, Id) ->
    Url = url_for(Secret, Id, <<"object=all">>),
    delete(Url);

handle_unsubscribe({object, Object}, Secret, Id) ->
    Params = <<"object={unsubscribe_object}">>,
    BinaryParams = binary:replace(Params, <<"{unsubscribe_object}">>, Object),
    Url = url_for(Secret, Id, BinaryParams),
    delete(Url);

handle_unsubscribe({id, SubscriptionId}, Secret, Id) ->
    Params = <<"id={unsubscribe_id}">>,
    BinaryParams = binary:replace(Params, <<"{unsubscribe_id}">>, SubscriptionId),
    Url = url_for(Secret, Id, BinaryParams),
    delete(Url).

post(Url, Body) ->
    request(post, {Url, [], "", Body}).

do_get(Url) ->
    request(get,  {Url, []}).

delete(Url) ->
   request(delete, {Url, [], "", <<"">>}).

request(Method, Request) ->
    case httpc:request(Method, Request, [], []) of
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

body_for(ClientId, ClientSecret, CallbackUrl, VerifyToken, Params) ->
    Body = binary:replace(?BASE_BODY, <<"{client-id}">>, ClientId),
    Body1 = binary:replace(Body, <<"{client-secret}">>, ClientSecret),
    Body2 = binary:replace(Body1, <<"{callback_url}">>, CallbackUrl),
    Body3 = binary:replace(Body2, <<"{verify_token}">>, VerifyToken),
    Body4 = binary:replace(Body3, <<"{params}">>, Params),
    binary_to_list(Body4).

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
