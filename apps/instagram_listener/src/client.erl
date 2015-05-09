-module(client).
-behaviour(gen_server).

-export([start_link/0, stop/0, subscribe/1, subscriptions/0, unsubscribe/0, unsubscribe/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(URL, <<"https://api.instagram.com/v1/subscriptions">>).
-define(BASE_BODY, <<"client_id=~s&client_secret=~s&callback_url=~s&verify_token=~s&~s">>).

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
    Params = <<"object=~s">>,
    Params1 = lists:flatten(io_lib:format(Params, [Object])),
    Url = url_for(Secret, Id, Params1),
    delete(Url);

handle_unsubscribe({id, SubscriptionId}, Secret, Id) ->
    Params = <<"id=~s">>,
    Params1 = lists:flatten(io_lib:format(Params, [SubscriptionId])),
    Url = url_for(Secret, Id, Params1),
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
            {ok, ResponseBody};
        Error ->
            {error, Error}
    end.

url_for(ClientSecret, ClientId) ->
    url_for(ClientSecret, ClientId, <<"">>).

url_for(ClientSecret, ClientId, Params) ->
    SubscriptionParams = <<"?client_secret=~s&client_id=~s&~s">>,
    lists:flatten(io_lib:format(<<?URL/binary, SubscriptionParams/binary>>, [ClientSecret, ClientId, Params])).

body_for(ClientId, ClientSecret, CallbackUrl, VerifyToken, Params) ->
    lists:flatten(io_lib:format(?BASE_BODY, [ClientId, ClientSecret, CallbackUrl, VerifyToken, Params])).

custom_body(user) ->
    <<"object=user&aspect=media">>;

custom_body({tag, ObjectId}) ->
    Body = <<"object=tag&aspect=media&object_id=~s">>,
    lists:flatten(io_lib:format(Body, [ObjectId]));

custom_body({location, ObjectId}) ->
    Body = <<"object=location&aspect=media&object_id=~s">>,
    lists:flatten(io_lib:format(Body, [ObjectId]));

custom_body({geography, {Lat, Lng, Radius}}) ->
    Body = <<"object=geography&lat=~s&lng=~s&radius=~s&aspect=media">>,
    lists:flatten(io_lib:format(Body, [Lat, Lng, Radius])).
