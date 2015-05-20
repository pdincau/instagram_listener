-module(instagram_listener_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

-define(C_ACCEPTORS, 100).

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting application instagram_listener.~n", []),
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    Port = 8080,
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    instagram_listener_sup:start_link().

stop(_State) ->
    ok.

routes() ->
    [{'_', [{"/instagram", handler, []},
            {"/subscriptions/[:subscription_id]", subscriptions_handler, []}]}].
