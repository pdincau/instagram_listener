# instagram_listener
An Erlang implementation for subscribing and receiving [`Instagram Realtime Photo Updates`](https://instagram.com/developer/realtime/ "Instagram Realtime Photo Updates").


Build
-----

    $ rebar3 release

Run in dev mode:

    $ _build/default/rel/instagram_listener/bin/instagram_listener console

Remember to set in your `config/sys.config` all the variables needed.

Once Instagram Listener is running, you can subscribe for a given kind of update using the module `client.erl` inside the release shell

    1> client:start_link().
    2> client:subscribe(Subscription).

where Subscriptions can be: `{tag, <<"yourtag">>}, {location, <<"locationId">>}, {geography, {<<"Lat">>, <<"Lng">>, <<"Radius">>}}`.

You can list all your subscriptions using:

    3> client:subscriptions().

You can also unsubscribe from all subscriptions using:

    4> client:unsubscribe().

or you can unsubscribe from a specific object type using:

    5> client:unsubscribe({object, Object}).

where Object can be `<<"tag">>, <<"location">>, <<"geography">>`.

or you can unsubscribe form a specific id using:

    6> client:unsubscribe({id, <<"id">>).

Whenever a Realtime Update is received, the application will forward it to the module called `worker.erl`. It is your responsability to implement this module and make it do whatever you want (e.g. log the update, retrieve the object, etc etc).
