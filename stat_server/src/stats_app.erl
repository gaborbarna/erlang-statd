%% @private
-module(stats_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, ServerPid} = stat_server:start_server(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", statpage_handler, []},
            {"/websocket", ws_handler, {ServerPid}},
            {"/static/[...]", cowboy_static, [
                {directory, {priv_dir, stats, [<<"static">>]}},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]),
    websocket_sup:start_link().

stop(_State) ->
	ok.
