-module(stat_server).
-behaviour(gen_event).

-export([init/1, handle_event/2, start_server/0]).

init([]) ->
    {ok, []}.

handle_event({stat_info, {Host, Name, Val}}, State) ->
    io:format("~s ~s: ~p~n", [Name, Host, Val]), 
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

start_server() ->
    {ok, _Pid} = gen_event:start_link({global, statsrv}),
    gen_event:add_handler({global, statsrv}, stat_server, []).
