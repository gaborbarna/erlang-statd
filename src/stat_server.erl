-module(stat_server).
-behaviour(gen_event).

-export([init/1, handle_event/2, start_server/0]).

init([]) ->
    {ok, []}.

display_stat({Name, Host, Val}) ->
    io:format("(~s) ~s: ~p~n", [Name, Host, Val]).
    
handle_event({stat_info, Stats}, State) ->
    lists:foreach(fun display_stat/1, Stats),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

start_server() ->
    {ok, _Pid} = gen_event:start_link({global, statsrv}),
    gen_event:add_handler({global, statsrv}, stat_server, []).
