-module(stat_server).
-behaviour(gen_event).

-export([init/1, handle_event/2, start_server/0]).

init([]) ->
    {ok, []}.

display_stat(Node, Name, Val, Suffix) ->
    io:format("(~p) ~p: ~p ~p~n", [Node, Name, Val, Suffix]).
    
handle_event({stat_info, Node, Stats}, State) ->
    Display = fun({Name, Val, Suffix}) ->
                      display_stat(Node, Name, Val, Suffix)
              end,
    lists:foreach(Display, Stats),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

start_server() ->
    {ok, _Pid} = gen_event:start_link({global, statsrv}),
    gen_event:add_handler({global, statsrv}, stat_server, []).
