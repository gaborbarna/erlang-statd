-module(stat_client).
-behaviour(gen_server).
-export([init/1, handle_info/2, terminate/2]).
-export([code_change/3, handle_call/3, handle_cast/2]).

init([]) ->
    StatFns = [{fun commons:get_battery/0, 10000},
               {fun commons:get_thermal/0, 1000}],
    lists:map(fun({Fn, T}) -> timer:send_interval(T, Fn) end, StatFns),
    net_kernel:connect_node(lr@szalon),
    {ok, []}.

notify_server(Stat) ->
    gen_event:notify({global, statsrv}, {stat_info, Stat}).

handle_info(StatFn, State) ->
    Stats = [{node(), Name, Val} || {Name, Val} <- StatFn()],
    lists:foreach(fun notify_server/1, Stats),
    {noreply, State}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _State) ->
    ok.
