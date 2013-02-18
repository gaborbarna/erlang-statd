-module(stat_client).
-behaviour(gen_server).
-export([init/1, handle_info/2, terminate/2]).
-export([code_change/3, handle_call/3, handle_cast/2]).

init(Server) ->
    StatFns = [{fun commons:get_battery/0, 10000},
               {fun commons:get_thermal/0, 1000},
               {fun commons:get_cpuload/0, 1000},
               {fun commons:get_memory/0, 5000},
               {ifstat, 1000}],
    lists:map(fun({Fn, T}) -> timer:send_interval(T, Fn) end, StatFns),
    net_kernel:connect_node(Server),
    commons:init_os_mon(),
    {ok, {Server, _IfStat=commons:init_ifstat()}}.

notify_server(Stats) ->
    gen_event:notify({global, statsrv}, {stat_info, Stats}).

handle_info(ifstat, _State={Server, IfStat}) ->
    {Delta, NewIfStat} = commons:get_ifbytes(IfStat),
    Stats = [{node(), Name, Value} || {Name, Value} <- Delta],
    notify_server(Stats),
    {noreply, {Server, NewIfStat}};
handle_info(StatFn, State) ->
    Stats = [{node(), Name, Val} || {Name, Val} <- StatFn()],
    notify_server(Stats),
    {noreply, State}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _State) ->
    ok.

