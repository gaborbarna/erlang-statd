-module(stat_client).
-behaviour(gen_server).
-export([init/1, handle_info/2, terminate/2]).
-export([code_change/3, handle_call/3, handle_cast/2]).

init(Server) ->
    StatFns = [{fun commons:get_battery/0, 10000},
               {fun commons:get_thermal/0, 1000},
               {cpu_load, 1000},
               {memory, 5000}],
    lists:map(fun({Fn, T}) -> timer:send_interval(T, Fn) end, StatFns),
    net_kernel:connect_node(Server),
    init_os_mon(),
    {ok, Server}.

init_os_mon() ->
    application:start(sasl),
    application:start(os_mon).

notify_server(Stats) ->
    gen_event:notify({global, statsrv}, {stat_info, Stats}).

handle_info(cpu_load, State) ->
    Stats = [{node(), "CPU " ++ integer_to_list(Num), Val}
             || {Num, Val, _, _} <- cpu_sup:util([per_cpu])],
    notify_server(Stats),
    {noreply, State};
handle_info(memory, State) ->
    Data = memsup:get_system_memory_data(),
    Total = lists:keyfind(total_memory, 1, Data),
    Free = lists:keyfind(free_memory, 1, Data),
    Stats = [{node(), atom_to_list(Name), Val} 
             || {Name, Val} <- [Total, Free]],
    notify_server(Stats),
    {noreply, State};
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
