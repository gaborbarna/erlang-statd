-module(commons).
-export([get_battery/0, get_thermal/0, get_memory/0, get_cpuload/0]).
-export([get_ifbytes/1, init_ifstat/0, init_os_mon/0]).

init_os_mon() ->
    application:start(sasl),
    application:start(os_mon).

get_battery() ->
    Output = os:cmd("acpi -b"),
    Format = fun(T) -> {X, _} = string:to_integer(strip_int(T)), X end,
    parse_acpi_output(Output, 4, Format, "%").

get_thermal() ->
    Output = os:cmd("acpi -t"),
    Format = fun(T) -> {X, _} = string:to_float(T), X end,
    parse_acpi_output(Output, 4, Format, "C").

get_memory() ->
    Data = memsup:get_system_memory_data(),
    {_, Total} = lists:keyfind(total_memory, 1, Data),
    {_, Free} = lists:keyfind(free_memory, 1, Data),
    UsedStat = {'Used memory', Total - Free, 'KB'},
    FreeStat = {'Free memory', Free, 'KB'},
    [UsedStat, FreeStat].

get_cpuload() ->
     [create_stat("CPU "++integer_to_list(Num), Val, "%")
      || {Num, Val, _, _} <- cpu_sup:util([per_cpu])].

nth_term(N, Line) ->
    Terms = re:split(Line, " ", [{return,list}]),
    lists:nth(N, Terms).

strip_int(Term) ->
    {_, [{Start, End}|_]} = re:run(Term, "[0-9]*"),
    lists:sublist(Term, Start+1, End).

create_stat(Name, Value, Suffix) when is_list(Name), is_list(Suffix) ->
    {list_to_atom(Name), Value, list_to_atom(Suffix)}.

parse_acpi_output(Data, Column, FormatFn, Suffix) ->
    Lines = re:split(Data, "\\n", [{return,list}]),
    [create_stat(acpi_name(L), FormatFn(nth_term(Column, L)), Suffix)
     || L <- Lines, L =/= ""].

acpi_name(L) ->
    nth_term(1, L) ++ " " ++ strip_int(nth_term(2, L)).

init_ifstat() ->
    [{Name, nil, nil} || Name <- getiflist()].

getiflist() ->
    {ok, L} = inet:getiflist(),
    lists:filter(fun(If) -> re:run(If, "lo") =:= nomatch end, L).

get_ifbytes(IfStat) ->
    get_ifbytes(IfStat, [], []).

get_ifbytes([], Delta, Total) ->
    {Delta, Total};
get_ifbytes([IfStat|RestIf], Delta, Total) ->
    {If, PrevRx, PrevTx} = IfStat,
    Rx = read_iface_stat(If, "rx_bytes"),
    Tx = read_iface_stat(If, "tx_bytes"),
    DRx = if PrevRx == nil -> 0; true -> Rx - PrevRx end,
    DTx = if PrevTx == nil -> 0; true -> Tx - PrevTx end,
    DeltaRxStat = create_stat(If++" rx", DRx, "B"),
    DeltaTxStat = create_stat(If++" tx", DTx, "B"),
    NewDelta = [DeltaRxStat|[DeltaTxStat|Delta]],
    get_ifbytes(RestIf, NewDelta, [{If, Rx, Tx}|Total]).

read_iface_stat(Iface, File) ->
    {ok, F} = file:open("/sys/class/net/"++Iface++"/statistics/"++File, read),
    {ok, Line} = file:read_line(F),
    file:close(F),
    {N, _} = string:to_integer(Line),
    N.

