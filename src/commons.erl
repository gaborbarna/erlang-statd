-module(commons).
-export([get_battery/0, get_thermal/0]).

get_battery() ->
    Output = os:cmd("acpi -b"),
    Format = fun(T) -> {X, _} = string:to_integer(strip_int(T)), X end,
    parse_acpi_output(Output, 4, Format).

get_thermal() ->
    Output = os:cmd("acpi -t"),
    Format = fun(T) -> {X, _} = string:to_float(T), X end,
    parse_acpi_output(Output, 4, Format).

nth_term(N, Line) ->
    Terms = re:split(Line, " ", [{return,list}]),
    lists:nth(N, Terms).

strip_int(Term) ->
    {_, [{Start, End}|_]} = re:run(Term, "[0-9]*"),
    lists:sublist(Term, Start+1, End).

construct_acpi_header(L) ->
    nth_term(1, L) ++ " " ++ strip_int(nth_term(2, L)).

parse_acpi_output(Data, Column, FormatFn) ->
    Lines = re:split(Data, "\\n", [{return,list}]),
    [{construct_acpi_header(L), FormatFn(nth_term(Column, L))}
     || L <- Lines, L =/= ""].

%% get_load({PrevIdle, PrevTotal}) ->
%%     {ok, F} = file:open("/proc/stat", read),
%%     {ok, Line} = file:read_line(F),
%%     Splitted = re:split(Line, "\s", [{return, list}]),
%%     file:close(F),
%%     {get_idle(Splitted), get_total(Splitted)}.
    
%% get_idle(Splitted) ->
%%     IdleT = [fun(L) -> lists:nth(6, L) end,
%%              fun(S) -> {N, _} = string:to_integer(S), N end],
%%     lists:foldl(fun(Fn, Acc) -> Fn(Acc) end, Splitted, IdleT).
    
%% get_total(Splitted) ->
%%     ToInt = fun(S) -> {N, _} = string:to_integer(S), N end,
%%     TotalT = [fun(L) -> lists:sublist(L, 3, 4) end,
%%               fun(L) -> lists:foldl(fun(X, Acc) -> ToInt(X) + Acc end,
%%                                     0, L) end],
%%     lists:foldl(fun(Fn, Acc) -> Fn(Acc) end, Splitted, TotalT).
    
