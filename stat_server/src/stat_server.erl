-module(stat_server).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, start_server/0]).
-define(QUEUE_SIZE, 20).

init([]) ->
    ets:new(nodes, [named_table, ordered_set]),
    ets:new(handlers, [named_table, set]),
    {ok, []}.

store_node(Node) ->
    case ets:member(nodes, Node) of
        false -> ets:insert(nodes, {Node}),
                 ets:new(Node, [named_table, ordered_set]);
        true -> ok
    end.

store_stat_event(Node, Name, Suffix) ->
    case ets:member(Node, Name) of
        false -> ets:insert(Node, {Name, queue:new(), Suffix});
        true -> ok
    end.

get_current_time() ->
    {MegaS, S, MicroS} = now(),
    MegaS*1000*1000*1000*1000 + S*1000*1000 + MicroS.

store_stat(Node, Name, Val, Suffix, CurrentTime) ->
    store_node(Node),
    store_stat_event(Node, Name, Suffix),
    OldQueue = ets:lookup_element(Node, Name, 2),
    TempQueue = case queue:len(OldQueue) >= ?QUEUE_SIZE of
                    true -> {_, Q} = queue:out(OldQueue), Q;
                    false -> OldQueue
                end,
    NewQueue = queue:in([CurrentTime, Val], TempQueue),
    ets:update_element(Node, Name, {2, NewQueue}).

get_stats(Node, Resource) ->
    [{_, Stats, Suffix}|_] = ets:lookup(Node, Resource),
    {queue:to_list(Stats), Suffix}.

get_node_resource_atom(Node, Resource) ->
    list_to_atom(atom_to_list(Node) ++ atom_to_list(Resource)).

send_stat(Node, Name, Val, Suffix, CurrentTime) ->
    Stat = {[{node, Node}, {name, Name}, {val, [CurrentTime, Val]},
            {suffix, Suffix}]},
    ets:foldl(
      fun({Handler, ResourceSet}, _) ->
              NodeResource = get_node_resource_atom(Node, Name),
              case sets:is_element(NodeResource, ResourceSet) of
                  true -> Handler ! {stat_info, Stat};
                  false -> ok
              end
      end, [], handlers).

handle_event({stat_info, Node, Stats}, State) ->
    CurrentTime = get_current_time(),
    Handle = fun({Name, Val, Suffix}) ->
                     store_stat(Node, Name, Val, Suffix, CurrentTime),
                     send_stat(Node, Name, Val, Suffix, CurrentTime)
             end,
    lists:foreach(Handle, Stats),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_info({register_handler, Pid}, State) ->
    ets:insert(handlers, {Pid, sets:new()}),
    self() ! {getnodes, Pid},
    {ok, State};
handle_info({remove_handler, Pid}, State) ->
    ets:delete(handlers, Pid),
    {ok, State};
handle_info({getnodes, Pid}, State) ->
    Nodes = ets:foldl(
              fun({Node}, NodeList) ->
                      Resources = ets:foldl(
                                    fun({R, _, _}, ResList) ->
                                            [R|ResList]
                                    end, [], Node),
                      [{Node,Resources}|NodeList]
              end, [], nodes),
    Pid ! {refresh_nodes, {Nodes}},
    {ok, State};
handle_info({register_resource, Pid, Node, Resource}, State) ->
    update_resource_set(Pid, Node, Resource),
    {Stats, Suffix} = get_stats(Node, Resource),
    Pid ! {stats_init, {[{node, Node}, {resource, Resource}, 
                         {stats, Stats}, {suffix, Suffix}]}},
    {ok, State};
handle_info({remove_resource, Pid, Node, Resource}, State) ->
    NodeResource = get_node_resource_atom(Node, Resource),
    Set = ets:lookup_element(handlers, Pid, 2),
    NewSet = sets:del_element(NodeResource, Set),
    ets:update_element(handlers, Pid, {2, NewSet}),
    {ok, State}.

update_resource_set(Pid, Node, Resource) ->
    NodeResource = get_node_resource_atom(Node, Resource),
    Set = ets:lookup_element(handlers, Pid, 2),
    NewSet = sets:add_element(NodeResource, Set),
    ets:update_element(handlers, Pid, {2, NewSet}).

start_server() ->
    {ok, Pid} = gen_event:start_link({global, statsrv}),
    gen_event:add_handler({global, statsrv}, stat_server, []),
    {ok, Pid}.
