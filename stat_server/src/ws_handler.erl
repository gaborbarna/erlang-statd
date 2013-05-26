-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _State) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, State={ServerPid}) ->
    ServerPid ! {register_handler, self()},
	{ok, Req, State}.

websocket_handle({text, Msg}, Req, State) ->
    {ok, Decoded} = json:decode(Msg),
    {Command, Params} = decode_json_message(Decoded),
    execute_command(Command, Params, State),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({Command, Params}, Req, State) ->
    {ok, JsonInfo} = json:encode({[{command, Command}, {params, Params}]}),
	{reply, {text, JsonInfo}, Req, State}.

websocket_terminate(_Reason, _Req, _State={ServerPid}) ->
    ServerPid ! {remove_handler, self()},
	ok.

get_json_value(List, Key) ->
    KeyBinary = atom_to_binary(Key, utf8),
    {_, ValBinary} = lists:keyfind(KeyBinary, 1, List),
    ValBinary.
    
decode_json_message(_Msg={MsgList}) ->
    Command = get_json_value(MsgList, command),
    Params = get_json_value(MsgList, params),
    {binary_to_atom(Command, utf8), Params}.

decode_node_resource_pair(ParamList) ->
    NodeBinary = get_json_value(ParamList, node),
    ResourceBinary = get_json_value(ParamList, resource),
    {binary_to_atom(NodeBinary, utf8), binary_to_atom(ResourceBinary, utf8)}.
    
execute_command(register_resource, {ParamList}, _State={ServerPid}) ->
    {Node, Resource} = decode_node_resource_pair(ParamList),
    ServerPid ! {register_resource, self(), Node, Resource};
execute_command(remove_resource, {ParamList}, _State={ServerPid}) ->
    {Node, Resource} = decode_node_resource_pair(ParamList),
    ServerPid ! {remove_resource, self(), Node, Resource};
execute_command(_Command, _Params, _State) ->
    ok.


