-module(server_handler).
-export([listen/2, accept_connections/2, setup_user/2, client_loop/3, get_dict/1, broadcast_message/2]).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

listen(Portno, DictPid) -> 
  {ok, ListenSocket} = gen_tcp:listen(Portno, ?TCP_OPTIONS),
  io:format("Server is listening~n"),
  spawn_link(fun() -> accept_connections(ListenSocket, DictPid) end).

accept_connections(ListenSocket, DictPid) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
  gen_tcp:send(ClientSocket, "Welcome! Enter your name~n"),
  ClientPid = spawn(fun() -> setup_user(ClientSocket, DictPid) end),
  gen_tcp:controlling_process(ClientSocket, ClientPid),
  accept_connections(ListenSocket, DictPid).
  
setup_user(ClientSocket, DictPid) ->
  {ok, Username} = gen_tcp:recv(ClientSocket, 0),
  DictPid ! {add_new_pair, ClientSocket, Username},
  ClientDict = get_dict(DictPid),
  broadcast_message(dict:fetch_keys(ClientDict), "[" ++ Username ++ "has entered the chat]"),
  client_loop(ClientSocket, Username, DictPid).

client_loop(ClientSocket, Username, DictPid) ->
  {ok, Message} = gen_tcp:recv(ClientSocket, 0),
  ClientDict = get_dict(DictPid),
  case string:equal(Message, "{quit}") of
    true ->
      gen_tcp:send(ClientSocket, "{quit}"),
      gen_tcp:close(ClientSocket),
      DictPid ! {remove_client, ClientSocket},
      NewClientDict = get_dict(DictPid),
      broadcast_message(NewClientDict, "[" ++ Username ++ " has left the chat]");
    
    false ->
      broadcast_message(ClientDict, "<" ++ Username ++ "> " ++ Message),
      client_loop(ClientSocket, Username, DictPid)    
  end.
  
get_dict(DictPid) ->
  DictPid ! {get_dict, self()},
  receive
    {client_dict, Pids} ->
      ClientDict = Pids
  end,
  ClientDict.

broadcast_message([Pid|Rest], Message) ->
  gen_tcp:send(Pid, Message),
  broadcast_message(Rest, Message);
broadcast_message(ClientPid, Message) ->
  gen_tcp:send(ClientPid, Message).
