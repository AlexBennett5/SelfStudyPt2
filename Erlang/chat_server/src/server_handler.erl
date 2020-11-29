-module(server_handler).
-export([listen/2]).

listen(Portno, DictPid) ->
  {ok, ListenSocket} = gen_tcp:listen(Portno, ?TCP_OPTIONS),
  spawn_link(fun() -> accept_connections(ListenSocket, DictPid)).

accept_connections(ListenSocket, DictPid) ->
  {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
  gen_tcp:send(ClientSocket, "Welcome! Enter your name~n"),
  spawn_link(fun() -> setup_user(ClientSocket, DictPid)),
  accept_connection(ListenSocket, DictPid).
  
setup_user(ClientSocket, DictPid) ->
  {ok, Username} = gen_tcp:recv(ClientSocket, 0),
  DictPid ! {add_new_pair, ClientSocket, Username},
  ClientDict = get_dict(),
  broadcast_message(dict:fetch_keys(ClientDict), "[" ++ Username ++ "has entered the chat]"),
  client_loop(ClientSocket, Username, DictPid).

client_loop(ClientSocket, Username, DictPid) ->
  {ok, Message} = gen_tcp:recv(ClientSocket, 0),
  ClientDict = get_dict(),
  if
    string:equal(Message, "{quit}") ->
      gen_tcp:send(ClientSocket, "{quit}"),
      gen_tcp:close(ClientSocket),
      DictPid ! {remove_client, ClientSocket}
      NewClientDict = get_dict(),
      broadcast_message(NewClientDict, "[" ++ Username ++ " has left the chat]")
    
    true ->
      broadcast_message(ClientDict, "<" ++ Username ++ "> " ++ Message),
      client_loop(ClientSocket, Username, DictPid);    
  end.
  
get_dict() ->
  DictPid ! {get_dict, self()},
  receive
    {client_dict, Pids} ->
      ClientDict = Pids
  end,
  ClientDict.

broadcast_message(ClientPid, Message) ->
  gen_tcp:send(ClientPid, Message);
broadcast_message([Pid|Rest], Message) ->
  gen_tcp:send(Pid, Message),
  broadcast_message(Rest, Message).
