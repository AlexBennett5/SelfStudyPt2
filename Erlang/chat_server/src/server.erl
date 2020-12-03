-module(server).
-export([start/0]).
-export([setup_server/1, dict_manager/1, listen/2, accept_connections/2, setup_user/2, client_loop/3, broadcast_message/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

setup_server(Portno) ->
  ClientDict = dict:new(),
  DictPid = spawn_link(fun() -> dict_manager(ClientDict) end),
  listen(Portno, DictPid).

dict_manager(ClientDict) ->
  receive
    {add_new_pair, ClientPid, ClientName} ->
      NewClientDict = dict:store(ClientPid, ClientName, ClientDict),
      dict_manager(NewClientDict);

    {remove_client, ClientPid} ->
      NewClientDict = dict:erase(ClientPid, ClientDict),
      dict_manager(NewClientDict);

    {get_client_name, ReceiverPid, ClientPid} ->
      {ok, ClientName} = dict:find(ClientPid, ClientDict),
      ReceiverPid ! {username, ClientName},
      dict_manager(ClientDict);

    {get_dict, ReceiverPid} ->
      ReceiverPid ! {client_dict, ClientDict},
      dict_manager(ClientDict);

    _ ->
      {error, "Invalid request"}

  end,
  dict_manager(ClientDict).

listen(Portno, DictPid) -> 
  case gen_tcp:listen(Portno, ?TCP_OPTIONS) of
    {ok, ListenSocket} -> 
      io:format("Listening on ~p~n", [ListenSocket]),
      accept_connections(ListenSocket, DictPid);
    {error, Error} ->
      io:format("Listen Error: ~w~n", [Error])
  end.

accept_connections(ListenSocket, DictPid) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, ClientSocket} ->
      io:format("Accepting:~w~n", [ClientSocket]),
      gen_tcp:send(ClientSocket, "Welcome! Enter your name\n"),
      ClientPid = spawn(fun() -> io:format("Client connected"),
                                 setup_user(ClientSocket, DictPid) end),
      gen_tcp:controlling_process(ClientSocket, ClientPid),
      accept_connections(ListenSocket, DictPid);
    {error, Error} ->
      io:format("Accept Error: ~w~n", [Error])
  end.

setup_user(ClientSocket, DictPid) ->
  {ok, Username} = gen_tcp:recv(ClientSocket, 0),
  DictPid ! {add_new_pair, ClientSocket, Username},
  EntranceMessage = "[" ++ process_string(Username) ++ " has entered the chat]\n", 
  broadcast_message(DictPid, EntranceMessage),
  client_loop(ClientSocket, Username, DictPid).

client_loop(ClientSocket, Username, DictPid) ->
  {ok, Message} = gen_tcp:recv(ClientSocket, 0),
  ProcessedMessage = process_string(Message),
  case string:equal(ProcessedMessage, "{quit}") of
    true ->
      gen_tcp:send(ClientSocket, "{quit}"),
      gen_tcp:close(ClientSocket),
      DictPid ! {remove_client, ClientSocket},
      LeaveMessage = "[" ++ process_string(Username) ++ " has left the chat]\n",
      broadcast_message(DictPid, LeaveMessage);
    
    false ->
      ChatMessage = "<" ++ process_string(Username) ++ "> " ++ ProcessedMessage ++ "\n",
      broadcast_message(DictPid, ChatMessage),
      client_loop(ClientSocket, Username, DictPid)    
  end.
  
broadcast_message(DictPid, Message) ->
  DictPid ! {get_dict, self()},
  receive
    {client_dict, Pids} ->
      ClientDict = Pids
  end,
  ClientPids = dict:fetch_keys(ClientDict),
  lists:map(fun (Pid) -> gen_tcp:send(Pid, Message) end, ClientPids).

process_string(Binary) ->
  string:trim(binary_to_list(Binary)).

start() ->
  setup_server(1234).
