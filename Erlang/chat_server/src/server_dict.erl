-module(server_dict).
-export([start/0, run_server/1, stop_server/1, dict_manager/1]).

run_server(Portno) ->
  io:format("Server initializing~n"),
  ClientDict = dict:new(),
  DictPid = spawn_link(fun() -> dict_manager(ClientDict) end),
  server_handler:listen(Portno, DictPid).

stop_server(Socketno) ->
  gen_tcp:close(Socketno).

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

  end.

start() ->
  run_server(1234).
