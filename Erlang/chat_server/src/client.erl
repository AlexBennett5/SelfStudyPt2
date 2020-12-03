-module(client).

-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).

connect_to(Portno) ->
  case gen_tcp:connect("localhost", Portno, ?TCP_OPTIONS) of

    {ok, Socket} ->
      spawn(fun() -> receive_loop(Socket) end);

    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

receive_loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    
    {ok, Packet} ->
      String = binary_to_term(Packet),
      io:format("~p~n", [String]),
      receive_loop(Socket);

    {error, Reason} ->
      io:format("Error: ~p~n", [Reason])
  end.

send_message(Socket, Message) ->
  gen_tcp:send(Socket, Message).
