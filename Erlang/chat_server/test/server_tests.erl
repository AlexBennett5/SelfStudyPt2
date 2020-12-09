-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

get_msg(Socket, Message) ->
  gen_tcp:send(Socket, Message).

canary_test() ->
  ?assertEqual(1, 1).

meck_test() ->
  meck:new(gen_tcp, [unstick]),
  meck:expect(gen_tcp, send, fun(Socket, Message) when Socket =:= 10 ->
                                  Message;
                                (Socket, Message) ->
                                  erlang:error(invalid_socket)
                             end),
  ?assertEqual("Hello", get_msg(10, "Hello")),
  ?assert(meck:validate(gen_tcp)),
  meck:unload(gen_tcp).
