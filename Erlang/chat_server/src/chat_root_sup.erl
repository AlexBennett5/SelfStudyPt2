-module(chat_root_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Portno, MaxAcceptors) ->
  io:format("Starting root supervisor~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Portno, MaxAcceptors]).

init([Portno, MaxAcceptors]) ->
  AcceptSpec = {chat_accept_sup, {chat_accept_sup, start_link, [Portno, MaxAcceptors]}, transient, 5000, supervisor, [chat_accept_sup]},
  ServerSpec = {chat_genserv, {chat_genserv, start_link, []}, transient, 5000, worker, [chat_genserv]},
  {ok, {{one_for_one, 1, 5}, [AcceptSpec, ServerSpec]}}.
