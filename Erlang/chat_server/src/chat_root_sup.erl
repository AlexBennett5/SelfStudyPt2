-module(chat_root_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {modulename {modulename, startingfunction, args}, restart(transient), shutdownafter(5000), type(supervisor or worker), callbackmodule(by default should be modulename) }

  AcceptSpec = ,
  ServerSpec = ,
  {ok, {{one_for_one, 1, 5}, [AcceptSpec, ServerSpec]}}.
