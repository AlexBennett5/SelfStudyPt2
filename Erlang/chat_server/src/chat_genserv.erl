-module(chat_genserv).
-behavior(gen_server).

-export([start_link/1, add_client/2, remove_client/1, broadcast_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/2, broadcast/2, retrieve_from_list/2, remove_from_list/2]).

-record(state, {clients}).
-record(client_info, {username, socket, pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Client API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_client(Username, Socket) ->
  gen_server:call(?MODULE, {add_client, Username, Socket}).

remove_client(Pid) ->
  gen_server:cast(?MODULE, {remove_client, Pid}).

broadcast_message(Pid, Message) ->
  gen_server:cast(?MODULE, {broadcast_message, Pid, Message}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
  {ok, #state{clients=[]}}.

handle_call({add_client, Username, Socket}, {Pid, Tag}, State) ->
  NewClient = #client_info{username=Username, socket=Socket, pid=Pid},
  NewState = State#state{clients=[NewClient|State#state.clients]},
  broadcast(State#state.clients, "[" ++ Username ++ " has entered the chat]\n"),
  {noreply, NewState}.

handle_cast({remove_client, Pid}, State) ->
  ClientBeingRemoved = retrieve_from_list(State#state.clients, Pid),
  NewClients = remove_from_list(State#state.clients, ClientBeingRemoved),
  NewState = State#state{clients=NewClients},
  broadcast(NewClients, "[" ++ ClientBeingRemove#client_info.username ++ " has left the chat]\n"),
  {noreply, NewState};
handle_cast({broadcast_message, Pid, Message}, State) ->
  ClientSendingMessage = retrieve_from_list(State#state.clients, Pid),
  ClientUsername = ClientSendingMessage#client_info.username,
  broadcast(State#state.clients, "<" ++ ClientUsername ++ "> " ++ Message),
  {noreply, State}.

handle_info(_E, _From, _State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

broadcast(Clients, Message) ->
  [gen_tcp:send(Client#client_info.socket, Message) || Client <- Clients].

retrieve_from_list(Clients, ClientPid) ->
  RetrievedClient = lists:filter(fun (Elem) -> Elem#client_info.pid =:= ClientPid, Clients),
  hd(RetrievedClient).

remove_from_list(Clients, ClientBeingRemoved) ->
  NewClients = Clients -- ClientBeindRemoved.

