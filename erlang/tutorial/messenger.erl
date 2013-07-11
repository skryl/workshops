-module(messenger).
-export([start_server/0, server/1, logon/1, logoff/0, message/2, client/2]).

server_node() ->
  'messenger@Alexs-MacBook-Air'.

%% Server

start_server() ->
  io:format("Spawned server at ~p~n", [self()]),
  register(messenger, spawn(messenger, server, [[]])).

server(User_List) ->
  process_flag(trap_exit, true),
  receive
    {From, logon, Name} ->
      io:format("Logged On: ~p@~p~n", [Name, From]),
      New_User_List = server_logon(From, Name, User_List),
      server(New_User_List);
    {'EXIT', From, _} ->
      io:format("Logged Off: ~p~n", [From]),
      New_User_List = server_logoff(From, User_List),
      server(New_User_List);
    {From, message_to, To, Message} ->
      io:format("Message: ~p, From: ~p, To: ~p~n", [Message, From, To]),
      server_transfer(From, To, Message, User_List),
      server(User_List)
  end.

server_logon(From, Name, User_List) ->
  case lists:keymember(Name, 2, User_List) of
    true ->
      From ! {messenger, stop, user_exists_at_other_node},
      User_List;
    false ->
      From ! {messenger, logged_on},
      link(From),
      [{From, Name} | User_List]
  end.

server_logoff(From, User_List) ->
  lists:keydelete(From, 1, User_List).

server_transfer(From, To, Message, User_List) ->
  case lists:keysearch(From, 1, User_List) of
    false ->
      From ! {messenger, stop, you_are_not_logged_on};
    {value, {_, Name}} ->
      server_transfer(From, Name, To, Message, User_List)
  end.

server_transfer(From, Name, To, Message, User_List) ->
  case lists:keysearch(To, 2, User_List) of
    false ->
      From ! {messenger, receiver_not_found};
    {value, {ToPid, To}} ->
      ToPid ! {message_from, Name, Message},
      From ! {messenger, sent}
  end.

%% Client

client(Server_Node, Name) ->
  {messenger, Server_Node} ! {self(), logon, Name},
  await_result(),
  client(Server_Node).

client(Server_Node) ->
  receive
    logoff ->
      exit(normal);
    {message_to, ToName, Message} ->
      {messenger, Server_Node} ! {self(), message_to, ToName, Message},
      await_result();
    {message_from, FromName, Message} ->
      io:format("Message from ~p: ~p~n", [FromName, Message])
  end,
  client(Server_Node).

await_result() ->
  receive
    {messenger, stop, Why} ->
      io:format("~p~n", [Why]),
      exit(normal);
    {messenger, What} ->
      io:format("~p~n", [What])
  after 5000 ->
    io:format("No response from server~n", []),
    exit(timeout)
  end.

%% User Commands

logon(Name) ->
  case whereis(mess_client) of
    undefined ->
      register(mess_client, spawn(messenger, client, [server_node(), Name]));
    _ -> already_logged_on
  end.

logoff() ->
  mess_client ! logoff.

message(ToName, Message) ->
  case whereis(mess_client) of
    undefined ->
      not_logged_on;
    _ -> mess_client ! {message_to, ToName, Message},
      ok
  end.
