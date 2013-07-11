%% Messages from Client to server
-record(logon, {client_pid, username}).
-record(message, {client_pid, to_name, message}).
%% {'EXIT', ClientPid, Reason} - client terminated or unreachable

%% Messages from Server to Client received in await_result/0
%% Messages are:
%%  user_exists_at_other_node,
%%  you_are_not_logged_on
-record(abort_client, {message}).
%% Messages are:
%%  logged_on,
%%  receiver_not_found
%%  sent
-record(server_reply, {message}).

%% Messages from Server to Client received in client/1
-record(message_from, {from_name, message}).

%% Messages from shell to Client received in client/1
-record(message_to, {to_name, message}).
%% logoff
