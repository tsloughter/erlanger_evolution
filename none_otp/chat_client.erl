-module(chat_client).
-export([login/2, chat_loop/3, broadcast/2, message/3, logout/1]).

login(Server, UserName) ->
    Pid = spawn(?MODULE, chat_loop, [not_connected, Server, UserName]),
    Server ! {login, UserName, Pid},
    Pid.

chat_loop(not_connected, Server, UserName) ->
    receive
        success ->
            io:format("Connected!~n"),
            chat_loop(connected, Server, UserName);
        failure ->
            io:format("Failed to connect!~n")            
    end;
chat_loop(connected, Server, UserName) ->
    receive
        logout ->
            Server ! {logout, UserName};
        {broadcast, Message} ->
            Server ! {broadcast, UserName, Message},
     	    chat_loop(connected, Server, UserName);
        {send_message, To, Message} ->
            Server ! {message, To, UserName, Message},
     	    chat_loop(connected, Server, UserName);
     	{message, From, Message} ->
            io:format("~s : ~s~n", [From, Message]),
     	    chat_loop(connected, Server, UserName);
        Unknown ->
            io:format("What is this? ~p~n", [Unknown])
    end.

logout(Pid) ->
    Pid ! logout.

broadcast(Pid, Message) ->
    Pid ! {broadcast, Message}.

message(Pid, To, Message) ->
    Pid ! {send_message, To, Message}.
    
     
     
