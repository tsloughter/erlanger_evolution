-module(chat_server).
-export([main/0, server_loop/1]).

main() ->
    register(server_loop, spawn(?MODULE, server_loop, [[]])).

server_loop(UserList) ->
    receive
        {message, To, From, Message} ->
            case lists:keyfind(To, 1, UserList) of
                {To, Pid} ->
                    Pid ! {message, From, Message};
                false ->
                    no_user
            end;
        {broadcast, From, Message} ->
            lists:foreach(fun({UserName, Pid}) ->
                                  if 
                                      UserName =/= From ->
                                          Pid ! {message, From, Message};
                                      true ->
                                          nothing
                                  end
                          end, UserList);
        {logout, UserName} ->
            server_loop(lists:filter(fun({Name, _Pid}) ->
                                             Name =/= UserName
                                     end, UserList));
        {login, UserName, Pid} ->
            server_loop([{UserName, Pid}|UserList])                   
    end.

