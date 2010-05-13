%% This is the application resource file (.app file) for the chat_server,
%% application.
{application, chat_server, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [chat_server_app,
              chat_server_sup,

             chat_server]},
   {registered,[chat_server_sup]},
   {applications, [kernel, stdlib]},
   {mod, {chat_server_app,[]}},
   {start_phases, []}]}.

