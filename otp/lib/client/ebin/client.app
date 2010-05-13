%% This is the application resource file (.app file) for the client,
%% application.
{application, client, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [client_app,
              client_sup,
             
              chat_client]},
   {registered,[client_sup]},
   {applications, [kernel, stdlib]},
   {mod, {client_app,[]}},
   {start_phases, []}]}.

