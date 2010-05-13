%%%-------------------------------------------------------------------
%%% File    : chat_server.erl
%%% Author  : Tristan Sloughter <>
%%% Description : 
%%%
%%% Created : 13 May 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(chat_server).

-behaviour(gen_server).

%% API
-export([start_link/0, login/2, logout/1, message/3, broadcast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {userlist=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
login(Pid, UserName) ->
    gen_server:call(?SERVER, {login, Pid, UserName}).

logout(UserName) ->
    gen_server:call(?SERVER, {logout, UserName}).

message(To, From, Message) ->
    gen_server:cast(?SERVER, {message, To, From, Message}).

broadcast(From, Message) ->
    gen_server:cast(?SERVER, {broadcast, From, Message}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({login, Pid, UserName}, _From, State) ->
    NewUserList = [{UserName, Pid}|State#state.userlist],
    {reply, success, State#state{userlist=NewUserList}};
handle_call({logout, UserName}, _From, State) ->
    UserList = State#state.userlist,
    NewUserList = lists:filter(fun({Name, _Pid}) ->
                                       Name =/= UserName
                               end, UserList),
    {reply, success, State#state{userlist=NewUserList}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({message, To, From, Message}, State) ->
    UserList = State#state.userlist,
    case lists:keyfind(To, 1, UserList) of
        {To, Pid} ->
            chat_client:message(Pid, From, Message);
        false ->
            no_user
    end,
    {noreply, State};
handle_cast({broadcast, From, Message}, State) ->
    UserList = State#state.userlist,
    lists:foreach(fun({UserName, Pid}) ->
                          if 
                              UserName =/= From ->
                                  chat_client:message(Pid, From, Message);
                              true ->
                                  nothing
                          end
                  end, UserList),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
