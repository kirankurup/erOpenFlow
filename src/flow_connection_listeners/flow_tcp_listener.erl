%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2017 12:43
%%%-------------------------------------------------------------------
-module(flow_tcp_listener).
-author("kirankurup@gmail.com").

-behaviour(gen_server).

-include("logger.hrl").

%% External API
-export([start_link/3]).

%% gen_server callbacks
-export([
  init/1, handle_call/3,
  handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-record(state, {
  listener,         % Listening socket
  acceptor,         % Asynchronous acceptor's internal reference
  supervisor_name,  % SuperVisor name for the Module
  module            % FSM handling module
}).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Port, SupervisorRef, Module) when is_integer(Port), is_atom(Module) ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [Port, SupervisorRef, Module], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init ([Port, SupName, HandlerModule]) ->
  process_flag (trap_exit, true),
  Opts = [binary, {packet, 0}, {reuseaddr, true},
    {keepalive, true}, {backlog, 30}, {active, false}],
  case gen_tcp:listen (Port, Opts) of
    {ok, Listen_socket} ->
      %%Create first accepting process
      {ok, Ref} = prim_inet:async_accept (Listen_socket, -1),
      {ok, #state{listener = Listen_socket,
        acceptor = Ref,
        supervisor_name = SupName,
        module   = HandlerModule}};
    {error, Reason} ->
      {stop, Reason}
  end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call (Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast (_Msg, State) ->
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info ({inet_async, ListSock, Ref, {ok, CliSocket}},
    #state{listener=ListSock, acceptor=Ref, supervisor_name=SessionSupName, module=Module} = State) ->
  try
    case set_sockopt(ListSock, CliSocket) of
      ok ->
        ok;
      {error, Reason} ->
        exit({set_sockopt, Reason})
    end,

    %% New client connected - spawn a new process using the simple_one_for_one supervisor.
    {ok, Pid} = supervisor:start_child(SessionSupName, []),
    gen_tcp:controlling_process(CliSocket, Pid),
    %% Instruct the new FSM that it owns the socket.
    Module:set_socket(Pid, CliSocket),

    %% Signal the network driver that we are ready to accept another connection
    NewRef = case prim_inet:async_accept(ListSock, -1) of
               {ok, Refer} ->
                 Refer;
               {error, Refer} ->
                 exit({async_accept, inet:format_error(Refer)}),
                 Refer
             end,
    {noreply, State#state{acceptor=NewRef}}
  catch exit:Why ->
    ?LOG_ERROR ("Error in async accept: ~p.\n", [Why]),
    {stop, Why, State}
  end;

handle_info ({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
  ?LOG_ERROR ("Error in socket acceptor: ~p.\n", [Error]),
  {stop, Error, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
  gen_tcp:close (State#state.listener),
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt (ListSock, CliSocket) ->
  true = inet_db:register_socket (CliSocket, inet_tcp),
  case prim_inet:getopts (ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
      case prim_inet:setopts (CliSocket, Opts) of
        ok    ->
          ok;
        Error ->
          gen_tcp:close (CliSocket),
          Error
      end;
    Error ->
      gen_tcp:close (CliSocket),
      Error
  end.


