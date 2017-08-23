%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2017 14:54
%%%-------------------------------------------------------------------
-module(flow_tcp_fsm).
-author("kirankurup@gmail.com").

-behaviour(gen_fsm).

-include("logger.hrl").
-include("ofproto_common.hrl").

%% API
-export([start_link/1, set_socket/2, send_outgoing_data/2]).

%% gen_fsm callbacks
-export([init/1,
  state_name/2,
  state_name/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

%% FSM States
-export([
  'WAIT_FOR_SOCKET'/2,
  'WAIT_FOR_DATA'/2
]).

-define(TIMEOUT, 120000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_fsm:start_link(?MODULE, Args, []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
  gen_fsm:send_event(Pid, {socket_ready, Socket}).

send_outgoing_data (Pid, Data) when is_pid(Pid) ->
  gen_fsm:send_event(Pid, {outgoing_data, Data}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #?SWITCH_TCP_STATE{}} |
  {ok, StateName :: atom(), StateData :: #?SWITCH_TCP_STATE{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Version, CallbackModule, StorageType]) ->
  process_flag(trap_exit, true),
  ?LOG_INFO("test:~p~n", [Version]),
  {ok, Pid} = flow_switch_handler_fsm:start_link({Version, CallbackModule, StorageType, self()}),
  {ok, 'WAIT_FOR_SOCKET', #?SWITCH_TCP_STATE{switch = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), State :: #?SWITCH_TCP_STATE{}) ->
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{}} |
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #?SWITCH_TCP_STATE{}}).
state_name(_Event, State) ->
  {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), From :: {pid(), term()},
    State :: #?SWITCH_TCP_STATE{}) ->
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{}} |
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #?SWITCH_TCP_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #?SWITCH_TCP_STATE{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #?SWITCH_TCP_STATE{}}).
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #?SWITCH_TCP_STATE{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #?SWITCH_TCP_STATE{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #?SWITCH_TCP_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #?SWITCH_TCP_STATE{}}).
handle_event(Event, StateName, State) ->
  ?LOG_ERROR ("Received Event ~p in State ~p", [Event, StateName]),
  {stop, {StateName, undefined_event, Event}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(Event, _From, StateName, State) ->
  ?LOG_ERROR ("Received Sync Event ~p in State ~p", [Event, StateName]),
  {stop, {StateName, undefined_event, Event}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info({tcp, Socket, Bin}, StateName, #?SWITCH_TCP_STATE{socket=Socket} = State) ->
  % Flow control: enable forwarding of next TCP message
  inet:setopts(Socket, [{active, once}]),
  ?MODULE:StateName({data, Bin}, State);

handle_info({tcp_closed, Socket}, _StateName,
    #?SWITCH_TCP_STATE{socket=Socket, ip_address = Ip, port = Port} = State) ->
  ?LOG_INFO ("TCP Closed on Socket(~p), from Address ~p:~p ~n", [Socket, Ip, Port]),
  %% TODO: Need to disconnect the application FSM also. May be we can send terminate from flow_switch_handler_fsm.
  flow_switch_handler_fsm:stop_handler(State#?SWITCH_TCP_STATE.switch),
  {stop, normal, State};

handle_info(_Info, StateName, State) ->
  ?LOG_ERROR ("Unknown Info ~p from Address ~p:~p on State: ~p~n", [_Info, State#?SWITCH_TCP_STATE.ip_address,
    State#?SWITCH_TCP_STATE.port, StateName]),
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, #?SWITCH_TCP_STATE{socket=Socket}) ->
  (catch gen_tcp:close(Socket)),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #?SWITCH_TCP_STATE{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #?SWITCH_TCP_STATE{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
  % Now we own the socket
  inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
  {ok, {IP, Port}} = inet:peername(Socket),
  ?LOG_INFO ("IP ~p and Port: ~p~n", [IP, Port]),
  %% Send Hello
  flow_switch_handler_fsm:send_hello(State#?SWITCH_TCP_STATE.switch),
  {next_state, 'WAIT_FOR_DATA', State#?SWITCH_TCP_STATE{socket=Socket, ip_address = IP, port = Port}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
  ?LOG_WARN ("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
  %% Allow to receive async messages
  {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, State) ->
  ?LOG_INFO ("Incoming Data ~p~n", [Data]),
  flow_switch_handler_fsm:handle_msg(State#?SWITCH_TCP_STATE.switch, {incoming, Data}, async),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'({outgoing_data, Data}, State) ->
  ?LOG_INFO("sending outgoing message : ~p~n",[Data]),
  gen_tcp:send(State#?SWITCH_TCP_STATE.socket, Data),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
  ?LOG_ERROR ("Client connection timeout - closing.\n", []),
  {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
  ?LOG_INFO ("Ignoring data: ~p\n", [self(), Data]),
  {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

