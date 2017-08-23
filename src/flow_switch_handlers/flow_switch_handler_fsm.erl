%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2017 11:08
%%%-------------------------------------------------------------------
-module(flow_switch_handler_fsm).
-author("kirankurup@gmail.com").

-behaviour(gen_fsm).

-include("logger.hrl").
-include("ofproto_common.hrl").

%% API
-export([start_link/1, send_hello/1,
  send_outgoing_msg/2, send_outgoing_msg/3,
  handle_msg/3, stop_handler/1]).

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
  'INIT'/2,
  'SETUP'/2,
  'CONNECTED'/2,
  'TERMINATE'/2
]).

%%%===================================================================
%%% User callback declarations
%%%===================================================================

-callback handle_incoming (Args) -> term()
  when Args :: {SwitchId, MsgType, MsgList},
  SwitchId :: term(),
  MsgType :: atom(),
  MsgList :: list().
-optional_callbacks([handle_incoming/1]).

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
-spec(start_link(term()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
  gen_fsm:start_link(?MODULE, [Args], []).

%%--------------------------------------------------------------------
%% @doc
%% Create OFPT_HELLO Message specifying the versions supported by self,
%% and send it to the switch.
%%
%% @end
%%--------------------------------------------------------------------
-spec(send_hello(pid()) -> ok).
send_hello(Pid) when is_pid(Pid) ->
  gen_fsm:send_event(Pid, send_hello).

%%--------------------------------------------------------------------
%% @doc
%% Create an OFP Msg and send it across to the switch,
%% using main connection, unless specified.
%%
%% @end
%%--------------------------------------------------------------------
send_outgoing_msg (Pid, Msg) ->
  send_outgoing_msg(Pid, Msg, true).

send_outgoing_msg (Pid, Msg, UseMainConnection) ->
  handle_msg(Pid, {outgoing, Msg, UseMainConnection}, async).
%%--------------------------------------------------------------------
%% @doc
%% Handle the message from the connection module and respond io it appropriately.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_msg(pid(), term(), atom()) -> ok | term()).
handle_msg(Pid, Msg, sync) when is_pid(Pid) ->
  %% TODO: Added to handle OFPT_ECHO_REQUEST. Do we really need this, or can it be done without sync?
  gen_fsm:sync_send_event(Pid, Msg);

handle_msg(Pid, Msg, async) when is_pid(Pid) ->
  gen_fsm:send_event(Pid, Msg).

%%--------------------------------------------------------------------
%% @doc
%% Stop the FSM idenitified by the argument.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop_handler(pid()) -> ok).
stop_handler(Pid) when is_pid(Pid) ->
  gen_fsm:stop(Pid).

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
  {ok, StateName :: atom(), StateData :: #?SWITCH_STATE{}} |
  {ok, StateName :: atom(), StateData :: #?SWITCH_STATE{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([{Version, CallbackModule, StorageType, Connector}]) ->
  {ok, 'INIT', #?SWITCH_STATE{supported_versions = Version, callback_module = CallbackModule,
    connection_listeners = [Connector], main_connection = Connector, switch_storage_type = StorageType}}.

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
-spec(state_name(Event :: term(), State :: #?SWITCH_STATE{}) ->
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_STATE{}} |
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #?SWITCH_STATE{}}).
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
    State :: #?SWITCH_STATE{}) ->
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_STATE{}} |
  {next_state, NextStateName :: atom(), NextState :: #?SWITCH_STATE{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #?SWITCH_STATE{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #?SWITCH_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #?SWITCH_STATE{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #?SWITCH_STATE{}}).
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
    StateData :: #?SWITCH_STATE{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #?SWITCH_STATE{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #?SWITCH_STATE{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #?SWITCH_STATE{}}).
handle_event(_Event, StateName, State) ->
  ?LOG_ERROR ("Received Event ~p in State ~p", [_Event, StateName]),
  {stop, {StateName, undefined_event, _Event}, State}.

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
handle_sync_event(_Event, _From, StateName, State) ->
  ?LOG_ERROR ("Received Sync Event ~p in State ~p", [_Event, StateName]),
  {stop, {StateName, undefined_event, _Event}, State}.

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
handle_info(_Info, StateName, State) ->
  ?LOG_ERROR ("Unknown Info ~p on State: `p~n", [_Info, StateName]),
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
terminate(_Reason, _StateName, State) ->
  ?LOG_ERROR("Reason: ~p: StateName: ~p: State: ~p~n", [_Reason, _StateName, lager:pr(State, ofproto_common)]),
  case State#?SWITCH_STATE.switch_id of
    undefined ->
      ok;
    SwitchId ->
      Pid = self(),
      flow_switch:unregister_switch(SwitchId, Pid, State#?SWITCH_STATE.switch_storage_type),
      ?LOG_INFO ("Unregistering self (~p) with SwitchId: ~p~n", [Pid, SwitchId])
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #?SWITCH_STATE{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #?SWITCH_STATE{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
'INIT'(send_hello, State) ->
  %% Send OFPT_HELLO
  ?LOG_INFO ("Sending Hello with Version: ~p~n", [State#?SWITCH_STATE.supported_versions]),
  Xid = incr_xid(State),
  case create_hello(State, Xid) of
    {ok, HelloPkt} ->
      %% TODO: We need to send on auxiliary connections also
      flow_tcp_fsm:send_outgoing_data(State#?SWITCH_STATE.main_connection, HelloPkt),
      {next_state, 'SETUP', State#?SWITCH_STATE{xid = Xid}};
    {error, Reason} ->
      ?LOG_ERROR ("Error (~p) while creating OFP HELLO~n", [Reason]),
      {stop, <<"OFP_HELLO_CREATION_FAILURE">>, State}
  end;

'INIT'(_Event, State) ->
  ?LOG_ERROR("Unhandled Event: ~p in INIT State, Ignore it???~n", [_Event]),
  {next_state, 'INIT', State}.

'SETUP'({incoming, Msg}, State) ->
  ?LOG_INFO ("Incoming Msg: ~p in SETUP State~n", [Msg]),
  %% Handle only Hello here
  case flow_packet:decode(Msg) of
    {ok, OFPMsg} ->
      check_hello_version(OFPMsg, State);
    {error, _Reason, _Xid} ->
      %% Handle error reason with Xid to send the error back
      handle_error_in_decode(_Reason, 'SETUP', State);
    {error, _Reason} ->
      %% Handle error reason without Xid to send the error back
      handle_error_in_decode(_Reason, 'SETUP', State)
  end;

'SETUP'(_Event, State) ->
  ?LOG_ERROR("Unhandled Event: ~p in SETUP State, Ignore it???~n", [_Event]),
  {next_state, 'SETUP', State}.

'CONNECTED'({outgoing, {OFPMsgType, OfpMsgArgs} = _Msg, _UseMainConnection}, #?SWITCH_STATE{parser = Parser} = State) ->
  %?LOG_INFO ("Incoming Msg: ~p in CONNECTED State~n", [Msg]),
  Xid = incr_xid(State),
  %% TODO: Send out always using main connection.
  send_message(Xid, Parser, State#?SWITCH_STATE.main_connection, OFPMsgType, OfpMsgArgs),
  NewState = State#?SWITCH_STATE{xid = Xid},
  {next_state, 'CONNECTED', NewState};

'CONNECTED'({incoming, Msg}, #?SWITCH_STATE{parser = Parser} = State) ->
  %?LOG_INFO ("Incoming Msg: ~p in CONNECTED State~n", [Msg]),
  NewState = case ofproto_parser:decode(Parser, Msg) of
               {error, Reason} ->
                 ?LOG_ERROR ("Error (~p) in decoding Msg ~p~n", [Reason, Msg]),
                 State;
               {ok, DecMsg} ->
                 case ofproto_parser:invoke(Parser, get_msg_type, {DecMsg}) of
                   {ok, MsgType} ->
                     MyState = handle_decoded_message (DecMsg, MsgType, State),
                     invoke_callback(MsgType, DecMsg, MyState),
                     MyState;
                   {error, TypeError} ->
                     ?LOG_ERROR ("Error (~p) while retrieving message type for Message ~p~n", [TypeError, lager:pr(DecMsg, ofproto_common)]),
                     State
                 end
             end,
  {next_state, 'CONNECTED', NewState};

'CONNECTED'(_Event, State) ->
  ?LOG_ERROR("Unhandled Event: ~p in CONNECTED State, Ignore it???~n", [_Event]),
  {next_state, 'CONNECTED', State}.

'TERMINATE'(Msg, State) ->
  ?LOG_INFO ("Incoming Msg: ~p in TERMINATE State~n", [Msg]),
  {next_state, 'SETUP', State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create OFP_HELLO packet based on the version supported.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_hello(State :: #?SWITCH_STATE{}, Xid :: integer()) ->
  {ok, binary()} | {error, Reason :: term()}).
create_hello(State, Xid) ->
  MaxVersion = lists:max (State#?SWITCH_STATE.supported_versions),
  HelloMsg = case MaxVersion >= ?OFP_VERSION_V_1_4 of
               true ->
                 %% hello_elements might be present
                 #?OFP_MSG{header = #?OFP_HEADER{version = MaxVersion, xid = Xid},
                   msg = #?OFP_HELLO{elements = [#?OFPHE_VERSIONBITMAP{type = ?OFPHET_VERSIONBITMAP,
                     bitmaps = State#?SWITCH_STATE.supported_versions}]}};
               false ->
                 %% No hello_elements
                 #?OFP_MSG{header = #?OFP_HEADER{version = MaxVersion, xid = Xid}, msg = #?OFP_HELLO{}}
             end,
  case ?PROTO_MOD(MaxVersion) of
    unsupported_version ->
      {error, unsupported_version};
    Module ->
      Module:encode (HelloMsg)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle error_codes from decode.
%% Returns the next state to which the FSM should move.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_error_in_decode(Reason :: term(), CurState :: atom(), State :: #?SWITCH_STATE{}) -> term()).
handle_error_in_decode (Reason, CurState, State) ->
  ?LOG_ERROR ("Reason: ~p, CurState : ~p, State: ~p~n", [Reason, CurState, State]),
  case Reason of
    unknown_packet ->
      {next_state, CurState, State};
    _ ->
      gen_fsm:stop(State#?SWITCH_STATE.main_connection),
      {stop, Reason, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle error_codes from decode.
%% Returns the next state to which the FSM should move.
%%
%% @end
%%--------------------------------------------------------------------
-spec(check_hello_version(OFPMsg :: ofp_msg(), State :: #?SWITCH_STATE{}) ->
  term()).
check_hello_version(#?OFP_MSG{header = #?OFP_HEADER{version = Version, xid = Xid}, msg = #?OFP_HELLO{}} = OFPMsg, State) ->
  SupportedVersions = State#?SWITCH_STATE.supported_versions,
  case lists:member(Version, SupportedVersions) of
    true ->
      handle_success_version_match(Version, State);
    false ->
      ErrorData = <<"No common versions.">>,
      %% Check whether we have Hello Elements.
      case Version >= ?OFP_VERSION_V_1_4 of
        true ->
          #?OFP_MSG{msg = #?OFP_HELLO{elements = #?OFPHE_VERSIONBITMAP{bitmaps = BitMap}}} = OFPMsg,
          CommonVersions = [V || V <- BitMap, lists:member(V, SupportedVersions)],
          %% check whether we support the version and decide on the version to be used.
          case CommonVersions of
            [] ->
              %% Send unsupported version Error
              create_error_and_send(lists:max(SupportedVersions), hello_failure, incompatible_verions,
                ErrorData, Xid, State#?SWITCH_STATE.main_connection),
              gen_fsm:stop(State#?SWITCH_STATE.main_connection),
              {stop, {shutdown,incompatible_verions}, State};
            _ ->
              handle_success_version_match(lists:max(CommonVersions), State)
          end;
        false ->
          %% Send unsupported version Error
          create_error_and_send(lists:max(SupportedVersions), hello_failure, incompatible_verions,
            ErrorData, Xid, State#?SWITCH_STATE.main_connection),
          gen_fsm:stop(State#?SWITCH_STATE.main_connection),
          {stop, {shutdown, incompatible_verions}, State}
      end
  end;

check_hello_version(_, State) ->
  %% Discard all other messages. Wait till we receive OFPT_HELLO.
  {next_state, 'SETUP', State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles successful negotiation of OFP Versions.
%% Creates a parser module, send OFPT_FEATURES_REQUEST
%%
%% @end
%%--------------------------------------------------------------------
handle_success_version_match(Version, State) ->
  case ofproto_parser:create(Version) of
    {ok, Parser} ->
      Xid = incr_xid(State),
      % Create OFPT_FEATURES_REQUEST and send it across the connection.
      send_message(Xid, Parser, State#?SWITCH_STATE.main_connection, ofp_features_request, <<>>),
      %create_features_request_and_send(Parser, Xid, State#?SWITCH_STATE.main_connection),
      NewState = State#?SWITCH_STATE{parser = Parser, xid = Xid},
      {next_state, 'CONNECTED', NewState};
    {error, Reason} ->
      ?LOG_ERROR ("Error in creating Parser module ~p~n", [Reason]),
      gen_fsm:stop(State#?SWITCH_STATE.main_connection),
      {stop, Reason, State}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the encoded Error Message and send to switch
%%
%% @end
%%--------------------------------------------------------------------
create_error_and_send(Version, ErrType, ErrCode, Data, Xid, Connection) ->
  case create_error(Version, ErrType, ErrCode, Data, Xid) of
    {error, Reason} ->
      ?LOG_ERROR ("Error ~p while creating OFP ERROR ~n", [Reason]);
    {ok, EncMsg} ->
      flow_tcp_fsm:send_outgoing_data(Connection, EncMsg)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the encoded Error Message.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_error(Version :: non_neg_integer(), ErrType :: atom(),
    ErrCode :: atom(), Data :: binary(), Xid :: non_neg_integer())
      -> {ok, binary()} | {error, term()}).
create_error(Version, ErrType, ErrCode, Data, Xid) ->
  case ?PROTO_MOD(Version) of
    unsupported_version ->
      {error, unsupported_version};
    Module ->
      case Module:create_error_msg (ErrType, ErrCode, Data) of
        {ok, ErrRec} ->
          ErrMsg = #?OFP_MSG{header = #?OFP_HEADER{version = Version, xid = Xid}, msg = ErrRec},
          case Module:encode (ErrMsg) of
            {ok, EncErrMsg} ->
              {ok, EncErrMsg};
            Err ->
              Err
          end;
        {error, ErrReason} ->
          {error, ErrReason}
      end
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Increment Transaction Id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(incr_xid(State :: #?SWITCH_STATE{}) -> integer()).
incr_xid(State) ->
  State#?SWITCH_STATE.xid + 1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles Incoming message.
%%
%% @end
%%--------------------------------------------------------------------
-spec (handle_decoded_message(ofp_msg(), atom(), #?SWITCH_STATE{}) -> #?SWITCH_STATE{}).
handle_decoded_message(#?OFP_MSG{} = Msg, ofp_features_reply, State) ->
  ?LOG_INFO ("Received ofp_features_reply ~n", []),
  FeatureList = ofproto_parser:invoke(State#?SWITCH_STATE.parser, convert_rec_to_proplist, Msg),
  SwitchId = proplists:get_value(datapath_str, FeatureList),
  Pid = self(),
  flow_switch:register_switch(SwitchId, Pid, State#?SWITCH_STATE.switch_storage_type),
  ?LOG_INFO ("Registering self (~p) with SwitchId: ~p~n", [Pid, SwitchId]),
  State#?SWITCH_STATE{switch_id = SwitchId};

handle_decoded_message(#?OFP_MSG{header = Header, msg = OfpEchoBody}, ofp_echo_request, State) ->
  ?LOG_INFO ("Received ofp_echo_request ~n", []),
  %% Send ofp_echo_reply back.
  Xid = Header#?OFP_HEADER.xid,
  Parser = State#?SWITCH_STATE.parser,
  Connection = State#?SWITCH_STATE.main_connection,
  case send_message(Xid, Parser, Connection, ofp_echo_reply, OfpEchoBody) of
    {ok, _EncOFPReply} ->
      State;
    {error, _Reason} ->
      State
  end;

handle_decoded_message(_OtherMsg, _OtherType, State) ->
  ?LOG_INFO("No FSM handling for Msg of type: ~p ~n", [_OtherType]),
  %% TODO: Convert below log trace to LOG_DEBUG
  ?LOG_ERROR("Unhandled Message is ~p~n", [lager:pr(_OtherMsg, ofproto_common)]),
  State.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create the OFP Message based on the Type & Args passed, Encode the body
%% and then send it across the connection passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec(send_message(integer(), ofp_parser(), pid(), atom(), term()) -> {ok, binary()} | {error, term()}).
send_message(Xid, Parser, Connection, OfpMsgType, OfpMsgArgs) ->
  case ofproto_parser:invoke(Parser, create_ofp_msg_body, {OfpMsgType, OfpMsgArgs}) of
    {ok, OfpBody} ->
      OfpMsg = #?OFP_MSG{header = #?OFP_HEADER{version = Parser#?OFP_PARSER.version, xid = Xid}, msg = OfpBody},
      case ofproto_parser:encode (Parser, OfpMsg) of
        {ok, EncOfMsg} ->
          flow_tcp_fsm:send_outgoing_data(Connection, EncOfMsg),
          {ok, EncOfMsg};
        {error, ErrReason} ->
          ?LOG_ERROR ("Error(~p) while encoding ~p~n", [ErrReason, OfpMsgType]),
          {error, ErrReason}
      end;
    {error, ErrReason} ->
      ?LOG_ERROR ("Error(~p) while forming OFP Msg Body of type: ~p~n", [ErrReason, OfpMsgType]),
      {error, ErrReason}
  end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Send the decoded data to the callback fn.
%%
%% @end
%%--------------------------------------------------------------------
invoke_callback(MsgType, Msg, State) ->
  MsgList = ofproto_parser:invoke(State#?SWITCH_STATE.parser, convert_rec_to_proplist, Msg),
  case invoke({State#?SWITCH_STATE.switch_id, MsgType, MsgList}, State, handle_incoming) of
    not_exported ->
      ?LOG_ERROR ("CallbackFn:: handle_incoming is not implemented in CB Module", []);
    _ ->
      ok
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%  Checks whether the callback module has exported the functions,
%%  if yes, invoke them
%%
%% @end
%%--------------------------------------------------------------------
invoke (Args, #?SWITCH_STATE{callback_module = HandlerMod} = _State, CallbackFn) ->
  case erlang:function_exported (HandlerMod, CallbackFn, 1) of
    true ->
      HandlerMod:CallbackFn(Args);
    false ->
      not_exported
  end.
