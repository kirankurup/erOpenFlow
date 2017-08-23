%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2017 15:16
%%%-------------------------------------------------------------------
-module(flow_switch).
-author("kirankurup@gmail.com").

-include("flow_defaults.hrl").
-include("logger.hrl").

%% API
-export([register_switch/3,
  unregister_switch/3, send_to_switch/3]).


%%====================================================================
%% Interface APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Registers the switch
%%
%% @end
%%--------------------------------------------------------------------
-spec register_switch(SwitchId :: term(), ActorPid :: pid(),
    StorageType :: atom()) -> term().
register_switch(SwitchId, ActorPid, StorageType) ->
  case flow_storage:invoke_method(StorageType, lookup, ?FLOW_SWITCH_DB, SwitchId) of
    [] ->
      %% Don't do anything here. We will register anyways.
      ok;
    [{SwitchId, OldActorPid}] ->
      %% Check if PID still alive. Shouldn't be the case.
      %% Go ahead and stop the FSM.
      case OldActorPid =:= ActorPid of
        true ->
          %% LOG and come out. We shouldn't be here
          ?LOG_WARN("Trying to register again the same Pid: ~p for the same Switch ~p~n", [ActorPid, SwitchId]);
        false ->
          case is_process_alive(OldActorPid) of
            false ->
              ?LOG_WARN("Dead Pid: ~p found for the same Switch ~p~n", [OldActorPid, SwitchId]);
            true ->
              ?LOG_WARN("Alive Pid: ~p found for the same Switch ~p~n", [OldActorPid, SwitchId]),
              flow_switch_handler_fsm:stop_handler(OldActorPid)
          end
      end
  end,
  flow_storage:invoke_method(StorageType, insert, ?FLOW_SWITCH_DB, {SwitchId, ActorPid}).

%%--------------------------------------------------------------------
%% @doc
%% Un registers the switch
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_switch(SwitchId :: term(), ActorPid :: pid(),
    StorageType :: atom()) -> term().
unregister_switch(SwitchId, ActorPid, StorageType) ->
  case flow_storage:invoke_method(StorageType, lookup, ?FLOW_SWITCH_DB, SwitchId) of
    [] ->
      %% Don't do anything here. We will register anyways.
      ?LOG_ERROR("No actors found for switch_id ~p.. Nothing to unregister~n", [SwitchId]);
    [{SwitchId, ActorPid}] ->
      flow_storage:invoke_method(StorageType, delete, ?FLOW_SWITCH_DB, SwitchId);
    [{SwitchId, Pid}] ->
      ?LOG_WARN("Found another actor ~p for switch_id ~p.. Dont register~n", [Pid, SwitchId])
  end.

%%--------------------------------------------------------------------
%% @doc
%% Sends Msg to the FSM registered with the Switch Id.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_to_switch(SwitchId :: term(), StorageType :: atom(),
    Msg :: term()) -> term().
send_to_switch(SwitchId, StorageType, Msg) ->
  case flow_storage:invoke_method(StorageType, lookup, ?FLOW_SWITCH_DB, SwitchId) of
    [] ->
      %% Don't do anything here. We will register anyways.
      ?LOG_ERROR("No actors found for switch_id ~p~n", [SwitchId]),
      {error, <<"No switch">>};
    [{SwitchId, ActorPid}] ->
      %% Check if PID still alive.
      case is_process_alive(ActorPid) of
        false ->
          ?LOG_WARN("Dead Pid: ~p found for the Switch ~p~n", [ActorPid, SwitchId]),
          unregister_switch(SwitchId, ActorPid, StorageType);
        true ->
          flow_switch_handler_fsm:send_outgoing_msg(ActorPid, Msg)
      end
  end.

