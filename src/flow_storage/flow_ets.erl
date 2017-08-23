%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%  ETS specific code
%%%
%%% @end
%%% Created : 17. May 2017 14:50
%%%-------------------------------------------------------------------
-module(flow_ets).
-author("kirankurup@gmail.com").

%% API
-export([
  new/2, info/2,
  insert/2, lookup/2,
  delete/2, update_element/3,
  delete/1
]).

%%%===================================================================
%%% API Implementation
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a new ETS table with the given options.
%%
%% @end
%%--------------------------------------------------------------------
-spec new (atom(), list()) -> integer() | atom().
new (Name, Options) ->
  ets:new (Name, Options).

%%--------------------------------------------------------------------
%% @doc
%% Get information on table.
%%
%% @end
%%--------------------------------------------------------------------
-spec info (integer() | atom(), atom()) -> undefined | term().
info (Tab, Item) ->
  ets:info (Tab, Item).

%%--------------------------------------------------------------------
%% @doc
%% Inserts value on to table.
%%
%% @end
%%--------------------------------------------------------------------
-spec insert(Tab, ObjectOrObjects) -> true when
  Tab :: integer() | atom(),
  ObjectOrObjects :: tuple() | [tuple()].
insert (Table, Value) ->
  ets:insert (Table, Value).

%%--------------------------------------------------------------------
%% @doc
%% Lookup for Key on table.
%%
%% @end
%%--------------------------------------------------------------------
-spec lookup(Tab, Key) -> [Object] when
  Tab :: integer() | atom(),
  Key :: term(),
  Object :: tuple().
lookup (Table, Key) ->
  ets:lookup (Table, Key).

%%--------------------------------------------------------------------
%% @doc
%% Delete Key & Associated value from Table.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete (Tab, term()) -> true when
  Tab :: integer() | atom().
delete (TableName, Key) ->
  ets:delete(TableName, Key).

%%--------------------------------------------------------------------
%% @doc
%% Delete the entire Table.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete (Tab) -> true when
  Tab :: integer() | atom().
delete (TableName) ->
  ets:delete(TableName).

%%--------------------------------------------------------------------
%% @doc
%% Updates the value associated with the Key.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_element (Tab, term(), ElementSpec :: {Pos, Value}) -> true when
  Tab :: integer() | atom(),
  Pos :: pos_integer(),
  Value :: term().
update_element (Table, Key, ElementSpec) ->
  ets:update_element (Table, Key, ElementSpec).
