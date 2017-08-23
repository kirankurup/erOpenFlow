%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%  Global ETS table interface functions.
%%%
%%% @end
%%% Created : 17. May 2017 14:51
%%%-------------------------------------------------------------------
-module(flow_ets_init).
-author("kirankurup@gmail.com").

-include("flow_defaults.hrl").
-include("logger.hrl").

%% API
-export([
  init_tables/0
]).

%%====================================================================
%% Interface APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates all ETS tables.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_tables() -> ok.
init_tables () ->
  init_table_with_type (?FLOW_SWITCH_DB, undefined, set),
  ok.

%%====================================================================
%% Internal APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates the table of the given specifications if table not already
%% present.
%%
%% @end
%%--------------------------------------------------------------------
-spec (init_table_with_type(atom(), undefined | non_neg_integer(), atom()) ->
  integer() | atom() | ok).
init_table_with_type (TableName, KeyPos, Type) ->
  case flow_ets:info (TableName,exists) of
    undefined ->
      Options = [named_table, public, Type,
        {write_concurrency, true}, {read_concurrency, true}],
      NewOptions = case KeyPos of
                     undefined ->
                       Options;
                     _ ->
                       Options ++ [{keypos, KeyPos}]
                   end,
      ?LOG_INFO ("Creating ~p Table.. ~n",[TableName]),
      flow_ets:new (TableName, NewOptions);
    _ ->
      ?LOG_INFO ("Table name (~p) already present~n", [TableName]),
      ok
  end.


