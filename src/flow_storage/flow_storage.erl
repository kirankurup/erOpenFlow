%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2017 14:57
%%%-------------------------------------------------------------------
-module(flow_storage).
-author("kirankurup@gmail.com").

-include("logger.hrl").

%% API
-export([init_storage/1,
  invoke_method/4]).

%%====================================================================
%% Interface APIs
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initializes all storage platforms.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_storage(atom()) -> ok.
init_storage (StorageType) ->
  case get_storage_module(StorageType) of
    {ok, Module} ->
      InitModule = list_to_atom(atom_to_list(Module) ++ "_init"),
      InitModule:init_tables();
    {error, Reason} ->
      ?LOG_ERROR("Error ~p while tryying to initialize storage~n", [Reason])
   end.

%%--------------------------------------------------------------------
%% @doc
%% Initializes all storage platforms.
%%
%% @end
%%--------------------------------------------------------------------
-spec (invoke_method(atom(), atom(), term(), term()) -> term()).
invoke_method (StorageType, MethodName, TableName, Args) ->
  case get_storage_module(StorageType) of
    {ok, Module} ->
      Module:MethodName(TableName, Args);
    {error, Reason} ->
      ?LOG_ERROR("Error ~p while trying to invoke method ~p with args: ~p in table: ~p~n",
        [Reason, MethodName, Args, TableName])
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves module dealing with the storage_type passed
%%
%% @end
%%--------------------------------------------------------------------
-spec (get_storage_module(atom()) -> {ok, atom()} | {error, unknown}).
get_storage_module (StorageType) ->
  case StorageType of
    ets ->
      {ok, flow_ets};
    _ ->
      {error, unknown}
  end.
