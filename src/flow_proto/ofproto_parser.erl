%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2017 17:28
%%%-------------------------------------------------------------------
-module(ofproto_parser).
-author("kirankurup@gmail.com").

-include ("logger.hrl").
-include("ofproto_common.hrl").

%% API
-export([create/1, decode/2, encode/2, invoke/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates parser record.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create(non_neg_integer()) -> {ok, ofp_parser()} | {error, Reason :: term()}).
create(Version) ->
  case ?PROTO_MOD(Version) of
    unsupported_version ->
      {error, unsupported_version};
    Module ->
      {ok, #?OFP_PARSER{version = Version, module = Module}}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Decode incoming packet.
%%
%% @end
%%--------------------------------------------------------------------
-spec(decode(Parser :: ofp_parser(), Pkt :: binary()) ->
  {ok, ofp_msg()} | {error, Reason :: term()}).
decode(Parser, Pkt) ->
  (Parser#?OFP_PARSER.module):decode(Pkt).

%%--------------------------------------------------------------------
%% @doc
%% Encode Msg for sending to SWITCH.
%%
%% @end
%%--------------------------------------------------------------------
-spec(encode(Parser :: ofp_parser(), Msg :: ofp_msg()) ->
  {ok, binary()} | {error, Reason :: term()}).
encode(Parser, Msg) ->
  (Parser#?OFP_PARSER.module):encode(Msg).

%%--------------------------------------------------------------------
%% @doc
%% Invoke the function of the respective Version with the Args.
%%
%% @end
%%--------------------------------------------------------------------
-spec(invoke(Parser :: ofp_parser(), FuncName :: atom(), FuncArgs :: term()) ->
  {ok, term()} | {error, Reason :: term()}).
invoke(Parser, FuncName, FuncArgs) ->
  (Parser#?OFP_PARSER.module):FuncName(FuncArgs).
