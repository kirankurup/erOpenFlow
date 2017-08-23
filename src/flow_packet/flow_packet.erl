%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2017 13:30
%%%-------------------------------------------------------------------
-module(flow_packet).
-author("kirankurup@gmail.com").

-include("logger.hrl").
-include("ofproto_common.hrl").

%% API
-export([decode/1, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Decodes the incoming OF Messages from Switch.
%%
%% @end
%%--------------------------------------------------------------------
-spec(decode(binary()) -> {ok, Msg :: ofp_msg()} | {error, term()} | {error, term(), non_neg_integer()}).
decode (<<Version:?UINT_8, _Type:?UINT_8, _Len:?UINT_16, Xid:?UINT_32, _/binary>> = Data) ->
  case ?PROTO_MOD(Version) of
    unsupported_version ->
      {error, unsupported_version, Xid};
    Module ->
      Module:decode (Data)
  end;

decode(_Data) ->
  {error, unknown_packet}.

%%--------------------------------------------------------------------
%% @doc
%% Encodes the message to be ready to send to Switch.
%%
%% @end
%%--------------------------------------------------------------------
encode (#?OFP_MSG{header = Header, msg = Msg}) ->
  Data = encode_msg (Msg),
  Len = size(Data) + 8,
  #?OFP_HEADER{version = Version, type = Type, xid = Xid} = Header,
  EncMsg = <<Version:8, Type:8, Len:16, Xid:32, Data/binary>>,
  EncMsg.

encode_msg (<<>>) ->
  <<>>.


