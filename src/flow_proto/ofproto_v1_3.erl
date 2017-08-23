%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2017 18:57
%%%-------------------------------------------------------------------
-module(ofproto_v1_3).
-author("kirankurup@gmail.com").

%% API
-export([create_error_msg/3]).
-export([create_ofp_msg_body/1]).
-export([get_msg_type/1, get_ofp_type/1]).
-export([decode/1, encode/1]).
-export([convert_rec_to_proplist/1]).


-include("logger.hrl").
-include("ofproto_common.hrl").
-include("ofproto_v1_3.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates OFP Error Message ready to be send to switch.
%%
%% @end
%%--------------------------------------------------------------------
-spec (create_error_msg(Type :: atom(), Code :: atom(), Data :: binary()) -> {ok, term()} | {error, term()}).
create_error_msg(hello_failure, incompatible_verions, Data) ->
  {ok, #?OFP_ERROR{type = ?OFPET_HELLO_FAILED, code = ?OFPHFC_INCOMPATIBLE, data = Data}};

create_error_msg(_Type, _Code, _Data) ->
  {error, unknown_error_type}.

%%--------------------------------------------------------------------
%% @doc
%% Creates OFP Message Body based on OFPT Type.
%%
%% @end
%%--------------------------------------------------------------------
-spec (create_ofp_msg_body({atom(), term()}) -> {ok, term()} | {error, term()}).
create_ofp_msg_body({?OFP_FEATURES_REQUEST, _Args}) ->
  {ok, #?OFP_FEATURES_REQUEST{}};

create_ofp_msg_body({?OFP_ECHO_REPLY, #?OFP_ECHO_REQUEST{data = Data}}) ->
  {ok, #?OFP_ECHO_REPLY{data = Data}};

create_ofp_msg_body({?OFP_GET_CONFIG_REQUEST, _Args}) ->
  {ok, #?OFP_GET_CONFIG_REQUEST{}};

create_ofp_msg_body({?OFP_SET_CONFIG, {Flags, MissSend}}) ->
  MissSendLen = case MissSend of
                  ofpcml_max_len ->
                    ?OFPCML_MAX_LEN;
                  ofpcml_no_buffer ->
                    ?OFPCML_NO_BUFFER;
                  Other when is_integer(Other) ->
                    Other;
                  _ ->
                    ?OFPCML_DEFAULT
                end,
  {ok, #?OFP_SET_CONFIG{flags = Flags, miss_send_len = MissSendLen}};

create_ofp_msg_body({?OFP_MULTIPART_REQUEST, {ofpmp_desc, Flags}}) ->
  {ok, #?OFP_MULTIPART_REQUEST{type = ofpmp_desc, flags = Flags}};

create_ofp_msg_body({?OFP_MULTIPART_REQUEST, {ofpmp_flow, Flags, FlowArgs}}) ->
  FlowBody = create_flow_body(FlowArgs),
  {ok, #?OFP_MULTIPART_REQUEST{type = ofpmp_flow, flags = Flags, body = FlowBody}};

create_ofp_msg_body(_Other) ->
  {error, bad_message_type}.

%%--------------------------------------------------------------------
%% @doc
%% Decode packet.
%%
%% @end
%%--------------------------------------------------------------------
-spec(decode(Data :: binary()) ->
  {ok, ofp_msg()} | {error, Reason :: term()}).
decode(Data) ->
  <<Version:?UINT_8, Type:?UINT_8, Len:?UINT_16, Xid:?UINT_32, Rest/binary>> = Data,
  Header = #?OFP_HEADER{version = Version, type = Type, len = Len, xid = Xid},
  try
    BodyLen = Len - 8,
    <<Body:BodyLen/binary, _/binary>> = Rest,
    DecBody = decode_body(Type, Body),
    ?LOG_INFO ("Decoded Body: ~p~n", [lager:pr(DecBody, ofproto_v1_3)]),
    {ok, #?OFP_MSG{header = Header, msg = DecBody}}
  catch
    _:Ex ->
      ?LOG_ERROR ("Error while decoding: ~p~n", [Ex]),
      {error, Ex}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Encode Data.
%%
%% @end
%%--------------------------------------------------------------------
-spec(encode(Data :: ofp_msg()) ->
  {ok, binary()} | {error, Reason :: term()}).
encode(#?OFP_MSG{header = #?OFP_HEADER{version = Version, xid = Xid}, msg = Msg} = _Data) ->
%  try
    Body = encode_body(Msg),
    Type = get_type_from_msg (Msg),
    Len = ?OFP_HEADER_SIZE + size(Body),
    {ok, <<Version:?UINT_8, Type:?UINT_8, Len:?UINT_16, Xid:?UINT_32, Body/binary>>}.
%  catch
%    _:Ex ->
%      {error, Ex}
%  end.

%%--------------------------------------------------------------------
%% @doc
%% Get OFP Type(atom) based on the record passed.
%%
%% @end
%%--------------------------------------------------------------------
get_msg_type({#?OFP_MSG{msg = #?OFP_HELLO{}}}) ->
  {ok, ?OFP_HELLO};

get_msg_type({#?OFP_MSG{msg = #?OFP_ERROR{}}}) ->
  {ok, ?OFP_ERROR};

get_msg_type({#?OFP_MSG{msg = #?OFP_FEATURES_REQUEST{}}}) ->
  {ok, ?OFP_FEATURES_REQUEST};

get_msg_type({#?OFP_MSG{msg = #?OFP_FEATURES_REPLY{}}}) ->
  {ok, ?OFP_FEATURES_REPLY};

get_msg_type({#?OFP_MSG{msg = #?OFP_ECHO_REQUEST{}}}) ->
  {ok, ?OFP_ECHO_REQUEST};

get_msg_type({#?OFP_MSG{msg = #?OFP_ECHO_REPLY{}}}) ->
  {ok, ?OFP_ECHO_REPLY};

get_msg_type({#?OFP_MSG{msg = #?OFP_GET_CONFIG_REQUEST{}}}) ->
  {ok, ?OFP_GET_CONFIG_REQUEST};

get_msg_type({#?OFP_MSG{msg = #?OFP_GET_CONFIG_REPLY{}}}) ->
  {ok, ?OFP_GET_CONFIG_REPLY};

get_msg_type(_Other) ->
  {error, bad_message}.

%%--------------------------------------------------------------------
%% @doc
%% Convert OFP Msg to a proplists for easier parsing.
%%
%% @end
%%--------------------------------------------------------------------
-spec(convert_rec_to_proplist(term()) -> list()).
convert_rec_to_proplist(OFPMsg) ->
  record_to_proplist(OFPMsg#?OFP_MSG.msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% All Decode Related
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decode capabilities.
%%
%% @end
%%--------------------------------------------------------------------
decode_capabilities(_Caps, _CapType, -1, Acc) ->
  Acc;

decode_capabilities(Caps, CapType, Length, Acc) when Caps band (1 bsl Length) =/= 0 ->
  decode_capabilities(Caps, CapType, Length - 1, get_capabilities_from_num({CapType, Length}, Acc));

decode_capabilities(Caps, CapType, Length, Acc) ->
  decode_capabilities(Caps, CapType, Length - 1, Acc).

decode_capabilities(Capabilities, CapType, Acc) ->
  CapLength = size(Capabilities) * 8,
  <<CapInt:CapLength>> = Capabilities,
  decode_capabilities(CapInt, CapType, CapLength - 1, Acc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decode Body based on the type passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec(decode_body(Type :: non_neg_integer(), Body :: binary()) -> {ok, term()} | {error, term()}).
decode_body(?OFPT_HELLO, _Body) ->
  #?OFP_HELLO{};

decode_body(?OFPT_ERROR, <<Type:16, Code:16, Data/bytes>> = _Body) ->
  #?OFP_ERROR{type = Type, code = Code, data = Data};

decode_body(?OFPT_FEATURES_REPLY, <<DatapathBin:8/bytes, Buffers:32,
  NTables:8, AuxId:8, _Pad:16, Capabilities:4/bytes, _Res:32>> = _Body) ->
  <<_Impl:16, MacAddress:6/bytes>> = DatapathBin,
  <<DatapathInt:64>> = DatapathBin,
  ?LOG_ERROR("Capabilities ~p~n", [Capabilities]),
  DecCaps = decode_capabilities(Capabilities, capabilities, ?DEFAULT_CAPS),
  #?OFP_FEATURES_REPLY{datapath_id = DatapathBin, datapath_str = integer_to_binary(DatapathInt), mac_address = MacAddress,
    n_buffers = Buffers, n_tables = NTables, auxiliary_id = AuxId, capabilities = DecCaps};

decode_body(?OFPT_ECHO_REQUEST, <<Data/bytes>> = _Body) ->
  #?OFP_ECHO_REQUEST{data = Data};

decode_body(?OFPT_GET_CONFIG_REPLY, <<ConfigFlagsInt:16, MissSendLen:16>> = _Body) ->
  DecConfigFlags = get_capabilities_from_num({config_flags, ConfigFlagsInt band ?OFPC_FRAG_MASK}, ?DEFAULT_CONFIG_FLAGS),
  #?OFP_GET_CONFIG_REPLY{flags = DecConfigFlags, miss_send_len = MissSendLen};

decode_body(Type, _Body) ->
  throw({bad_message, Type}).
%%%===================================================================
%%% All Encode Related
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode Body based on the record passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec(encode_body(term()) -> binary()).
encode_body(#?OFP_HELLO{}) ->
  <<>>;

encode_body(#?OFP_ERROR{type = Type, code = Code, data = Data}) ->
  <<Type:?UINT_16, Code:?UINT_16, Data/bytes>>;

encode_body(#?OFP_FEATURES_REQUEST{}) ->
  <<>>;

encode_body(#?OFP_ECHO_REQUEST{}) ->
  <<>>;

encode_body(#?OFP_ECHO_REPLY{data = Data}) ->
  Data;

encode_body(#?OFP_GET_CONFIG_REQUEST{}) ->
  <<>>;

encode_body(#?OFP_SET_CONFIG{flags = Flags, miss_send_len = MissSendLen}) ->
  <<Flags:?UINT_16, MissSendLen:?UINT_16>>;

encode_body(#?OFP_MULTIPART_REQUEST{type = OfpMpType, flags = Flags, pad = Pad, body = Body} = Msg) ->
  FlagBin = convert_flags_to_binary(Flags, <<0:16>>, 16),
  case construct_ofpmp_req(OfpMpType, Body) of
    unknown_multireq_type ->
      throw({unknown_multireq_type, Msg});
    {?OFPMP_DESC, _EncMPBody} ->
      <<?OFPMP_DESC:?UINT_16, FlagBin/bytes, Pad/bytes>>;
    {?OFPMP_FLOW, EncMPBody} ->
      <<?OFPMP_FLOW:?UINT_16, FlagBin/bytes, Pad/bytes, EncMPBody/bytes>>
  end;

encode_body(#?OFP_MATCH{oxm_fields = Fields}) ->
  OXMFieldEnc = encode_list(encode_body, Fields, <<>>),
  OXMFieldLength = size(OXMFieldEnc),
  Length = OXMFieldLength + 4, % additional 4 bytes for Type & Length
  PadLength = get_pad_bits_length (Length, 8),
  <<?OFPMT_OXM:?UINT_16,Length:?UINT_16, OXMFieldEnc/bytes, 0:PadLength>>;

encode_body(#?OXM_FIELD{}) ->
  <<>>;

encode_body(Other) ->
  throw({bad_message, Other}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get OFP Type based on the record passed.
%%
%% @end
%%--------------------------------------------------------------------
get_type_from_msg(#?OFP_HELLO{}) ->
  ?OFPT_HELLO;

get_type_from_msg(#?OFP_ERROR{}) ->
  ?OFPT_ERROR;

get_type_from_msg(#?OFP_FEATURES_REQUEST{}) ->
  ?OFPT_FEATURES_REQUEST;

get_type_from_msg(#?OFP_ECHO_REQUEST{}) ->
  ?OFPT_ECHO_REQUEST;

get_type_from_msg(#?OFP_ECHO_REPLY{}) ->
  ?OFPT_ECHO_REPLY;

get_type_from_msg(#?OFP_GET_CONFIG_REQUEST{}) ->
  ?OFPT_GET_CONFIG_REQUEST;

get_type_from_msg(#?OFP_GET_CONFIG_REPLY{}) ->
  ?OFPT_GET_CONFIG_REPLY;

get_type_from_msg(#?OFP_SET_CONFIG{}) ->
  ?OFPT_SET_CONFIG;

get_type_from_msg(#?OFP_MULTIPART_REQUEST{}) ->
  ?OFPT_MULTIPART_REQUEST;

get_type_from_msg(Other) ->
  throw({bad_message, Other}).

%%--------------------------------------------------------------------
%% @doc
%% Get OFP Type(integer) based on the atom passed.
%%
%% @end
%%--------------------------------------------------------------------
get_ofp_type(?OFP_HELLO) ->
  ?OFPT_HELLO;

get_ofp_type(?OFP_ERROR) ->
  ?OFPT_ERROR;

get_ofp_type(?OFP_FEATURES_REQUEST) ->
  ?OFPT_FEATURES_REQUEST;

get_ofp_type(?OFP_FEATURES_REPLY) ->
  ?OFPT_FEATURES_REPLY;

get_ofp_type(?OFP_ECHO_REQUEST) ->
  ?OFPT_ECHO_REQUEST;

get_ofp_type(?OFP_ECHO_REPLY) ->
  ?OFPT_ECHO_REPLY;

get_ofp_type(_Other) ->
  bad_message.

%%--------------------------------------------------------------------
%% @doc
%% Decode OFP Capabilities.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_capabilities_from_num(non_neg_integer(), list()) -> list()).
get_capabilities_from_num(OfpCaps, Acc) ->
  CapsAtom = convert_caps_to_atom(OfpCaps),
  lists:keyreplace(CapsAtom, 1, Acc, {CapsAtom, 1}).

%%%-------------------------------------------------------------------
%% Record to proplists
%%%-------------------------------------------------------------------
record_to_proplist(#?OFP_ERROR{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_ERROR), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_FEATURES_REQUEST{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_FEATURES_REQUEST), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_FEATURES_REPLY{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_FEATURES_REPLY), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_ECHO_REQUEST{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_ECHO_REQUEST), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_ECHO_REPLY{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_ECHO_REPLY), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_GET_CONFIG_REQUEST{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_GET_CONFIG_REQUEST), tl(tuple_to_list(Rec)));
record_to_proplist(#?OFP_GET_CONFIG_REPLY{} = Rec) ->
  lists:zip(record_info(fields, ?OFP_GET_CONFIG_REPLY), tl(tuple_to_list(Rec)));
record_to_proplist(_Rec) ->
  [].

%%%-------------------------------------------------------------------
%% Convert Capabilities to atom
%%%-------------------------------------------------------------------
convert_caps_to_atom({capabilities, ?OFPC_FLOW_STATS}) ->
  ofpc_flow_stats;
convert_caps_to_atom({capabilities, ?OFPC_TABLE_STATS}) ->
  ofpc_table_stats;
convert_caps_to_atom({capabilities, ?OFPC_PORT_STATS}) ->
  ofpc_port_stats;
convert_caps_to_atom({capabilities, ?OFPC_GROUP_STATS}) ->
  ofpc_group_stats;
convert_caps_to_atom({capabilities, ?OFPC_IP_REASM}) ->
  ofpc_ip_reasm;
convert_caps_to_atom({capabilities, ?OFPC_QUEUE_STATS}) ->
  ofpc_queue_stats;
convert_caps_to_atom({capabilities, ?OFPC_PORT_BLOCKED}) ->
  ofpc_port_blocked;
convert_caps_to_atom({config_flags, ?OFPC_FRAG_NORMAL}) ->
  ofpc_frag_normal;
convert_caps_to_atom({config_flags, ?OFPC_FRAG_DROP}) ->
  ofpc_frag_drop;
convert_caps_to_atom({config_flags, ?OFPC_FRAG_REASM}) ->
  ofpc_frag_reasm;
convert_caps_to_atom(_) ->
    undefined.

%%%-------------------------------------------------------------------
%% convert ofp multipart atom to appropriate int
%%%-------------------------------------------------------------------
construct_ofpmp_req(ofpmp_desc, _Body) ->
  {?OFPMP_DESC, <<>>};
construct_ofpmp_req(ofpmp_flow, #?OFP_FLOW_STATS_REQUEST{table_id = TableId, out_port = OutPort, out_group = OutGrp, cookie = Cookie,
  cookie_mask = CookieMask, match = Match} = _Body) ->
  MatchBin = encode_body(Match),
  {?OFPMP_FLOW, <<TableId:?UINT_8, 0:24, OutPort:?UINT_32, OutGrp:?UINT_32, 0:32, Cookie:8/bytes, CookieMask:8/bytes,
    MatchBin/bytes>>};
construct_ofpmp_req(ofpmp_aggregate, _Body) ->
  {?OFPMP_AGGREGATE, <<>>};
construct_ofpmp_req(ofpmp_table, _Body) ->
  {?OFPMP_TABLE, <<>>};
construct_ofpmp_req(ofpmp_port_stats, _Body) ->
  {?OFPMP_PORT_STATS, <<>>};
construct_ofpmp_req(ofpmp_queue, _Body) ->
  {?OFPMP_QUEUE, <<>>};
construct_ofpmp_req(ofpmp_group, _Body) ->
  {?OFPMP_GROUP, <<>>};
construct_ofpmp_req(ofpmp_group_desc, _Body) ->
  {?OFPMP_GROUP_DESC, <<>>};
construct_ofpmp_req(ofpmp_group_features, _Body) ->
  {?OFPMP_GROUP_FEATURES, <<>>};
construct_ofpmp_req(ofpmp_meter, _Body) ->
  {?OFPMP_METER, <<>>};
construct_ofpmp_req(ofpmp_meter_config, _Body) ->
  {?OFPMP_METER_CONFIG, <<>>};
construct_ofpmp_req(ofpmp_meter_features, _Body) ->
  {?OFPMP_METER_FEATURES, <<>>};
construct_ofpmp_req(ofpmp_table_features, _Body) ->
  {?OFPMP_TABLE_FEATURES, <<>>};
construct_ofpmp_req(ofpmp_port_desc, _Body) ->
  {?OFPMP_PORT_DESC, <<>>};
construct_ofpmp_req(ofpmp_experimenter, _Body) ->
  {?OFPMP_EXPERIMENTER, <<>>};
construct_ofpmp_req(_, _) ->
  unknown_multireq_type.


%%%-------------------------------------------------------------------
%% convert flags to binary
%%%-------------------------------------------------------------------
convert_flags_to_binary([], Acc, _SizeInBits) ->
  Acc;
convert_flags_to_binary([H|R], Acc, SizeInBits) ->
  <<Binary1:SizeInBits>> = Acc,
  NewAcc = case get_lsl_bits(H) of
             undefined ->
               Acc;
             LslBits ->
               (Binary1 bor (1 bsl LslBits))
           end,
  convert_flags_to_binary(R, NewAcc, SizeInBits).

%%%-------------------------------------------------------------------
%% Get left shift bits
%%%-------------------------------------------------------------------
get_lsl_bits(ofpmp_req_more) ->
  ?OFPMPF_REQ_MORE;
get_lsl_bits(_) ->
  undefined.

%%%-------------------------------------------------------------------
%% Create Flow body from the arguments passed
%%%-------------------------------------------------------------------
create_flow_body({TableId, OutPort, OutGroup, Cookie, CookieMask, Match}) ->
  NewTableId = get_table_id(TableId),
  NewOutPort = get_port(OutPort),
  NewOutGroup = get_group(OutGroup),
  NewMatch = get_match(Match),
  #?OFP_FLOW_STATS_REQUEST{table_id = NewTableId, out_port = NewOutPort, out_group = NewOutGroup, cookie = Cookie,
    cookie_mask = CookieMask, match = NewMatch}.

%%%-------------------------------------------------------------------
%% Get Table Id
%%%-------------------------------------------------------------------
get_table_id(TableId) ->
  case TableId of
    all ->
      ?OFPTT_ALL;
    Val when is_integer(Val) ->
      Val;
    _ ->
      ?OFPTT_ALL
  end.

%%%-------------------------------------------------------------------
%% Get Port
%%%-------------------------------------------------------------------
get_port(Port) ->
  case Port of
    in_port ->
      ?OFPP_IN_PORT;
    table ->
      ?OFPP_TABLE;
    normal ->
      ?OFPP_NORMAL;
    flood ->
      ?OFPP_FLOOD;
    all ->
      ?OFPP_ALL;
    controller ->
      ?OFPP_CONTROLLER;
    local ->
      ?OFPP_LOCAL;
    Val when is_integer(Val) ->
      Val;
    _ ->
      ?OFPP_ANY
  end.

%%%-------------------------------------------------------------------
%% Get Group
%%%-------------------------------------------------------------------
get_group(Group) ->
  case Group of
    all ->
      ?OFPG_ALL;
    Val when is_integer(Val) ->
      Val;
    _ ->
      ?OFPG_ANY
  end.

%%%-------------------------------------------------------------------
%% Get Match
%%%-------------------------------------------------------------------
get_match(Match) ->
  case Match of
    undefined ->
      #?OFP_MATCH{};
    _ ->
      #?OFP_MATCH{}
  end.

%%%-------------------------------------------------------------------
%% Encodes the list
%%%-------------------------------------------------------------------
encode_list(_EncodeFn, [], Acc) ->
  Acc;

encode_list(EncodeFn, [H|R], Acc) ->
  Result = erlang:apply(EncodeFn, [H]),
  encode_list(EncodeFn, R, <<Acc/bytes, Result/bytes>>).

%%%-------------------------------------------------------------------
%% Calculate how many pad bits are required to align.
%%%-------------------------------------------------------------------
get_pad_bits_length(Length, AlignLen) ->
  Reminder = Length rem AlignLen,
  Reminder * 8.