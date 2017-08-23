%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2017 17:46
%%%-------------------------------------------------------------------
-module(ofproto_v1_5).
-author("kirankurup@gmail.com").

-include("logger.hrl").
-include("ofproto_common.hrl").
-include("ofproto_v1_5.hrl").

%% API
-export([create_error_msg/3]).
-export([get_msg_type/1]).
-export([decode/1, encode/1]).

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
    ?LOG_DEBUG ("Decoded Body: ~p~n", [lager:pr(DecBody, ofproto_v1_5)]),
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
  try
    Body = encode_body(Msg),
    Type = get_type_from_msg (Msg),
    Len = ?OFP_HEADER_SIZE + size(Body),
    {ok, <<Version:?UINT_8, Type:?UINT_8, Len:?UINT_16, Xid:?UINT_32, Body/binary>>}
  catch
    _:Ex ->
      ?LOG_ERROR ("Error while encoding: ~p~n", [Ex]),
      {error, Ex}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Get OFP Type(atom) based on the record passed.
%%
%% @end
%%--------------------------------------------------------------------
get_msg_type(#?OFP_MSG{msg = #?OFP_HELLO{}}) ->
  ?OFP_HELLO;

get_msg_type(#?OFP_MSG{msg = #?OFP_ERROR{}}) ->
  ?OFP_ERROR;

get_msg_type(_Other) ->
  bad_message.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% All Decode Related
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decode Bitmap.
%%
%% @end
%%--------------------------------------------------------------------
decode_bitmap(_, Index, Acc) when Index >= 32 ->
  Acc;

decode_bitmap(Bitmap, Index, Acc) when Bitmap band (1 bsl Index) == (1 bsl Index) ->
  decode_bitmap(Bitmap, Index + 1, Acc ++ [Index]);

decode_bitmap(Bitmap, Index, Acc) ->
  decode_bitmap(Bitmap, Index + 1, Acc).

decode_bitmap(<<BitMap:32, _Rest/binary>>) ->
  decode_bitmap(BitMap, 0, []).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decode hello elements. Right now only OFPHET_VERSIONBITMAP is supported.
%%
%% @end
%%--------------------------------------------------------------------
decode_hello_elements(<<>>, Acc) ->
  Acc;

decode_hello_elements(Elements, Acc) ->
  <<?OFPHET_VERSIONBITMAP:16, Len:16, Rest/bytes>> = Elements,
  BitmapLen = Len - 4,
  <<BitMapEnc:BitmapLen/bytes, Rest1/bytes>> = Rest,
  NewAcc = Acc ++ #?OFPHE_VERSIONBITMAP{type = ?OFPHET_VERSIONBITMAP, len = Len, bitmaps = decode_bitmap(BitMapEnc)},
  decode_hello_elements(Rest1, NewAcc).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Decode Body based on the type passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec(decode_body(Type :: non_neg_integer(), Body :: binary()) -> {ok, term()} | {error, term()}).
decode_body(?OFPT_HELLO, Body) ->
  #?OFP_HELLO{elements = decode_hello_elements(Body, [])};

decode_body(?OFPT_ERROR, <<Type:16, Code:16, Data/bytes>> = _Body) ->
  #?OFP_ERROR{type = Type, code = Code, data = Data};

decode_body(Type, _Body) ->
  throw({bad_message, Type}).

%%%===================================================================
%%% All Encode Related
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode to Bitmap.
%%
%% @end
%%--------------------------------------------------------------------
encode_to_bitmap([], Acc) ->
  <<Acc:32>>;

encode_to_bitmap([H|R], Acc) ->
  encode_to_bitmap(R, Acc bor (1 bsl H)).

encode_to_bitmap(Versions) ->
  encode_to_bitmap(Versions, 0).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode Hello Element of type OFPHET_VERSIONBITMAP.
%%
%% @end
%%--------------------------------------------------------------------
encode_he(#?OFPHE_VERSIONBITMAP{type = ?OFPHET_VERSIONBITMAP, bitmaps = Versions}) ->
  BitmapEnc = encode_to_bitmap(Versions),
  Len = 4 + size(BitmapEnc),
  <<?OFPHET_VERSIONBITMAP:?UINT_16, Len:?UINT_16, BitmapEnc/bytes>>;

encode_he(_) ->
  <<>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode hello elements. Right now only OFPHET_VERSIONBITMAP is supported.
%%
%% @end
%%--------------------------------------------------------------------
encode_hello_elements([], Acc) ->
  list_to_binary(Acc);

encode_hello_elements([H|R], Acc) ->
  encode_hello_elements(R, [encode_he(H)|Acc]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Encode Body based on the record passed.
%%
%% @end
%%--------------------------------------------------------------------
-spec(encode_body(term()) -> binary()).
encode_body(#?OFP_HELLO{elements = Elements}) ->
  encode_hello_elements(Elements, []);

encode_body(#?OFP_ERROR{type = Type, code = Code, data = Data}) ->
  <<Type:?UINT_16, Code:?UINT_16, Data/bytes>>;

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

get_type_from_msg(Other) ->
  throw({bad_message, Other}).
