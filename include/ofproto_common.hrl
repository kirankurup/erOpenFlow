%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2017 15:58
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-ifndef (_OFP_PROTOCOL_COMMON_HRL).
-define (_OFP_PROTOCOL_COMMON_HRL, true).

-define (UINT_8,                                    8/big-unsigned-integer).
-define (UINT_16,                                   16/big-unsigned-integer).
-define (UINT_32,                                   32/big-unsigned-integer).

-define (OFP_VERSION_V_1_1,                         1).
-define (OFP_VERSION_V_1_2,                         3).
-define (OFP_VERSION_V_1_3,                         4).
-define (OFP_VERSION_V_1_4,                         5).
-define (OFP_VERSION_V_1_5,                         6).

-define (PROTO_MOD(Version),                        case Version of
                                                      ?OFP_VERSION_V_1_5 ->
                                                        ofproto_v1_5;
                                                      ?OFP_VERSION_V_1_4 ->
                                                        ofproto_v1_5;
                                                      ?OFP_VERSION_V_1_3 ->
                                                        ofproto_v1_3;
                                                      ?OFP_VERSION_V_1_2 ->
                                                        ofproto_v1_2;
                                                      ?OFP_VERSION_V_1_1 ->
                                                        ofproto_v1_1;
                                                      _ ->
                                                        unsupported_version
                                                    end).

%%%-------------------------------------------------------------------
%% TCP FSM State
%%%-------------------------------------------------------------------
-define (SWITCH_TCP_STATE,                          switch_tcp_state_rec).

-record (?SWITCH_TCP_STATE,
          { socket :: port(),
            ip_address :: string(),
            port :: non_neg_integer(),
            switch :: pid()}).

%%%-------------------------------------------------------------------
%% Switch State
%%%-------------------------------------------------------------------
-define (SWITCH_STATE,                              switch_state_rec).

-record (?SWITCH_STATE,
          { callback_module = undefined :: atom(),
            supported_versions :: list(),
            connection_listeners :: list(),
            main_connection :: pid(),
            xid = 0 :: non_neg_integer(),
            parser :: ofp_parser(),
            switch_id = undefined :: term(),
            switch_storage_type = ets :: atom()}).

%%%-------------------------------------------------------------------
%% OF Protocol Parser
%%%-------------------------------------------------------------------
-define (OFP_PARSER,                                ofp_parser).
-record (?OFP_PARSER,
          { module :: atom(),
            version :: non_neg_integer()}).
-type ofp_parser() :: #?OFP_PARSER{}.

%%%-------------------------------------------------------------------
%% OFP Header
%%%-------------------------------------------------------------------
-define (OFP_HEADER_SIZE,                           8).
-define (OFP_HEADER,                                ofp_header).
-record (?OFP_HEADER,
          { version :: non_neg_integer(),
            type :: non_neg_integer(),
            len :: non_neg_integer(),
            xid  = -1 :: integer()}).

-type ofp_header() :: #?OFP_HEADER{}.

%%%-------------------------------------------------------------------
%% OFP Message Generic Structure
%%%-------------------------------------------------------------------
-define (OFP_MSG,                                   ofp_message).
-record (?OFP_MSG,
          { header :: ofp_header(),
            msg :: term()}).

-type ofp_msg() :: #?OFP_MSG{}.

%%%-------------------------------------------------------------------
%% OFPT_HELLO
%%%-------------------------------------------------------------------

-define (OFPHET_VERSIONBITMAP,                      1).
-define (OFPHE_VERSIONBITMAP,                       ofp_hello_elem_versionbitmap).
-record (?OFPHE_VERSIONBITMAP,
          { type  = ?OFPHE_VERSIONBITMAP :: non_neg_integer(),
            len :: non_neg_integer(),
            bitmaps :: list()}).
-type ofp_hello_element() :: #?OFPHE_VERSIONBITMAP{}.


-define (OFP_HELLO,                                 ofp_hello).
-record (?OFP_HELLO,
          { elements  = [] :: [ofp_hello_element()]}).


-endif.
