%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2017 18:49
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-ifndef (_OFP_V1_3_HRL).
-define (_OFP_V1_3_HRL, true).

%%%-------------------------------------------------------------------
%% OFP Message Types
%%%-------------------------------------------------------------------
%% Immutable messages.
-define (OFPT_HELLO,                                0). % Symmetric message
-define (OFPT_ERROR,                                1). % Symmetric message
-define (OFPT_ECHO_REQUEST,                         2). % Symmetric message
-define (OFPT_ECHO_REPLY,                           3). % Symmetric message
-define (OFPT_EXPERIMENTER,                         4). % Symmetric message
%% Switch configuration messages.
-define (OFPT_FEATURES_REQUEST,                     5). % Controller/switch message
-define (OFPT_FEATURES_REPLY,                       6). % Controller/switch message
-define (OFPT_GET_CONFIG_REQUEST,                   7). % Controller/switch message
-define (OFPT_GET_CONFIG_REPLY,                     8). % Controller/switch message
-define (OFPT_SET_CONFIG,                           9). % Controller/switch message
%% Asynchronous messages.
-define (OFPT_PACKET_IN,                            10). % Async message
-define (OFPT_FLOW_REMOVED,                         11). % Async message
-define (OFPT_PORT_STATUS,                          12). % Async message
%%Controller command messages.
-define (OFPT_PACKET_OUT,                           13). % Controller/switch message
-define (OFPT_FLOW_MOD,                             14). % Controller/switch message
-define (OFPT_GROUP_MOD,                            15). % Controller/switch message
-define (OFPT_PORT_MOD,                             16). % Controller/switch message
-define (OFPT_TABLE_MOD,                            17). % Controller/switch message
%% Multipart messages.
-define (OFPT_MULTIPART_REQUEST,                    18). % Controller/switch message
-define (OFPT_MULTIPART_REPLY,                      19). % Controller/switch message
%% Barrier messages.
-define (OFPT_BARRIER_REQUEST,                      20). % Controller/switch message
-define (OFPT_BARRIER_REPLY,                        21). % Controller/switch message
%% Queue Configuration messages.
-define (OFPT_QUEUE_GET_CONFIG_REQUEST,             22). % Controller/switch message
-define (OFPT_QUEUE_GET_CONFIG_REPLY,               23). % Controller/switch message
%% Controller role change request messages.
-define (OFPT_ROLE_REQUEST,                         24). % Controller/switch message
-define (OFPT_ROLE_REPLY,                           25). % Controller/switch message
%% Asynchronous message configuration.
-define (OFPT_GET_ASYNC_REQUEST,                    26). % Controller/switch message
-define (OFPT_GET_ASYNC_REPLY,                      27). % Controller/switch message
-define (OFPT_SET_ASYNC,                            28). % Controller/switch message
%% Meters and rate limiters configuration messages.
-define (OFPT_METER_MOD,                            29). % Controller/switch message

%%%-------------------------------------------------------------------
%% OFP Capabilities
%%%-------------------------------------------------------------------
%% Capabilities supported by the datapath.
-define (OFPC_FLOW_STATS,                           0). % Flow statistics.
-define (OFPC_TABLE_STATS,                          1). % Table statistics.
-define (OFPC_PORT_STATS,                           2). % Port statistics.
-define (OFPC_GROUP_STATS,                          3). % Group statistics.
-define (OFPC_IP_REASM,                             5). % Can reassemble IP fragments.
-define (OFPC_QUEUE_STATS,                          6). % Queue statistics.
-define (OFPC_PORT_BLOCKED,                         8). % Switch will block looping ports.

%% Handling of IP fragments.
-define (OFPC_FRAG_NORMAL,                          0). % No special handling for fragments
-define (OFPC_FRAG_DROP,                            1). % Drop fragments
-define (OFPC_FRAG_REASM,                           2). % Reassemble (only if OFPC_IP_REASM set)
-define (OFPC_FRAG_MASK,                            3). % Bitmask of flags dealing with frag

%%%-------------------------------------------------------------------
%% OFP Error Types
%%%-------------------------------------------------------------------
-define (OFPET_HELLO_FAILED,                        0). % Hello protocol failed.
-define (OFPET_BAD_REQUEST,                         1). % Request was not understood.
-define (OFPET_BAD_ACTION,                          2). % Error in action description.
-define (OFPET_BAD_INSTRUCTION,                     3). % Error in instruction list.
-define (OFPET_BAD_MATCH,                           4). % Error in match.
-define (OFPET_FLOW_MOD_FAILED,                     5). % Problem modifying flow entry.
-define (OFPET_GROUP_MOD_FAILED,                    6). % Problem modifying group entry.
-define (OFPET_PORT_MOD_FAILED,                     7). % Port mod request failed.
-define (OFPET_TABLE_MOD_FAILED,                    8). % Table mod request failed.
-define (OFPET_QUEUE_OP_FAILED,                     9). % Queue operation failed.
-define (OFPET_SWITCH_CONFIG_FAILED,                10). % Switch config request failed.
-define (OFPET_ROLE_REQUEST_FAILED,                 11). % Controller Role request failed.
-define (OFPET_METER_MOD_FAILED,                    12). % Error in meter.
-define (OFPET_TABLE_FEATURES_FAILED,               13). % Setting table features failed.
-define (OFPET_EXPERIMENTER,                        16#ffff). % Experimenter error messages.

%%%-------------------------------------------------------------------
%% OFP HELLO_FAILED Error Codes
%%%-------------------------------------------------------------------
-define (OFPHFC_INCOMPATIBLE,                       0). % No compatible version.
-define (OFPHFC_EPERM,                              1). % Permissions error.

%%%-------------------------------------------------------------------
%% OFP BAD_REQUEST Error Codes
%%%-------------------------------------------------------------------
-define (OFPBRC_BAD_VERSION,                        0). % ofp_header.version not supported.
-define (OFPBRC_BAD_TYPE,                           1). % ofp_header.type not supported.
-define (OFPBRC_BAD_MULTIPART,                      2). % ofp_multipart_request.type not supported.
-define (OFPBRC_BAD_EXPERIMENTER,                   3). % Experimenter id not supported * (in ofp_experimenter_header or * ofp_multipart_request or * ofp_multipart_reply).
-define (OFPBRC_BAD_EXP_TYPE,                       4). % Experimenter type not supported.
-define (OFPBRC_EPERM,                              5). %Permissions error.
-define (OFPBRC_BAD_LEN,                            6). %Wrong request length for type.
-define (OFPBRC_BUFFER_EMPTY,                       7). %Specified buffer has already been used.
-define (OFPBRC_BUFFER_UNKNOWN,                     8). %Specified buffer does not exist.
-define (OFPBRC_BAD_TABLE_ID,                       9). %Specified table-id invalid or does not * exist.
-define (OFPBRC_IS_SLAVE,                           10). %Denied because controller is slave.
-define (OFPBRC_BAD_PORT,                           11). %Invalid port.
-define (OFPBRC_BAD_PACKET,                         12). %Invalid packet in packet-out.
-define (OFPBRC_MULTIPART_BUFFER_OVERFLOW,          13). %ofp_multipart_request overflowed the assigned buffer.


%%%-------------------------------------------------------------------
%% OFP Controller Max Len
%%%-------------------------------------------------------------------
-define (OFPCML_DEFAULT,                            128). % Default max_len value.
-define (OFPCML_MAX_LEN,                            16#ffe5). % maximum max_len value which can be used.
-define (OFPCML_NO_BUFFER,                          16#ffff). % indicates that no buffering should be applied and whole packet is to be sent to controller.

%%%-------------------------------------------------------------------
%% OFP Table
%%%-------------------------------------------------------------------
-define (OFPTT_MAX,                                 16#fe). % Last usable table number
-define (OFPTT_ALL,                                 16#ff). % Fake tables.

%%%-------------------------------------------------------------------
%% OFP Buffer
%%%-------------------------------------------------------------------
-define (OFP_NO_BUFFER,                             16#ffffffff).

%%%-------------------------------------------------------------------
%% OFP Flow Mod Command
%%%-------------------------------------------------------------------
-define (OFPFC_ADD,                                 0). % New flow.
-define (OFPFC_MODIFY,                              1). % Modify all matching flows.
-define (OFPFC_MODIFY_STRICT,                       2). % Modify entry strictly matching wildcards and priority.
-define (OFPFC_DELETE,                              3). % Delete all matching flows.
-define (OFPFC_DELETE_STRICT,                       4). % Delete entry strictly matching wildcards and priority.

%%%---------------------------------------------------------------------------------------
%% Port Related
%%%---------------------------------------------------------------------------------------

%% OFP Port Number
-define (OFPP_MAX,                                  16#ffffff00). % Maximum number of physical and logical switch ports.
-define (OFPP_IN_PORT,                              16#fffffff8). % Send the packet out the input port.
-define (OFPP_TABLE,                                16#fffffff9). % Submit the packet to the first flow table.
-define (OFPP_NORMAL,                               16#fffffffa). % Forward using non-OpenFlow pipeline.
-define (OFPP_FLOOD,                                16#fffffffb). % Flood using non-OpenFlow pipeline.
-define (OFPP_ALL,                                  16#fffffffc). % All standard ports except input port.
-define (OFPP_CONTROLLER,                           16#fffffffd). % Send to controller.
-define (OFPP_LOCAL,                                16#fffffffe). % Local openflow "port".
-define (OFPP_ANY,                                  16#ffffffff). % Special value used in some requests when no port is specified.


%%%---------------------------------------------------------------------------------------
%% Group Related
%%%---------------------------------------------------------------------------------------

% Group Id values
-define (OFPG_MAX,                                  16#ffffff00). % Last usable group number.
-define (OFPG_ALL,                                  16#fffffffc). % Represents all groups for group delete commands.
-define (OFPG_ANY,                                  16#ffffffff). % Special wildcard: no group specified.

%%%---------------------------------------------------------------------------------------
%% Structure definitions
%%%---------------------------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Flow Match Structures
%%%-------------------------------------------------------------------

% ofp_match_type
-define (OFPMT_STANDARD,                            0). % Deprecated.
-define (OFPMT_OXM,                                 1). % OpenFlow Extensible Match

-define (OXM_FIELD,                                 oxm_field).
-record (?OXM_FIELD,
{}).
-type ofp_field() :: #?OXM_FIELD{}.

%ofp_match structure
-define (OFP_MATCH,                                 ofp_match).
-record (?OFP_MATCH,
        { type = ofpmt_oxm :: non_neg_integer(),
          length :: non_neg_integer(),
          oxm_fields = []  :: [ofp_field()]}).
-type ofp_match() :: #?OFP_MATCH{}.

%%%-------------------------------------------------------------------
%% OFPT_ERROR
%%%-------------------------------------------------------------------

-define (OFP_ERROR,                                 ofp_error).
-record (?OFP_ERROR,
          { type :: non_neg_integer(),
            code :: non_neg_integer(),
            data = <<>> :: binary()}).

-type ofp_error() :: #?OFP_ERROR{}.

%%%-------------------------------------------------------------------
%% OFPT_FEATURES_REQUEST
%%%-------------------------------------------------------------------
-define (OFP_FEATURES_REQUEST,                      ofp_features_request).
-record (?OFP_FEATURES_REQUEST,                     {}).

%%%-------------------------------------------------------------------
%% OFPT_FEATURES_REPLY
%%%-------------------------------------------------------------------
-define (DEFAULT_CAPS,
          [ {ofpc_flow_stats, 0},
            {ofpc_table_stats, 0},
            {ofpc_port_stats, 0},
            {ofpc_group_stats, 0},
            {ofpc_ip_reasm, 0},
            {ofpc_queue_stats, 0},
            {ofpc_port_blocked, 0}]).

-define (OFP_FEATURES_REPLY,                        ofp_features_reply).
-record (?OFP_FEATURES_REPLY,
          { datapath_id :: binary(),
            datapath_str :: binary(),
            mac_address :: binary(),
            n_buffers :: non_neg_integer(),
            n_tables :: non_neg_integer(),
            auxiliary_id :: non_neg_integer(),
            capabilities = ?DEFAULT_CAPS :: list()}).

-type ofp_features_reply() :: #?OFP_FEATURES_REPLY{}.

%%%-------------------------------------------------------------------
%% OFPT_ECHO_REQUEST
%%%-------------------------------------------------------------------
-define (OFP_ECHO_REQUEST,                          ofp_echo_request).
-record (?OFP_ECHO_REQUEST,
        { data = <<>> :: binary()}).

%%%-------------------------------------------------------------------
%% OFPT_ECHO_REPLY
%%%-------------------------------------------------------------------
-define (OFP_ECHO_REPLY,                            ofp_echo_reply).
-record (?OFP_ECHO_REPLY,
        { data = <<>> :: binary()}).

%%%-------------------------------------------------------------------
%% OFPT_GET_CONFIG_REQUEST
%%%-------------------------------------------------------------------
-define (OFP_GET_CONFIG_REQUEST,                    ofp_get_config_request).
-record (?OFP_GET_CONFIG_REQUEST,                   {}).

%%%-------------------------------------------------------------------
%% OFPT_GET_CONFIG_REPLY
%%%-------------------------------------------------------------------
-define (DEFAULT_CONFIG_FLAGS,
          [ {ofpc_frag_normal, 0},
            {ofpc_frag_drop, 0},
            {ofpc_frag_reasm, 0}]).

-define (OFP_GET_CONFIG_REPLY,                      ofp_get_config_reply).
-record (?OFP_GET_CONFIG_REPLY,
        { flags = ?DEFAULT_CONFIG_FLAGS :: list(),
          miss_send_len :: non_neg_integer()}).

%%%-------------------------------------------------------------------
%% OFPT_SET_CONFIG
%%%-------------------------------------------------------------------
-define (OFP_SET_CONFIG,                            ofp_set_config).
-record (?OFP_SET_CONFIG,
          { flags = [] :: list(),
            miss_send_len :: non_neg_integer()}).

%%%-------------------------------------------------------------------
%% OFPT_FLOW_MOD
%%%-------------------------------------------------------------------
-type ofp_flow_mod_command() :: ofpfc_add
                                | ofpfc_modify
                                | ofpfc_modify_strict
                                | ofpfc_delete
                                | ofpfc_delete_strict.


-define (OFP_FLOW_MOD,                              ofp_flow_mod).
-record (?OFP_FLOW_MOD,
          { cookie = <<0:64>> :: binary(),
            cookie_mask = <<0:64>> :: binary(),
            table_id = 0 :: binary(),
            command :: ofp_flow_mod_command(),
            idle_timeout = 0 :: non_neg_integer(),
            hard_timeout = 0 :: non_neg_integer(),
            priority = 16#8000 :: non_neg_integer(),
            buffer_id = ?OFP_NO_BUFFER :: non_neg_integer(),
            out_port :: integer(),
            out_group :: integer(),
            flags,
            match,
            instructions
            }).


%%%-------------------------------------------------------------------
%% OFPT_MULTIPART_REQUEST
%%%-------------------------------------------------------------------
% ofp_multipart_type
-define (OFPMP_DESC,                                0). % Description of openflow switch.
-define (OFPMP_FLOW,                                1). % Individual flow statistics.
-define (OFPMP_AGGREGATE,                           2). % Aggregate flow statistics.
-define (OFPMP_TABLE,                               3). % Flow table statistics.
-define (OFPMP_PORT_STATS,                          4). % Port statistics.
-define (OFPMP_QUEUE,                               5). % Queue statistics for a port
-define (OFPMP_GROUP,                               6). % Group counter statistics
-define (OFPMP_GROUP_DESC,                          7). %  Group description.
-define (OFPMP_GROUP_FEATURES,                      8). % Group features
-define (OFPMP_METER,                               9). % Meter statistics.
-define (OFPMP_METER_CONFIG,                        10). % Meter configuration.
-define (OFPMP_METER_FEATURES,                      11). % Meter features.
-define (OFPMP_TABLE_FEATURES,                      12). % Table features.
-define (OFPMP_PORT_DESC,                           13). % Port description.
-define (OFPMP_EXPERIMENTER,                        16#ffff). % Experimenter extension.

-type ofp_multipart_type () ::  ofpmp_desc
                                | ofpmp_flow
                                | ofpmp_aggregate
                                | ofpmp_table
                                | ofpmp_port_stats
                                | ofpmp_queue
                                | ofpmp_group
                                | ofpmp_group_desc
                                | ofpmp_group_features
                                | ofpmp_meter
                                | ofpmp_meter_config
                                | ofpmp_meter_features
                                | ofpmp_table_features
                                | ofpmp_port_desc
                                | ofpmp_experimenter.

% ofp_multipart_request_flags
-define (OFPMPF_REQ_MORE,                           0).
-type ofpmp_request_flags() ::  []
                                | [ofpmpf_req_more].

% ofpmp_desc
-define (OFP_DESC_REQUEST,                          ofp_desc).
-record (?OFP_DESC_REQUEST,                         {}).
-type ofp_desc() :: #?OFP_DESC_REQUEST{}.

%ofpmp_flow
-define (OFP_FLOW_STATS_REQUEST,                    ofp_flow_stats_request).
% padding is not added to the record.
-record (?OFP_FLOW_STATS_REQUEST,
        { table_id = ?OFPTT_ALL :: integer(),
          out_port = ?OFPP_ANY :: non_neg_integer(),
          out_group = ?OFPG_ANY :: non_neg_integer(),
          cookie = <<0:64>> :: binary(),
          cookie_mask = <<0:64>> :: binary(),
          match = #?OFP_MATCH{} :: ofp_match()
          }).
-type ofp_flow_stats_request() :: #?OFP_FLOW_STATS_REQUEST{}.


-type ofp_multipart_body() ::  ofp_desc() |  ofp_flow_stats_request().


-define (OFP_MULTIPART_REQUEST,                     ofp_multipart_request).
-record (?OFP_MULTIPART_REQUEST,
        { type :: ofp_multipart_type(),
          flags = [] :: ofpmp_request_flags(),
          pad = <<0:32>> :: binary(),
          body = #?OFP_DESC_REQUEST{} :: ofp_multipart_body()}).

% ofp_multipart_reply_flags
-define (OFPMPF_REPLY_MORE,                         0).

-endif.

