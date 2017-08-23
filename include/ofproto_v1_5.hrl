%%%-------------------------------------------------------------------
%%% @author Kiran N
%%% @copyright (C) 2017
%%% @doc
%%%
%%% @end
%%% Created : 31. Mar 2017 17:45
%%%-------------------------------------------------------------------
-author("kirankurup@gmail.com").

-ifndef (_OFP_V1_5_HRL).
-define (_OFP_V1_5_HRL, true).

%%%-------------------------------------------------------------------
%% OFP Message Types
%%%-------------------------------------------------------------------
%% Immutable messages.
-define (OFPT_HELLO,                        0). % Symmetric message
-define (OFPT_ERROR,                        1). % Symmetric message
-define (OFPT_ECHO_REQUEST,                 2). % Symmetric message
-define (OFPT_ECHO_REPLY,                   3). % Symmetric message
-define (OFPT_EXPERIMENTER,                 4). % Symmetric message
%% Switch configuration messages.
-define (OFPT_FEATURES_REQUEST,             5). % Controller/switch message
-define (OFPT_FEATURES_REPLY,               6). % Controller/switch message
-define (OFPT_GET_CONFIG_REQUEST,           7). % Controller/switch message
-define (OFPT_GET_CONFIG_REPLY,             8). % Controller/switch message
-define (OFPT_SET_CONFIG,                   9). % Controller/switch message
%% Asynchronous messages.
-define (OFPT_PACKET_IN,                    10). % Async message
-define (OFPT_FLOW_REMOVED,                 11). % Async message
-define (OFPT_PORT_STATUS,                  12). % Async message
%%Controller command messages.
-define (OFPT_PACKET_OUT,                   13). % Controller/switch message
-define (OFPT_FLOW_MOD,                     14). % Controller/switch message
-define (OFPT_GROUP_MOD,                    15). % Controller/switch message
-define (OFPT_PORT_MOD,                     16). % Controller/switch message
-define (OFPT_TABLE_MOD,                    17). % Controller/switch message
%% Multipart messages.
-define (OFPT_MULTIPART_REQUEST,            18). % Controller/switch message
-define (OFPT_MULTIPART_REPLY,              19). % Controller/switch message
%% Barrier messages.
-define (OFPT_BARRIER_REQUEST,              20). % Controller/switch message
-define (OFPT_BARRIER_REPLY,                21). % Controller/switch message
%% Controller role change request messages.
-define (OFPT_ROLE_REQUEST,                 24). % Controller/switch message
-define (OFPT_ROLE_REPLY,                   25). % Controller/switch message
%% Asynchronous message configuration.
-define (OFPT_GET_ASYNC_REQUEST,            26). % Controller/switch message
-define (OFPT_GET_ASYNC_REPLY,              27). % Controller/switch message
-define (OFPT_SET_ASYNC,                    28). % Controller/switch message
%% Meters and rate limiters configuration messages.
-define (OFPT_METER_MOD,                    29). % Controller/switch message
%% Controller role change event messages.
-define (OFPT_ROLE_STATUS,                  30). % Async message
%% Asynchronous messages.
-define (OFPT_TABLE_STATUS,                 31). % Async message
%% Request forwarding by the switch.
-define (OFPT_REQUESTFORWARD,               32). % Async message
%% Bundle operations (multiple messages as a single operation).
-define (OFPT_BUNDLE_CONTROL,               33). % Controller/switch message
-define (OFPT_BUNDLE_ADD_MESSAGE,           34). % Controller/switch message
%% Controller Status async message.
-define (OFPT_CONTROLLER_STATUS,            35). % Async message


%%%-------------------------------------------------------------------
%% OFP Error Types
%%%-------------------------------------------------------------------
-define (OFPET_HELLO_FAILED,                0). % Hello protocol failed.
-define (OFPET_BAD_REQUEST,                 1). % Request was not understood.
-define (OFPET_BAD_ACTION,                  2). % Error in action description.
-define (OFPET_BAD_INSTRUCTION,             3). % Error in instruction list.
-define (OFPET_BAD_MATCH,                   4). % Error in match.
-define (OFPET_FLOW_MOD_FAILED,             5). % Problem modifying flow entry.
-define (OFPET_GROUP_MOD_FAILED,            6). % Problem modifying group entry.
-define (OFPET_PORT_MOD_FAILED,             7). % Port mod request failed.
-define (OFPET_TABLE_MOD_FAILED,            8). % Table mod request failed.
-define (OFPET_QUEUE_OP_FAILED,             9). % Queue operation failed.
-define (OFPET_SWITCH_CONFIG_FAILED,        10). % Switch config request failed.
-define (OFPET_ROLE_REQUEST_FAILED,         11). % Controller Role request failed.
-define (OFPET_METER_MOD_FAILED,            12). % Error in meter.
-define (OFPET_TABLE_FEATURES_FAILED,       13). % Setting table features failed.
-define (OFPET_EXPERIMENTER,                16#ffff). % Experimenter error messages.

%%%-------------------------------------------------------------------
%% OFP HELLO_FAILED Error Codes
%%%-------------------------------------------------------------------
-define (OFPHFC_INCOMPATIBLE,               0). % No compatible version.
-define (OFPHFC_EPERM,                      1). % Permissions error.


%%%-------------------------------------------------------------------
%% OFPT_ERROR
%%%-------------------------------------------------------------------

-define (OFP_ERROR,                                 ofp_error).
-record (?OFP_ERROR,
{ type :: non_neg_integer(),
  code :: non_neg_integer(),
  data = <<>> :: binary()}).

-type ofp_error() :: #?OFP_ERROR{}.

-endif.