%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.8.0

-ifndef(im_pb).
-define(im_pb, true).

-define(im_pb_gpb_version, "4.8.0").

-ifndef('CS_FRENDS_ALL_FRIENDS_PB_H').
-define('CS_FRENDS_ALL_FRIENDS_PB_H', true).
-record(cs_frends_all_friends,
        {
        }).
-endif.

-ifndef('SC_FRENDS_ALL_FRIENDS_PB_H').
-define('SC_FRENDS_ALL_FRIENDS_PB_H', true).
-record(sc_frends_all_friends,
        {all_frends = []        :: [im_pb:p_frend()] | undefined % = 1
        }).
-endif.

-ifndef('P_FREND_PB_H').
-define('P_FREND_PB_H', true).
-record(p_frend,
        {user_id                :: integer(),       % = 1, 32 bits
         nickname               :: iodata()         % = 2
        }).
-endif.

-endif.
