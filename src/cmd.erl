%%%-------------------------------------------------------------------
%%% @author fanjianping
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 3月 2020 12:48
%%%-------------------------------------------------------------------
-module(cmd).
-author("fanjianping").

%% API
-export([]).
-compile(export_all).

pt()->
    erlang:send(db,table_info_to_file),
    ok.
