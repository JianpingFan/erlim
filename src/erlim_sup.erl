%%%-------------------------------------------------------------------
%% @doc erlim1 top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    application:start(log4erl),
    {ok,RootDir} = file:get_cwd(),
    log4erl:conf(lists:concat([RootDir,"/","priv/log4erl.conf"])),
    Servers = [
        {erlim_gw, {erlim_gw, start_link, []},
            permanent, 2000, worker, []},
        {db, {db, start_link, []},
            permanent, 2000, worker, []},
        {cache, {cache, start_link, []},
            permanent, 2000, worker, []},
        {login, {login, start_link, []},
            permanent, 2000, worker, []}
%%        {chat, {chat, start_link, []},
%%            permanent, 2000, worker, []}
    ],
    {ok, {{one_for_all, 0, 1}, Servers}}.

%%====================================================================
%% Internal functions
%%====================================================================
