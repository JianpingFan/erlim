%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2020 20:52
%%%-------------------------------------------------------------------
-module(erlim_gw).
-author("89710").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(erlim_gw_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #erlim_gw_state{}} | {ok, State :: #erlim_gw_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  process_flag(trap_exit,true),
  Routes    = routes(),
  Dispatch  = cowboy_router:compile(Routes),
  Port      = port(),
  TransOpts = [{port, Port}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},
  {ok, _}   = cowboy:start_clear(http, TransOpts, ProtoOpts),
  http:ets_user_online(),
  log4erl:info("start ~w success",[?MODULE]),
  {ok, #erlim_gw_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #erlim_gw_state{}) ->
  {reply, Reply :: term(), NewState :: #erlim_gw_state{}} |
  {reply, Reply :: term(), NewState :: #erlim_gw_state{}, timeout() | hibernate} |
  {noreply, NewState :: #erlim_gw_state{}} |
  {noreply, NewState :: #erlim_gw_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #erlim_gw_state{}} |
  {stop, Reason :: term(), NewState :: #erlim_gw_state{}}).
handle_call(_Request, _From, State = #erlim_gw_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #erlim_gw_state{}) ->
  {noreply, NewState :: #erlim_gw_state{}} |
  {noreply, NewState :: #erlim_gw_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #erlim_gw_state{}}).
handle_cast(_Request, State = #erlim_gw_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #erlim_gw_state{}) ->
  {noreply, NewState :: #erlim_gw_state{}} |
  {noreply, NewState :: #erlim_gw_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #erlim_gw_state{}}).
handle_info(_Info, State = #erlim_gw_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #erlim_gw_state{}) -> term()).
terminate(_Reason, _State = #erlim_gw_state{}) ->
  ok = cowboy:stop_listener(http),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #erlim_gw_state{},
    Extra :: term()) ->
  {ok, NewState :: #erlim_gw_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #erlim_gw_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

routes() ->
  [
    {'_', [
      {"/http/:action", http, []},
      {"/ws/token/:token", ws, []}
    ]}
  ].

port() ->
  case os:getenv("PORT") of
    false ->
      {ok, Port} = application:get_env(http_port),
      Port;
    Other ->
      list_to_integer(Other)
  end.