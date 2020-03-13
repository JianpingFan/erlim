%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 2月 2020 20:15
%%%-------------------------------------------------------------------
-module(chat_single).
-author("89710").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).
-include("def_include.hrl").
-define(SERVER, ?MODULE).
-define(CHAT_SINGLE_CACHE,chat_single_cache).
-record(chat_single_state, {}).

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
    {ok, State :: #chat_single_state{}} | {ok, State :: #chat_single_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    process_flag(trap_exit,true),
    log4erl:info("start ~w success",[?MODULE]),
    {ok, #chat_single_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #chat_single_state{}) ->
    {reply, Reply :: term(), NewState :: #chat_single_state{}} |
    {reply, Reply :: term(), NewState :: #chat_single_state{}, timeout() | hibernate} |
    {noreply, NewState :: #chat_single_state{}} |
    {noreply, NewState :: #chat_single_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #chat_single_state{}} |
    {stop, Reason :: term(), NewState :: #chat_single_state{}}).
handle_call(_Request, _From, State = #chat_single_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #chat_single_state{}) ->
    {noreply, NewState :: #chat_single_state{}} |
    {noreply, NewState :: #chat_single_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #chat_single_state{}}).
handle_cast(_Request, State = #chat_single_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #chat_single_state{}) ->
    {noreply, NewState :: #chat_single_state{}} |
    {noreply, NewState :: #chat_single_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #chat_single_state{}}).
handle_info({save_ws_pid,UserID,WsPid}, State = #chat_single_state{})->
    save_ws_pid(UserID,#chat_single{userid = UserID,ws_pid = WsPid}),
    {noreply, State};
handle_info({text,_FromID,ToID,Msg}, State = #chat_single_state{})->   %% 文本聊天消息
    Pid = get_ws_pid(ToID),
    log4erl:info("handle_info text = ~w",[Pid]),
    Pid ! {reply,Msg},
    {noreply, State};
handle_info(_Info, State = #chat_single_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #chat_single_state{}) -> term()).
terminate(_Reason, _State = #chat_single_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #chat_single_state{},
    Extra :: term()) ->
    {ok, NewState :: #chat_single_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #chat_single_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

save_ws_pid(UserID,ChatSingle) when is_record(ChatSingle,chat_single)->
    put({?CHAT_SINGLE_CACHE,UserID},ChatSingle).

get_ws_pid(UserID)->
    case get({?CHAT_SINGLE_CACHE,UserID}) of
        #chat_single{ws_pid = WsPid}->
            case is_pid(WsPid) of
                true->WsPid;
                _->false
            end;
        _->
            false
    end.