%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 2月 2020 14:44
%%%-------------------------------------------------------------------
-module(login).
-author("89710").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-include("def_include.hrl").
-record(login_state, {}).
-define(USER_CACHE, user_cache).
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
    {ok, State :: #login_state{}} | {ok, State :: #login_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    process_flag(trap_exit,true),
    log4erl:info("start ~w success",[?MODULE]),
    {ok, #login_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #login_state{}) ->
    {reply, Reply :: term(), NewState :: #login_state{}} |
    {reply, Reply :: term(), NewState :: #login_state{}, timeout() | hibernate} |
    {noreply, NewState :: #login_state{}} |
    {noreply, NewState :: #login_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #login_state{}} |
    {stop, Reason :: term(), NewState :: #login_state{}}).
handle_call({user_login,User,Passwd}, _From, State = #login_state{}) ->
    case user_login(User, Passwd) of
        {UserID,Cookie} when is_integer(UserID) andalso UserID > 0 ->
            {reply, {UserID,Cookie}, State};
        _->
            {reply, fail, State}
    end;
handle_call(_Request, _From, State = #login_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #login_state{}) ->
    {noreply, NewState :: #login_state{}} |
    {noreply, NewState :: #login_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #login_state{}}).
handle_cast(_Request, State = #login_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #login_state{}) ->
    {noreply, NewState :: #login_state{}} |
    {noreply, NewState :: #login_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #login_state{}}).
handle_info({user_create,User}, State = #login_state{}) ->
    user_create(User, <<"123456">>, <<"fanjianping">>),
    {noreply, State};
handle_info({user_login,From,User}, State = #login_state{}) ->
    case user_login(User, "123456") of
        UserID when is_integer(UserID) ->
            From ! {login_return, UserID};
        _->
            ignore
    end,
    {noreply, State};
handle_info(_Info, State = #login_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #login_state{}) -> term()).
terminate(_Reason, _State = #login_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #login_state{},
    Extra :: term()) ->
    {ok, NewState :: #login_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #login_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_create(User, Pwd, NickName) ->
    gen_server:call(db, {user_create, User, Pwd, NickName}).

user_login(Mobile, Pwd) ->
    Sql = io_lib:format("select * from users where mobile = '~s'", [Mobile]),
    case gen_server:call(db, {select, list_to_binary(Sql)}) of
        [[IDBin, MobileBin, PwdBin, _NickNameBin, _CreateTimeBin]] ->
            case binary_to_integer(MobileBin) == binary_to_integer(Mobile) andalso binary_to_list(PwdBin) == binary_to_list(Pwd) of
                true ->
                    Cookie = integer_to_list(util:random_int(1000000,9999999)),
                    set_user_cache(binary_to_integer(IDBin), #user_cache{userid = binary_to_integer(IDBin), mobile = binary_to_integer(Mobile),cookie = Cookie}),
                    log4erl:info("login success "),
                    {binary_to_integer(IDBin),Cookie};
                _ ->
                    false
            end;
        _ ->
            false
    end.

user_is_login(UserID)->
    case get_user_cahe(UserID) of
        #user_cache{}->true;
        _->false
    end.

set_user_cache(UserID, Cache) when is_record(Cache, user_cache) ->
    put({?USER_CACHE, UserID}, Cache).

get_user_cahe(UserID) ->
    get({?USER_CACHE, UserID}).

