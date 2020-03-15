%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 3月 2020 20:54
%%%-------------------------------------------------------------------
-module(cache).
-author("89710").
-include("def_include.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(IS_HAVA_NEW_DATA,is_hava_new_data). %% [{table,Key}|...] 待写数据库表和key
-define(TICK_WRITE_TO_DB_INTERVAL,5000).

-define(DATA_KEY,data_key). %% 内存中缓存的数据库数据
-record(table_data,{
    data,   %%数据坨
    key,
    status  %%数据状态

}).

-record(cache_state, {}).

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
    {ok, State :: #cache_state{}} | {ok, State :: #cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:send_after(2000,self(),get_cache),
    erlang:send_after(?TICK_WRITE_TO_DB_INTERVAL,self(),write_to_db),
    log4erl:info("start ~w success",[?MODULE]),
    {ok, #cache_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #cache_state{}) ->
    {reply, Reply :: term(), NewState :: #cache_state{}} |
    {reply, Reply :: term(), NewState :: #cache_state{}, timeout() | hibernate} |
    {noreply, NewState :: #cache_state{}} |
    {noreply, NewState :: #cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #cache_state{}} |
    {stop, Reason :: term(), NewState :: #cache_state{}}).
%%{get_data_by_field,table_users,4,<<"17380630290">>},
handle_call({get_data_by_field,TableAtom,Index,FieldData}, _From, State = #cache_state{}) ->
    Data = get_data_by_field(TableAtom,Index,FieldData),
    log4erl:info("get_data_by_field Data = ~w",[Data]),
    {reply, Data, State};
handle_call(_Request, _From, State = #cache_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #cache_state{}) ->
    {noreply, NewState :: #cache_state{}} |
    {noreply, NewState :: #cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #cache_state{}}).
handle_cast(_Request, State = #cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #cache_state{}) ->
    {noreply, NewState :: #cache_state{}} |
    {noreply, NewState :: #cache_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #cache_state{}}).
handle_info(write_to_db, State = #cache_state{})->
    case get_waite_to_db_key() of
        []->ok;
        Data->
            lists:foreach(
                fun({TableAtom,Key})->
                    case get_data_by_pri_key(TableAtom,Key) of
                        RowData when length(RowData) > 0 ->
                            erlang:send(db,{write_db,TableAtom,Key,RowData});
                        Err->
                            log4erl:warn("write_to_db TableAtom ~w,Key = ~w,Err = ~w",[TableAtom,Key,Err]),
                            ok
                    end
                end,
                Data
            )
    end,
    erlang:send_after(?TICK_WRITE_TO_DB_INTERVAL,self(),write_to_db),
    {noreply, State};
handle_info(get_cache, State = #cache_state{})->
    erlang:send(db,{get_cache,?TABLE_LIST}),
    {noreply, State};
handle_info({callback_cache,TableAtom,RowDataList}, State)->
    put_table_data(TableAtom,RowDataList),
    {noreply, State};
handle_info(_Info, State = #cache_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #cache_state{}) -> term()).
terminate(_Reason, _State = #cache_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #cache_state{},
    Extra :: term()) ->
    {ok, NewState :: #cache_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #cache_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% IndexKey #field_users....
get_data_by_pri_key(TableAtom,KeyData)->
    AllData = get_table_data(db:table_atom_name(TableAtom)),
    case lists:keyfind(KeyData,2,AllData) of
        Data when is_tuple(Data)->Data;
        _->[]
    end.

get_data_by_field(TableAtom,Index,FieldData)->
    AllData = get_table_data(db:table_atom_name(TableAtom)),
    get_data_by_field(Index,FieldData,AllData,[]).
get_data_by_field(Index,FieldData,[RowData|AllData],Acc)->
    case lists:nth(Index,tuple_to_list(RowData)) == FieldData of
        true->get_data_by_field(Index,FieldData,AllData,[RowData|Acc]);
        _->get_data_by_field(Index,FieldData,AllData,Acc)
    end;
get_data_by_field(_Index,_FieldData,[],Acc)->lists:reverse(Acc).


write_data(TableAtom,DataTup)->
    case lists:member(db:table_atom_name(TableAtom),?TABLE_LIST) of
        false->
            log4erl:error("write_data TableAtom = ~w not found",[TableAtom]),
            ok;
        true->
            AllData = get_table_data(db:table_atom_name(TableAtom)),
            List = tuple_to_list(DataTup),
            [_,PrimaryKeyData|_] = List,
            NewAllData = lists:keystore(PrimaryKeyData,2,AllData,DataTup),
            put_waite_to_db_key({TableAtom,PrimaryKeyData}),
            put({?DATA_KEY,TableAtom},NewAllData)
    end,
    ok.

get_waite_to_db_key()->
    case get(?IS_HAVA_NEW_DATA) of
        ?undefined->[];
        Data->Data
    end.
put_waite_to_db_key({TableAtom,KeyData})->
    Data = get_waite_to_db_key(),
    put(?IS_HAVA_NEW_DATA,[{TableAtom,KeyData}|Data]).




get_table_data(TableAtom)->
    case get({?DATA_KEY,TableAtom}) of
        ?undefined->[];
        Data->Data
    end.

put_table_data(TableAtom,RowDataList)->
    log4erl:info("put_table_data RowDataList = ~w",[RowDataList]),
    case lists:keyfind(TableAtom,#table_info.table_name,?TABLE_INFO_LIST) of
        #table_info{primary_key = PrimaryKey}->
            put_table_data(TableAtom,PrimaryKey,RowDataList,get_table_data(TableAtom));
        _->
            log4erl:error("put_table_data ~w not have primary_key = ~w",[TableAtom]),
            ok
    end.

put_table_data(TableAtom,PrimaryKey,[RowData|RowDataList],OldDataList)->
    Tuple = list_to_tuple([list_to_atom(lists:concat(["field_",atom_to_list(TableAtom)])),primary_key_data(PrimaryKey,RowData,[])|RowData]),
    log4erl:info("put_table_data Tuple = ~w",[Tuple]),
    put_table_data(TableAtom,PrimaryKey,RowDataList,[Tuple|OldDataList]);
put_table_data(TableAtom,_PrimaryKey,[],OldDataList)->
    put({?DATA_KEY,TableAtom},OldDataList).

primary_key_data([Index|PrimaryKey],RowData,Acc)->
    primary_key_data(PrimaryKey,RowData,[lists:nth(Index,RowData)|Acc]);
primary_key_data([],_RowData,Acc)->lists:reverse(Acc).