%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 2月 2020 12:54
%%%-------------------------------------------------------------------
-module(db).
-author("89710").

-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("def_include.hrl").
-define(TABLE_ATOM_LIST,table_list).
-define(TABLE_FIELD_ATOM_LIST,table_field_atom_list).
-record(r_field,{
  field,
  type,
  null,
  key,
  default,
  extra
}).
-record(db_state, {}).

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
  {ok, State :: #db_state{}} | {ok, State :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  process_flag(trap_exit,true),
  {ok, Host} = application:get_env(db_host),
  {ok, User} = application:get_env(db_user),
  {ok, Password} = application:get_env(db_pwd),
  {ok, Database} = application:get_env(db_name),
  case mysql:start_link(erlim, Host, User, Password, Database) of
    {ok,_Pid}->
      prepare(),
      log4erl:info("start ~w success",[?MODULE]),
      {ok, #db_state{}};
    _->
      {stop, conn_db_fail}
  end.


%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #db_state{}) ->
  {reply, Reply :: term(), NewState :: #db_state{}} |
  {reply, Reply :: term(), NewState :: #db_state{}, timeout() | hibernate} |
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #db_state{}} |
  {stop, Reason :: term(), NewState :: #db_state{}}).
%%handle_call({?USER_CREATE,User,Pwd,NickName}, _From, State = #db_state{}) ->
%%  Res = exec(?USER_CREATE,[User, Pwd,NickName,util:now()]),
%%  {reply, Res, State};
handle_call({select,Sql}, _From, State = #db_state{}) ->
  Res = fetch(Sql),
  log4erl:info("select Res = ~w ",[Res]),
  {reply, Res, State};
handle_call(_Request, _From, State = #db_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #db_state{}) ->
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #db_state{}}).
handle_cast(_Request, State = #db_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #db_state{}) ->
  {noreply, NewState :: #db_state{}} |
  {noreply, NewState :: #db_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #db_state{}}).
handle_info(table_info_to_file, State = #db_state{})->
  db_analysis_table:analysis_table(),
  {noreply, State};
handle_info({write_db,TableAtom,Key,RowData}, State = #db_state{})->
  delete_data_by_primary_key(TableAtom,Key),
  List = tuple_to_list(RowData),
  [_,_|InsetData] = List,
  delete_data_by_primary_key(TableAtom,InsetData),
  {noreply, State};
handle_info({get_cache,TaskList}, State = #db_state{})->
  get_cache(TaskList),
  {noreply, State};
handle_info({select,From,Sql}, State = #db_state{})->
  From ! {reply,fetch(Sql)},
  {noreply, State};
handle_info(_Info, State = #db_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #db_state{}) -> term()).
terminate(_Reason, _State = #db_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #db_state{},
    Extra :: term()) ->
  {ok, NewState :: #db_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #db_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_cache([Tab|TabList])->
  get_cache1(Tab,0,10),
  get_cache(TabList);
get_cache([])->
  ok.


get_cache1(Table,Index,Step)->
  Sql = lists:concat(["select * from ",atom_to_list(Table)," limit ",Index," , ",Step]),
  DataList = fetch(list_to_binary(Sql)),
  Len = length(DataList),
  case Len < Step of
    true->
      erlang:send(cache,{callback_cache,Table,DataList}),
      ok;
    _->
      erlang:send(cache,{callback_cache,Table,DataList}),
      get_cache1(Table,Index + Step,Step)
  end.


fetch(Sql)->
  case mysql:fetch(erlim, Sql) of
    {data, Result}->
      log4erl:info("fetch Result = ~w",[Result]),
      mysql:get_result_rows(Result);
    ERR->
      log4erl:info("fetch ERR = ~w",[ERR]),
      []
  end.

exec(Atom,ArgList)->
  case mysql:execute(erlim,Atom,ArgList) of
    {updated,_MysqlResult}->
      true;
    {error,Error}->
      log4erl:info("execute Error = ~w",[Error]),
      false
  end.

prepare()->
  lists:foreach(
    fun({PrepareAtom,PrepareSql})->
      mysql:prepare(PrepareAtom, PrepareSql)
    end,
    ?PREPARE_SQL_LIST
  ).

table_string_name(TableAtom) when is_atom(TableAtom)->
  S = atom_to_list(TableAtom),
  lists:sublist(S,7,length(S)).
table_atom_name(TableAtom) when is_atom(TableAtom)->
  list_to_atom(table_string_name(TableAtom)).

%%根据主键删除数据
delete_data_by_primary_key(TableAtom,ArgList)->
  PreAtom = list_to_atom(lists:concat(["sql_delete_",table_string_name(TableAtom),"_by_pri_key"])),
  exec(PreAtom,ArgList).

%%插入数据
insert_data_to_table(TableAtom,ArgList)->
  PreAtom = list_to_atom(lists:concat(["sql_insert_",table_string_name(TableAtom)])),
  exec(PreAtom,ArgList).

