%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 3月 2020 20:40
%%%-------------------------------------------------------------------
-module(db_analysis_table).
-author("89710").
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
%% API
-export([analysis_table/0]).

analysis_table()->
%%    case mysql:start_link(erlim, "127.0.0.1", "fjp", "fjp123456", "erlim") of
%%        {ok,_Pid}->
%%                ok;
%%        {error,{already_started,_}}->
%%            {stop, conn_db_fail};
%%        _->
%%            throw({error,conn_db_fail})
%%    end,
    io:format("analysis_table"),
    TableList = db:fetch("show tables"),
    put(?TABLE_ATOM_LIST,[list_to_atom(binary_to_list(TableBin))||[TableBin]<-TableList]),
    lists:foreach(
        fun(TableAtom)->
            desc_table(TableAtom)
        end,
        get(?TABLE_ATOM_LIST)
    ),
    weite_file(),
    ok.

desc_table(TableAtom)->
    TableInfoList = db:fetch(lists:concat(["desc ",atom_to_list(TableAtom)])),
    List =
    lists:map(
        fun([Field,Type,Null,Key,Default,Extra])->
            #r_field{field = util:to_string(Field),type = util:to_string(Type),null = util:to_string(Null),key = util:to_string(Key),default = util:to_string(Default),extra = util:to_string(Extra)}
        end,
        TableInfoList
    ),
    put({?TABLE_FIELD_ATOM_LIST,TableAtom},List),
%%    io:format("init TableList = ~w ~n",[get({?TABLE_FIELD_ATOM_LIST,TableAtom})]),
    ok.

weite_file()->
    {ok, S} = file:open("include/def_table.hrl", write),
    lists:foreach(
        fun(TableAtom)->
            io:format(S,"-define(table_~s,table_~s).~n",[string:to_lower(atom_to_list(TableAtom)),string:to_lower(atom_to_list(TableAtom))]),
            io:format(S,"-record(field_~s,{",[string:to_lower(atom_to_list(TableAtom))]),
            FieldList = [Field||#r_field{field = Field}<-get({?TABLE_FIELD_ATOM_LIST,TableAtom})],
            Len = length(FieldList),
            lists:foldl(
                fun(Field,Index)->
                    case Index of
                        Len->
                            io:format(S,"~s",[Field]);
                        _->
                            io:format(S,"~s,",[Field])
                    end,
                    Index + 1
                end,
                1,
                FieldList
            ),
            io:format(S,"}).~n~n~n",[])
        end,
        get(?TABLE_ATOM_LIST)
    ),

    lists:foreach(
        fun(TableAtom)->
            io:format(S,"-define(sql_insert_~s,sql_insert_~s).~n",[string:to_lower(atom_to_list(TableAtom)),string:to_lower(atom_to_list(TableAtom))]),
            [io:format(S,"-define(sql_delete_~s_by_~s,sql_delete_~s_by_~s).~n",[string:to_lower(atom_to_list(TableAtom)),Field,string:to_lower(atom_to_list(TableAtom)),Field])||#r_field{field = Field}<-get({?TABLE_FIELD_ATOM_LIST,TableAtom})]
        end,
        get(?TABLE_ATOM_LIST)
    ),

    io:format(S,"~n~n-define(PREPARE_SQL_LIST, [~n",[]),
    lists:foreach(
        fun(TableAtom)->
            io:format(S,"{sql_insert_~s,<<\"insert into ~s (",[string:to_lower(atom_to_list(TableAtom)),string:to_lower(atom_to_list(TableAtom))]),
            FieldList = [Field||#r_field{field = Field}<-get({?TABLE_FIELD_ATOM_LIST,TableAtom})],
            Len = length(FieldList),
            lists:foldl(
                fun(Field,Index)->
                    case Index of
                        Len->
                            io:format(S,"~s",[Field]);
                        _->
                            io:format(S,"~s,",[Field])
                    end,
                    Index + 1
                end,
                1,
                FieldList
            ),
            io:format(S,") values (",[]),
            lists:foldl(
                fun(_Field,Index)->
                    case Index of
                        Len->
                            io:format(S,"?)\">>},~n",[]);
                        _->
                            io:format(S,"?,",[])
                    end,
                    Index + 1
                end,
                1,
                FieldList
            )
        end,
        get(?TABLE_ATOM_LIST)
    ),

    lists:foreach(
        fun(TableAtom)->
            FieldList = [Field||#r_field{field = Field}<-get({?TABLE_FIELD_ATOM_LIST,TableAtom})],
            Len = length(FieldList),
            lists:foldl(
                fun(Field,Index)->
                    case Index of
                        Len->
                            io:format(S,"{sql_delete_~s_by_~s,<<\"delete from ~s where ~s = ?\">>}~n",[string:to_lower(atom_to_list(TableAtom)),Field,string:to_lower(atom_to_list(TableAtom)),Field]);
                        _->
                            io:format(S,"{sql_delete_~s_by_~s,<<\"delete from ~s where ~s = ?\">>},~n",[string:to_lower(atom_to_list(TableAtom)),Field,string:to_lower(atom_to_list(TableAtom)),Field])
                    end,
                    Index + 1
                end,
                1,
                FieldList
            )
        end,
        get(?TABLE_ATOM_LIST)
    ),
    io:format(S,"]).~n",[]),
    ok.
