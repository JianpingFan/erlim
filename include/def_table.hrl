-define(table_id_manage,table_id_manage).
-record(field_id_manage,{id,pid}).


-define(table_users,table_users).
-record(field_users,{user_id,mobile,pwd,nickname,create_time}).


-define(sql_insert_id_manage,sql_insert_id_manage).
-define(sql_delete_id_manage_by_id,sql_delete_id_manage_by_id).
-define(sql_delete_id_manage_by_pid,sql_delete_id_manage_by_pid).
-define(sql_insert_users,sql_insert_users).
-define(sql_delete_users_by_user_id,sql_delete_users_by_user_id).
-define(sql_delete_users_by_mobile,sql_delete_users_by_mobile).
-define(sql_delete_users_by_pwd,sql_delete_users_by_pwd).
-define(sql_delete_users_by_nickname,sql_delete_users_by_nickname).
-define(sql_delete_users_by_create_time,sql_delete_users_by_create_time).


-define(PREPARE_SQL_LIST, [
{sql_insert_id_manage,<<"insert into id_manage (id,pid) values (?,?)">>},
{sql_insert_users,<<"insert into users (user_id,mobile,pwd,nickname,create_time) values (?,?,?,?,?)">>},
{sql_delete_id_manage_by_id,<<"delete from id_manage where id_manage = ?">>},
{sql_delete_id_manage_by_pid,<<"delete from id_manage where id_manage = ?">>}
{sql_delete_users_by_user_id,<<"delete from users where users = ?">>},
{sql_delete_users_by_mobile,<<"delete from users where users = ?">>},
{sql_delete_users_by_pwd,<<"delete from users where users = ?">>},
{sql_delete_users_by_nickname,<<"delete from users where users = ?">>},
{sql_delete_users_by_create_time,<<"delete from users where users = ?">>}
]).
