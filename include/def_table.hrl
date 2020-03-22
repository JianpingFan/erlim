-record(table_info,{table_name,primary_key,field_info}).

-define(TABLE_LIST, [users]).
-define(table_users,users).
-record(field_users,{pri_key,user_id,mobile,pwd,nickname,create_time}).


-define(sql_insert_users,sql_insert_users).
-define(sql_delete_users_by_pri_key,sql_delete_users_by_pri_key).
-define(sql_delete_users_by_user_id,sql_delete_users_by_user_id).
-define(sql_delete_users_by_mobile,sql_delete_users_by_mobile).
-define(sql_delete_users_by_pwd,sql_delete_users_by_pwd).
-define(sql_delete_users_by_nickname,sql_delete_users_by_nickname).
-define(sql_delete_users_by_create_time,sql_delete_users_by_create_time).


-define(PREPARE_SQL_LIST, [
{sql_insert_users,<<"insert into users (user_id,mobile,pwd,nickname,create_time) values (?,?,?,?,?)">>},
{sql_delete_users_by_pri_key,<<"delete from users where user_id = ?">>},
{sql_delete_users_by_user_id,<<"delete from users where user_id = ?">>},
{sql_delete_users_by_mobile,<<"delete from users where mobile = ?">>},
{sql_delete_users_by_pwd,<<"delete from users where pwd = ?">>},
{sql_delete_users_by_nickname,<<"delete from users where nickname = ?">>},
{sql_delete_users_by_create_time,<<"delete from users where create_time = ?">>}
]).

-define(TABLE_INFO_LIST,[
{table_info,users,[1],[{r_field,"user_id","int(11)","NO","PRI","undefined",""},{r_field,"mobile","bigint(11)","NO","","undefined",""},{r_field,"pwd","varchar(255)","NO","","undefined",""},{r_field,"nickname","varchar(255)","NO","","undefined",""},{r_field,"create_time","int(11)","NO","","undefined",""}]}
]).
