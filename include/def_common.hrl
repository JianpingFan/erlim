-ifndef(def_common_hrl).
-define(def_common_hrl,1).

-define(ETS_USER_ONLINE,ets_user_online). %%在线用户
-define(undefined,undefined).


-record(user_online,{
    token,
    user_id
}).







-define(CATCH(Expression), (
    try Expression
    catch
        ErrType:ErrReason ->
            log4erl:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, ErrReason, erlang:get_stacktrace(), ??Expression]),
            {'EXIT',{ErrType, ErrReason}}
    end
)).
-define(CATCH(Expression, Reason), (
    try Expression
    catch
        _:Reason ->
            log4erl:error("ErrReason:~1000p, Stack=~100000p, Expression=~s",[Reason, erlang:get_stacktrace(), ??Expression]),
            {'EXIT',{loose_errType, Reason}}
    end
)).
-define(CATCH(Expression, ErrType, Reason), (
    try Expression
    catch
        ErrType:Reason ->
            log4erl:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
            {'EXIT',{ErrType, Reason}}
    end
)).
-define(CATCH_WITH_OUTPUT(Expression,Reason,Err_Happen_Output),(
    try Expression
    catch
        ErrType:Reason ->
            log4erl:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
            {'EXIT',{ErrType, Reason}},
            Err_Happen_Output
    end
)).

-define(LOOSE_CATCH(Expression), (
    try Expression
    catch
        throw:SomeThing ->
            SomeThing;
        ErrType:Reason ->
            log4erl:error("ErrType:~1000p, ErrReason:~1000p, Stack=~100000p, Expression=~s",[ErrType, Reason, erlang:get_stacktrace(), ??Expression]),
            {'EXIT',{ErrType, Reason}}
    end
)).

-endif.