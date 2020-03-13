-ifndef(def_chat_hrl).
-define(def_chat_hrl,1).
%%单聊
-record(chat_single,{
    userid = 0,
    ws_pid = 0
}).

-endif.