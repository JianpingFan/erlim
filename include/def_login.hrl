-ifndef(def_login_hrl).
-define(def_login_hrl,1).


-record(user_cache,{
  userid = 0,
  mobile="",
  cookie = ""
}).

-endif.