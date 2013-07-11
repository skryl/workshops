-module(mydb_app).

-behavior(e2_application).

-export([init/0]).

%%%===================================================================
%%% e2_application callbacks
%%%===================================================================

init() ->
    {ok, [{mydb_server, start_link, [1234]},
          {mydb_data, start_link, ["/tmp/data.db"]}] }.
