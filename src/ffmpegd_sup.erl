-module(ffmpegd_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_m3u8_worker/0,
         start_m3u8_worker/3]).

-export([init/1]).


-define(CHILD(Id, Mod, Args, Restart, Type), #{id => Id,
                                               start => {Mod, start_link, Args},
                                               restart => Restart,
                                               shutdown => 5,
                                               type => Type,
                                               modules => [Mod]}).

-define(SIMPLE_CHILD(Id, WorkerMod), ?CHILD(Id, WorkerMod, [[]], transient,
                                            worker)).

-define(SIMPLE_SUP(SupId, WorkerMod),
        ?CHILD(SupId, simple_sup,
               [SupId, simple_one_for_one, [?SIMPLE_CHILD(WorkerMod, WorkerMod)]], permanent,
               supervisor)).

%% API.
start_m3u8_worker() ->
    start_m3u8_worker(data_handler, write_to_file, []).

start_m3u8_worker(Mod, Fun, Args) ->
    {ok, Port} = port_manager:get_free_port(),
    {ok, _Pid} = supervisor:start_child(m3u8_worker_sup, [[{module, Mod},
                                                           {function, Fun},
                                                           {args, Args},
                                                           {port, Port}]]),
    {ok, Port}.


-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Processes = [?SIMPLE_SUP(m3u8_worker_sup, m3u8_worker),
                 ?SIMPLE_CHILD(port_manager, port_manager)
                ],
    {ok, {{one_for_one, 1, 5}, Processes}}.
