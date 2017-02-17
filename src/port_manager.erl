-module(port_manager).

-behaviour(gen_server).

%% API.
-export([start_link/1,
         get_free_port/0,
         set_port_free/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).



%% API.
get_free_port() ->
    gen_server:call(?MODULE, {get_port_free}).

set_port_free(Port) ->
    gen_server:call(?MODULE, {set_port_free, Port}).


-spec start_link([tuple()]) -> {ok, pid()}.
start_link(_Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
init(Args) ->
    {ok, [Start, End]} = application:get_env(ffmpegd, port_range),
    FreePorts = lists:seq(Start, End),
    lager:log(info, [], "~p: ~p~n", [?MODULE, Args]),
    {ok, #{free_ports => FreePorts, busy_ports => []}}.



handle_call({get_port_free}, _From, #{free_ports := [Port | Rest],
                                      busy_ports := BP} = State) ->
    {reply, {ok, Port}, State#{free_ports => Rest,
                               busy_ports => [Port | BP]}};

handle_call({get_port_free}, _From, #{free_ports := []} = State) ->
    {reply, {error, not_avail}, State};

handle_call({set_port_free, Port}, _From, #{busy_ports := BP,
                                            free_ports := FP} = State) ->
    BP2 = lists:delete(Port, BP),
    FP2 = [Port | FP],
    {reply, {ok, Port}, State#{busy_ports => BP2,
                               free_ports => FP2}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(Request, State) ->
    lager:log(info, [], "received unkown cast ~p~n", [Request]),
    {noreply, State}.


handle_info(Info, State) ->
    lager:log(info, [], "~p received unkown message ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, #{socket := Socket} = _State) ->
    lager:log(info, [], "shutting down ~p~n", [Socket]),
    ranch:stop_listener(Socket).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

