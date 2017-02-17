-module(m3u8_worker).

-behaviour(gen_server).

%% API.
-export([start_link/2,
         recv_part/2,
         upload_part/2,
         get_segment_name/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


recv_part(Pid, Bin) ->
    gen_server:cast(Pid, {recv_part, Bin}).

upload_part(Pid, MetaData) ->
    gen_server:cast(Pid, {upload_part, MetaData}).


%% API.
start_link(_Arg0, Args) ->
    gen_server:start_link(?MODULE, Args, []).
    %% gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    {port, Port} = lists:keyfind(port, 1, Args),
    BPort   = erlang:integer_to_binary(Port),
    {ok, _} = ranch:start_listener(BPort, 1, ranch_tcp, [{port, Port}],
                                   frecv_protocol, [{forward_to, self()}]),

    {module, Mod}   = lists:keyfind(module, 1, Args),
    {function, Fun} = lists:keyfind(function, 1, Args),
    {args, FunArgs} = lists:keyfind(args, 1, Args),

    {ok, #{socket => BPort, parts => [],
           mod => Mod, func => Fun, args => FunArgs}}.

handle_call(_Request, _From, State) ->
    lager:log(info, [], "unknown call ~n", []),
    {reply, ignored, State}.


handle_cast({upload_part, MetaData}, #{parts := Parts} = State) ->
    case lists:reverse(Parts) of
        [Part | Rest] ->
            RParts = lists:reverse(Rest),
            Name   = get_segment_name(MetaData),
            #{mod := Mod, func := Fun, args := Args} = State,
            erlang:apply(Mod, Fun, Args ++ [#{part => Part,
                                              name => Name, m3u8 => MetaData}]),
            {noreply, State#{parts := RParts}, 5000};
        [] ->
            {noreply, State, 5000}
    end;

handle_cast({recv_part, Bin}, #{parts := Parts} = State) ->
    Parts1 = [Bin | Parts],
    {noreply, State#{parts => Parts1}, 10000};

handle_cast(Request, State) ->
    lager:log(info, [], "received unkown cast ~p~n", [Request]),
    {noreply, State}.


handle_info(timeout, #{socket := Socket} = State) ->
    ranch:stop_listener(Socket),
    {ok, _} = port_manager:set_port_free(erlang:binary_to_integer(Socket)),
    {stop, normal, State};
handle_info(Info, State) ->
    lager:log(info, [], "~p received unkown message ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



get_segment_name(M3U8) ->
    [_, _, _, _ | PartNames] = binary:split(M3U8, [<<"\n">>], [global]),
    hd(get_names(PartNames, [])).

get_names([], Acc) -> Acc;
get_names([_], Acc) -> Acc;
get_names([Duration, Name | Rest], Acc) ->
    try <<Duration:56/bitstring>> of
        <<"#EXTINF">> ->
            get_names(Rest, [Name | Acc]);
        _ ->
            get_names(Rest, Acc)
    catch _:_ ->
            lager:log(info, [], "error while decoding part name"),
            get_names(Rest, Acc)
    end.
