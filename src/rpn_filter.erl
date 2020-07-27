-module(rpn_filter).

-behaviour(gen_server).

-export([start_link/5]).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, handle_info/2, init/1, 
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-define(check_interval, 1000).

-record(state, {
    eredis_pool :: atom(),
    rdb_list_key :: string(),
    rdb_result_set_key :: string(),
    nums_to_fetch :: pos_integer(),
    nums_fetched = 0 :: pos_integer()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(
    ManualStart :: boolean(),
    NumsPerSec :: pos_integer(),
    EredisPool :: atom(),
    ListKey :: string(),
    RSetKey :: string()) -> gen:start_ret().
%%--------------------------------------------------------------------
start_link(ManualStart, NumsPerSec, EredisPool, ListKey, RSetKey) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
        {ManualStart, NumsPerSec, EredisPool, ListKey, RSetKey}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({ManualStart, NumsPerSec, EredisPool, ListKey, RSetKey}) ->
    %% Manual or Automatic start
    ManualStart orelse gen_server:cast(self(), start_timer),
    ?LOG(notice, "rpn_filter initialized"),
    %% Try to fetch twice more than NumsPerSec. Helpful when generator experiencing cpu starvation
    {ok, #state{eredis_pool = EredisPool, nums_to_fetch = 2*NumsPerSec,
        rdb_list_key = ListKey, rdb_result_set_key = RSetKey}}.

%%%===================================================================
handle_cast(fetch, #state{eredis_pool = EredisPool, nums_to_fetch = M,
    rdb_list_key = ListKey, rdb_result_set_key = RSetKey} = State) ->
    %% Fetch numbers from ListKey, filter them, and save part to RSetKey
    {_NumsFetched, _BinPrimeNums} =
        process_fetch_req(M, EredisPool, ListKey, RSetKey),
    {noreply, State};

handle_cast(start_timer, State) ->
    timer:apply_interval(?check_interval, gen_server, cast, [self(), fetch]),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%%===================================================================
handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Test purpose callbacks
%%%===================================================================
handle_call(fetch, _From, #state{eredis_pool = EredisPool, nums_to_fetch = M,
    rdb_list_key = ListKey, rdb_result_set_key = RSetKey} = State) ->
    %% Fetch numbers from ListKey, filter them, and save part to RSetKey
    {NumsFetched, BinPrimeNums} =
        process_fetch_req(M, EredisPool, ListKey, RSetKey),
    {reply, {NumsFetched, BinPrimeNums}, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%%===================================================================
terminate(Reason, _State) ->
    ?LOG(debug, "rpn_filter is terminating..."),
    ?LOG(debug, "Terminate reason: ~p\n Message queue length: ~p",
        [Reason, process_info(self(), message_queue_len)]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
process_fetch_req(N, EredisPool, ListKey, RSetKey) ->
    BinNums = fetch(N, EredisPool, ListKey),
    NumsFetched = length(BinNums),
    log_nums_fetched(NumsFetched),

    {NumsFetched, filter_andalso_push(BinNums, true, EredisPool, RSetKey)}.

log_nums_fetched(N) ->
    ?LOG(debug, "rpn_filter has fetched ~p nums", [N]).

fetch(N, EredisPool, ListKey) ->
    Reqs = lrange_ltrim(ListKey, N),
    [{ok, BinNums}, {ok, <<"OK">>}] = 
        poolboy:transaction(EredisPool,
            fun(W) -> eredis:qp(W, Reqs) end),
    BinNums.

filter_andalso_push(BinNums, Push, EredisPool, RSetKey) ->
    BinPrimeNums = lists:filter(
        fun(BinNum) -> rpn_lib:is_prime(binary_to_integer(BinNum))
        end, BinNums),
    case Push andalso BinPrimeNums /= [] of
        true ->
            {ok, _} = poolboy:transaction(EredisPool,
                fun(W) -> eredis:q(W, sadd(RSetKey, BinPrimeNums)) end);
        false -> ok
    end,
    BinPrimeNums.

lrange_ltrim(ListKey, N) ->
    [["LRANGE", ListKey, 0, N-1], ["LTRIM", ListKey, N, -1]].

sadd(SetKey, BinNums) ->
    ["SADD", SetKey | BinNums].