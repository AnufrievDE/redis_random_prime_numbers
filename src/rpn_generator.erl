-module(rpn_generator).

-behaviour(gen_server).

-export([start_link/5]).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, handle_info/2, init/1, 
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-define(second_granularity, 1000).
-define(timeout, 5000). 
-define(check_interval, 1000).

-record(state, {
    eredis_pool :: atom(),
    rdb_list_key :: string(),
    nums_per_sec :: pos_integer(), %% speed requirement
    n :: pos_integer(), %% random number boarder: rand:uniform(1, state.n) + 1
    m :: pos_integer(), %% number of random numbers to generate at step
    m_seq = [] :: list(pos_integer()), %% dummy sequence 1..state.m, just not to create in a loop 
    timeout = ?timeout :: pos_integer(), %% when to stop server with {error, timeout}, if no messages coming
    nums_pushed = 0 :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(
    ManualStart :: boolean(),
    NumsPerSec :: pos_integer(),
    %NumsPerStep :: pos_integer(),
    %SleepPerStep :: pos_integer(),
    N :: pos_integer(),
    EredisPool :: atom(),
    ListKey :: string()) -> gen:start_ret().
%%--------------------------------------------------------------------
start_link(ManualStart, NumsPerSec, N, EredisPool, ListKey) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
        {ManualStart, NumsPerSec, N, EredisPool, ListKey}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({ManualStart, NumsPerSec, N, EredisPool, ListKey}) ->
    rand:seed(exro928ss, erlang:time()),
    %% set off_heap
    process_flag(message_queue_data, off_heap),
    %% Manual or Automatic start
    ManualStart orelse gen_server:cast(self(), start_timers),
    ?LOG(notice, "rpn_generator initialized"),

    {ok, #state{eredis_pool = EredisPool, rdb_list_key = ListKey,
        nums_per_sec = NumsPerSec, n = N - 1, timeout = ?timeout}}.

%%%===================================================================
handle_cast(generate, #state{eredis_pool = EredisPool, rdb_list_key = ListKey,
    n = N, m = M, m_seq = MSeq, nums_pushed = I, timeout = T} = State) ->
    %% Generate and push numbers to ListKey
    generate_andalso_push(N, MSeq, true, EredisPool, ListKey),
    {noreply, State#state{nums_pushed = I + M}, T};

handle_cast(nums_pushed, #state{nums_pushed = I, timeout = T} = State) ->
    log_nums_pushed(I),
    {noreply, State#state{nums_pushed = 0}, T};

handle_cast(start_timers, #state{nums_per_sec = NumsPerSec} = State) ->
    {_SleepPerStep, NumsPerStep} = start_timers(NumsPerSec),
    {noreply, State#state{m = NumsPerStep, m_seq = lists:seq(1, NumsPerStep)}};

handle_cast({start_timers, NumsPerSec}, State) ->
    {_SleepPerStep, NumsPerStep} = start_timers(NumsPerSec),
    {noreply, State#state{nums_per_sec = NumsPerSec,
        m = NumsPerStep, m_seq = lists:seq(1, NumsPerStep)}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%%===================================================================
handle_info(timeout, State) ->
    {stop, {error, timeout}, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Test purpose callbacks
%%%===================================================================
handle_call({generate, M}, _From, #state{eredis_pool = EredisPool,
    rdb_list_key = ListKey, n = N, nums_pushed = I} = State) ->
    MSeq = lists:seq(1, M),
    %% Generate and push numbers to ListKey
    BinNums = generate_andalso_push(N, MSeq, true, EredisPool, ListKey),
    {reply, BinNums, State#state{nums_pushed = I + M}};

handle_call(nums_pushed, _From, #state{nums_pushed = I} = State) ->
    log_nums_pushed(I),
    {reply, I, State#state{nums_pushed = 0}};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%%===================================================================
terminate(Reason, _State) ->
    ?LOG(debug, "rpn_generator is terminating..."),
    ?LOG(debug, "Terminate reason: ~p\n Message queue length: ~p",
        [Reason, process_info(self(), message_queue_len)]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
log_nums_pushed(I) ->
    ?LOG(debug, "rpn_generator has pushed ~p nums", [I]).

generate_andalso_push(N, MSeq, Push, EredisPool, ListKey) ->
    %% 2*O(MSeq) just due to test purpose needs to return numbers
    BinNums = [integer_to_binary(rand:uniform(N + 1)) || _ <- MSeq],
    Reqs = rpush_numbers(ListKey, BinNums),
    Push andalso poolboy:transaction(EredisPool,
        fun(W) -> eredis:qp(W, Reqs)
        end),
    BinNums.

rpush_numbers(ListKey, BinNums) ->
    [rpush(ListKey, BinNum) || BinNum <- BinNums].
    
rpush(ListKey, Num) ->
    ["RPUSH", ListKey, Num].

start_timers(NumsPerSec) ->
    {SleepPerStep, NumsPerStep} =
        case NumsPerSec >= ?second_granularity of
          true -> {1, ceil(NumsPerSec/?second_granularity)};
          false -> {?second_granularity div NumsPerSec, 1}
        end,
    timer:apply_interval(SleepPerStep, gen_server, cast,
        [self(), generate]),
    timer:apply_interval(?check_interval, gen_server, cast,
        [self(), nums_pushed]),
    {SleepPerStep, NumsPerStep}.