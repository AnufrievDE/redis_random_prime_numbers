-module(rpn_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-define(test_pool, default_pool).
-define(test_list_key, "test_list_key").
-define(test_result_set_key, "test_result_set_key").
-define(test_numbers_per_sec, 3000).
-define(test_n, 1000).

-define(prime_numbers_0_1000, [
    2,3,5,7,11,13,17,19,23,29,31,37,
    41,43,47,53,59,61,67,71,73,79,83,89,
    97,101,103,107,109,113,127,131,137,139,149,151,
    157,163,167,173,179,181,191,193,197,199,211,223,
    227,229,233,239,241,251,257,263,269,271,277,281,
    283,293,307,311,313,317,331,337,347,349,353,359,
    367,373,379,383,389,397,401,409,419,421,431,433,
    439,443,449,457,461,463,467,479,487,491,499,503,
    509,521,523,541,547,557,563,569,571,577,587,593,
    599,601,607,613,617,619,631,641,643,647,653,659,
    661,673,677,683,691,701,709,719,727,733,739,743,
    751,757,761,769,773,787,797,809,811,821,823,827,
    829,839,853,857,859,863,877,881,883,887,907,911,
    919,929,937,941,947,953,967,971,977,983,991,997]).

setup() ->
    setup(?test_numbers_per_sec).
setup(NumsPerSec) ->
    ?debugMsg("setup"),
    %ok = 
    application:load(random_prime_numbers),
    ok = 
    application:set_env([{random_prime_numbers,[
        {manual_start, true},
        {numbers_per_sec, NumsPerSec},
        {n, ?test_n},
        {rdb_list_key, ?test_list_key},
        {rdb_result_set_key, ?test_result_set_key},
        {pools, [
            {?test_pool, [
                {size, 40},
                {max_overflow, 10}
                ], [
                {host, "127.0.0.1"},
                {port, 6379},
                {database, 0},
                {password, ""}
                ]}
            ]}
        ]}]),

    application:ensure_all_started(random_prime_numbers),
    cleanup_test_keys(),
    ok.

cleanup() ->
    ?debugMsg("cleanup"),
    cleanup_test_keys(),
    application:stop(random_prime_numbers).

cleanup_test_keys() ->
    {ok, _} = poolboy:transaction(?test_pool, fun(W) ->
        eredis:q(W, ["DEL", ?test_list_key, ?test_result_set_key]) end).

lrange_list_key() ->
    lrange_list_key(?test_list_key).
lrange_list_key(ListKey) ->
    poolboy:transaction(?test_pool, fun(W) ->
        eredis:q(W, ["LRANGE", ListKey, 0, -1]) end).

smembers_set_key() ->
    smembers_set_key(?test_result_set_key).
smembers_set_key(SetKey) ->
    poolboy:transaction(?test_pool, fun(W) ->
        eredis:q(W, ["SMEMBERS", SetKey]) end).

rpn_lib_is_prime_test() ->
    PrimeNums = [X || X <-lists:seq(1, ?test_n), rpn_lib:is_prime(X)],
    ?assertEqual(?prime_numbers_0_1000, PrimeNums).

rpn_correctness_test() ->
    setup(),
    %% generate
    BinNums = gen_server:call(rpn_generator, {generate, 300}),
    {ok, ListBinContent} = lrange_list_key(),
    %% check list_key content
    ?assertEqual(300, length(BinNums)),
    ?assertEqual(BinNums, ListBinContent),

    %% fetch
    {NFetched, BinPrimeNums} = gen_server:call(rpn_filter, fetch),
    {ok, RSetBinContent} = smembers_set_key(),

    PrimeNums = [binary_to_integer(BinNum) || BinNum <- BinPrimeNums],
    RSetContent = [binary_to_integer(BinNum) || BinNum <- RSetBinContent],
    %% check result set_key content
    ?assertEqual(300, NFetched),
    ?assertEqual(lists:usort(PrimeNums), RSetContent),

    %% check all the nums fetched from result set_key are prime
    NotPrimeNums = [Num || Num <- PrimeNums, not rpn_lib:is_prime(Num)],
    ?assertEqual([], NotPrimeNums),
    cleanup().

rpn_accuracy_test() ->
    NumsPerSec = 10,
    setup(NumsPerSec), %% generate 10 nums/sec
    SleepStep = 1000 div NumsPerSec, %% ms between pushes
    FirstStepSleepError = SleepStep div 100 * 5, %% 5 perc error

    %% start filter fetch timer
    gen_server:cast(rpn_filter, start_timer),
    %% start generator step timer and items_pushed timer
    gen_server:cast(rpn_generator, start_timers),
    
    timer:sleep(SleepStep + FirstStepSleepError),
    assert_list_key_length(1),
    lists:foreach(
        fun(L) ->
            timer:sleep(SleepStep),
            assert_list_key_length(L)
        end, lists:seq(2, NumsPerSec - 1)), %% do not check the latest, it is already fetched
    cleanup().

assert_list_key_length(L) ->
    {ok, Res} = lrange_list_key(),
    ?assertEqual(L, length(Res)).