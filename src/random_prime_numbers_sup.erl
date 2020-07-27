%%%-------------------------------------------------------------------
%% @doc random_prime_numbers top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(random_prime_numbers_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Pools = proplists:get_value(pools, Args, []),
    {PoolName, _, _} = hd(Pools),

    PoolboysSupSpec = 
        #{id => poolboys_sup,
          start => {poolboys_sup, start_link, [Pools]},
          type => supervisor,
          restart => permanent,
          shutdown => infinity},

    ManualStart = proplists:get_value(manual_start, Args), %, false),
    NumsPerSec = proplists:get_value(numbers_per_sec, Args), %, 3000),
    ListKey = proplists:get_value(rdb_list_key, Args, "list_key"),

    ChildSpecs = [
        #{id => rpn_generator,
          start => {rpn_generator, start_link, [
            ManualStart,
            NumsPerSec,
            proplists:get_value(n, Args, 1000),
            PoolName, ListKey
          ]},
          type => worker,
          restart => permanent
        },
        #{id => rpn_filter,
          start => {rpn_filter, start_link, [
            ManualStart,
            NumsPerSec,
            PoolName,
            ListKey,
            proplists:get_value(rdb_result_set_key, Args, "result_set_key")
          ]},
          type => worker,
          restart => permanent
        }
    ],
    {ok, {SupFlags, [PoolboysSupSpec | ChildSpecs]}}.