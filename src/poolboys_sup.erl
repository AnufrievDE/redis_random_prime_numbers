%%%-------------------------------------------------------------------
%% @doc supervisor for pools (gen_servers(pools))
%% @end
%%%-------------------------------------------------------------------

-module(poolboys_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Pools).

init(Pools) when is_list(Pools) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
            		{worker_module, eredis}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs, map)
    end, Pools),
    
    {ok, {SupFlags, PoolSpecs}}.