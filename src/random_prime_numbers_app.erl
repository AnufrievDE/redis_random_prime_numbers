%%%-------------------------------------------------------------------
%% @doc random_prime_numbers public API
%% @end
%%%-------------------------------------------------------------------

-module(random_prime_numbers_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case application:get_all_env() of
        [] ->
            {error, no_config};
        Args ->
            random_prime_numbers_sup:start_link(Args)
    end.

stop(_State) ->
    ok.