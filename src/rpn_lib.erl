-module(rpn_lib).

-export([is_prime/1]).

is_prime(N) when N > 3 ->
    case (N rem 2 =:= 0) orelse (N rem 3 =:= 0) of
        true -> false;
        false -> is_prime(5, N)
    end;
is_prime(N) ->
    N > 1.

is_prime(I, N) ->
    is_prime(I, I*I, N).

is_prime(_I, II, N) when II > N ->
    true;
is_prime(I, _II, N) ->
    case (N rem I =:= 0) orelse (N rem (I + 2) =:= 0) of
        true -> false;
        false -> is_prime(I + 6, N)
    end.