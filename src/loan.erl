-module(loan).

-export([payment/3]).
-type frac() :: {integer(), integer(), integer()}.
-spec payment(frac(), frac(), frac()) -> frac().
%% @doc calculates the amortized schedule payment for a monthly loan
payment(Balance, Apr, Term) ->
    PI = frac:divd(Apr, {12, 0, 1}),
    T = frac:pow(
            frac:add({1, 0, 1}, PI), Term),
    N = frac:mul(PI, T),
    D = frac:sub(T, {1, 0, 1}),
    frac:mul(Balance, frac:divd(N, D)).

